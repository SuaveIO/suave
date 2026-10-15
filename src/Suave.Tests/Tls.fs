module Suave.Tests.Tls

open System
open System.Diagnostics
open System.IO
open System.Net
open System.Net.Security
open System.Net.Sockets
open System.Security.Authentication
open System.Security.Cryptography
open System.Security.Cryptography.X509Certificates
open System.Text
open System.Threading
open System.Threading.Tasks

open Expecto

open Suave

/// A self-signed, in-memory certificate for the TLS binding (the shipped
/// `suave.p12` is SHA-1/1024-bit and modern OpenSSL rejects it).
let private createTestCertificate () =
  use rsa = RSA.Create 2048
  let req =
    CertificateRequest("CN=localhost", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1)
  let sanBuilder = SubjectAlternativeNameBuilder()
  sanBuilder.AddIpAddress IPAddress.Loopback
  sanBuilder.AddDnsName "localhost"
  req.CertificateExtensions.Add(sanBuilder.Build())
  let cert =
    req.CreateSelfSigned(DateTimeOffset.UtcNow.AddMinutes -5.0, DateTimeOffset.UtcNow.AddHours 1.0)
  // Round-trip through PKCS#12 so the private key handle survives on every OS.
  X509CertificateLoader.LoadPkcs12(cert.Export X509ContentType.Pkcs12, (null : string))

let private freePort () =
  let listener = new TcpListener(IPAddress.Loopback, 0)
  listener.Start()
  let port = (listener.LocalEndpoint :?> IPEndPoint).Port
  listener.Stop()
  port

/// Run `body` against an HTTPS server on an ephemeral loopback port.
let private withHttpsServer (handshakeTimeout : TimeSpan) (body : int -> unit) =
  let port = freePort ()
  use cts = new CancellationTokenSource()
  let config =
    { defaultConfig with
        bindings = [ HttpBinding.create (HTTPS (createTestCertificate ())) IPAddress.Loopback (uint16 port) ]
        cancellationToken = cts.Token }
  // A process-wide setting; the suite runs sequenced, so restore it afterwards.
  let previousTimeout = Tcp.tlsHandshakeTimeout
  Tcp.tlsHandshakeTimeout <- handshakeTimeout
  let ready, serverTask = Web.startWebServerAsync config (Successful.OK "hello")
  ready |> Async.RunSynchronously |> ignore
  try
    body port
  finally
    cts.Cancel()
    try serverTask.Wait(TimeSpan.FromSeconds 5.0) |> ignore with _ -> ()
    Tcp.tlsHandshakeTimeout <- previousTimeout

/// Make one HTTPS request, accepting the self-signed certificate, and return
/// the raw response. Every socket operation is bounded so a stuck server fails
/// the test instead of hanging the run.
let private getTls (port : int) : string =
  use client = new TcpClient()
  client.ReceiveTimeout <- 10000
  client.SendTimeout <- 10000
  client.Connect(IPAddress.Loopback, port)
  use ssl =
    new SslStream(client.GetStream(), false, RemoteCertificateValidationCallback(fun _ _ _ _ -> true))
  let opts = SslClientAuthenticationOptions()
  opts.TargetHost <- "localhost"
  opts.EnabledSslProtocols <- SslProtocols.Tls12 ||| SslProtocols.Tls13
  ssl.AuthenticateAsClient opts
  let req = Encoding.ASCII.GetBytes "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
  ssl.Write(req, 0, req.Length)
  ssl.Flush()
  use ms = new MemoryStream()
  let buf = Array.zeroCreate 4096
  let mutable reading = true
  while reading do
    let n = ssl.Read(buf, 0, buf.Length)
    if n <= 0 then reading <- false
    else ms.Write(buf, 0, n)
  Encoding.ASCII.GetString(ms.ToArray())

/// `getTls`, failing if it does not finish within `limit`.
let private getTlsWithin (limit : TimeSpan) (port : int) =
  let request = Task.Run(fun () -> getTls port)
  Expect.isTrue (request.Wait limit) (sprintf "the HTTPS request did not complete within %O" limit)
  request.Result

[<Tests>]
let tlsTests (_ : SuaveConfig) =
  testList "TLS" [
    // An idle connection used to stall the TLS handshake on the accept loop,
    // so with one acceptor the whole HTTPS binding stopped accepting.
    testCase "a client that never starts the TLS handshake does not block other clients" <| fun _ ->
      withHttpsServer (TimeSpan.FromSeconds 30.0) <| fun port ->
        use idle = new TcpClient()
        idle.Connect(IPAddress.Loopback, port)
        // Let the server accept the idle connection first.
        Thread.Sleep 200
        let response = getTlsWithin (TimeSpan.FromSeconds 5.0) port
        Expect.stringStarts response "HTTP/1.1 200" "the second client is served"
        Expect.stringContains response "hello" "the second client gets the body"

    testCase "a stalled TLS handshake is closed after Tcp.tlsHandshakeTimeout" <| fun _ ->
      withHttpsServer (TimeSpan.FromMilliseconds 500.0) <| fun port ->
        use idle = new TcpClient()
        idle.ReceiveTimeout <- 10000
        idle.Connect(IPAddress.Loopback, port)
        let watch = Stopwatch.StartNew()
        let closed =
          try idle.GetStream().Read(Array.zeroCreate 16, 0, 16) = 0
          with :? IOException -> true
        watch.Stop()
        Expect.isTrue closed "the server closes the connection"
        Expect.isLessThan watch.Elapsed (TimeSpan.FromSeconds 5.0) "the connection is closed after the handshake timeout, not the read timeout"
        // The connection went back to the pool and the binding still serves.
        let response = getTlsWithin (TimeSpan.FromSeconds 5.0) port
        Expect.stringStarts response "HTTP/1.1 200" "the binding still serves requests"

    testCase "a failed TLS handshake closes the socket and recycles the connection" <| fun _ ->
      withHttpsServer (TimeSpan.FromSeconds 10.0) <| fun port ->
        // Clients that send plain HTTP instead of a ClientHello fail the
        // handshake at once; the server must close each socket (it used to
        // leave it open) and return its connection to the pool.
        let watch = Stopwatch.StartNew()
        for _ in 1 .. 20 do
          use bad = new TcpClient()
          bad.ReceiveTimeout <- 5000
          bad.Connect(IPAddress.Loopback, port)
          let junk = Encoding.ASCII.GetBytes "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
          bad.GetStream().Write(junk, 0, junk.Length)
          try
            let buf = Array.zeroCreate 4096
            while bad.GetStream().Read(buf, 0, buf.Length) > 0 do ()
          with :? IOException -> ()
        watch.Stop()
        Expect.isLessThan watch.Elapsed (TimeSpan.FromSeconds 5.0) "the server closes sockets whose handshake failed"
        for _ in 1 .. 5 do
          let response = getTlsWithin (TimeSpan.FromSeconds 5.0) port
          Expect.stringStarts response "HTTP/1.1 200" "the binding still serves requests"
  ]