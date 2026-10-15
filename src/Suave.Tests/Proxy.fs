module Suave.Tests.Proxy

open System
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
open Suave.Operators
open Suave.Proxy
open Suave.Testing

/// A minimal hand-rolled HTTP/1.1 upstream server that emits a response using
/// `Transfer-Encoding: chunked`. Written directly on top of TcpListener so we
/// control exact wire framing (multiple chunks + zero-terminator). Returns the
/// bound port and a CancellationTokenSource for shutdown.
let private startChunkedUpstream (body : string) : int * CancellationTokenSource =
  let cts = new CancellationTokenSource()
  let listener = new TcpListener(IPAddress.Loopback, 0)
  listener.Start()
  let port = (listener.LocalEndpoint :?> IPEndPoint).Port

  let handle (client : TcpClient) =
    task {
      use client = client
      use stream = client.GetStream()
      // Drain the request headers. A single read is enough for the small
      // GET request the proxy will send.
      let buf = Array.zeroCreate 4096
      let! _ = stream.ReadAsync(buf, 0, buf.Length)

      let mid = body.Length / 2
      let part1 = body.Substring(0, mid)
      let part2 = body.Substring(mid)
      let sb = StringBuilder()
      sb.Append("HTTP/1.1 200 OK\r\n") |> ignore
      sb.Append("Content-Type: text/plain\r\n") |> ignore
      sb.Append("Transfer-Encoding: chunked\r\n") |> ignore
      sb.Append("Connection: close\r\n") |> ignore
      sb.Append("\r\n") |> ignore
      sb.Append(part1.Length.ToString("X")) |> ignore
      sb.Append("\r\n") |> ignore
      sb.Append(part1) |> ignore
      sb.Append("\r\n") |> ignore
      sb.Append(part2.Length.ToString("X")) |> ignore
      sb.Append("\r\n") |> ignore
      sb.Append(part2) |> ignore
      sb.Append("\r\n") |> ignore
      sb.Append("0\r\n\r\n") |> ignore
      let bytes = Encoding.ASCII.GetBytes (sb.ToString())
      do! stream.WriteAsync(bytes, 0, bytes.Length)
      stream.Close()
    } :> Task

  let loop () : Task =
    task {
      try
        while not cts.IsCancellationRequested do
          let! client = listener.AcceptTcpClientAsync cts.Token
          // Fire-and-forget per-connection handler.
          let _ = Task.Run(fun () -> handle client)
          ()
      with _ -> ()
      listener.Stop()
    } :> Task

  Task.Run(System.Func<Task>(loop)) |> ignore
  port, cts

/// A minimal upstream server that echoes back what it received: `respond` is
/// handed the raw request text (headers + whatever of the body arrived) and
/// returns the complete raw response to write on the wire. Returns the bound
/// port and a CancellationTokenSource for shutdown.
let private startRawUpstream (respond : string -> string) : int * CancellationTokenSource =
  let cts = new CancellationTokenSource()
  let listener = new TcpListener(IPAddress.Loopback, 0)
  listener.Start()
  let port = (listener.LocalEndpoint :?> IPEndPoint).Port

  let handle (client : TcpClient) =
    task {
      use client = client
      use stream = client.GetStream()
      let buf = Array.zeroCreate 8192
      let! n = stream.ReadAsync(buf, 0, buf.Length)
      let request = Encoding.ASCII.GetString(buf, 0, n)
      let bytes = Encoding.ASCII.GetBytes (respond request)
      do! stream.WriteAsync(bytes, 0, bytes.Length)
      stream.Close()
    } :> Task

  let loop () : Task =
    task {
      try
        while not cts.IsCancellationRequested do
          let! client = listener.AcceptTcpClientAsync cts.Token
          let _ = Task.Run(fun () -> handle client)
          ()
      with _ -> ()
      listener.Stop()
    } :> Task

  Task.Run(System.Func<Task>(loop)) |> ignore
  port, cts

/// Build a raw HTTP response with a fixed `Content-Length` body.
let private fixedLengthResponse (statusLine : string) (body : string) =
  sprintf "HTTP/1.1 %s\r\nContent-Type: text/plain\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
    statusLine body.Length body

/// Bind a proxy built with `options` on an ephemeral port, pointing at `upstreamPort`.
let private startProxyWith (options : ProxyOptions) (upstreamPort : int) =
  let listener = new TcpListener(IPAddress.Loopback, 0)
  listener.Start()
  let proxyPort = (listener.LocalEndpoint :?> IPEndPoint).Port
  listener.Stop()

  let proxyCfg =
    { defaultConfig with
        bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" proxyPort ] }
  let proxyApp : WebPart = proxyWith options (Uri(sprintf "http://127.0.0.1:%d" upstreamPort))
  proxyPort, runWith proxyCfg proxyApp

/// Bind the proxy on an ephemeral port, pointing at `upstreamPort`.
let private startProxy (upstreamPort : int) =
  startProxyWith defaultProxyOptions upstreamPort

/// Read a full HTTP response from a raw TCP connection to `port`, driving the
/// request `GET path HTTP/1.1` + `Connection: close`. Returns the entire
/// wire-level response as an ASCII string so the test can assert on framing.
let private rawGet (port : int) (path : string) : string =
  use client = new TcpClient()
  client.Connect(IPAddress.Loopback, port)
  let s = client.GetStream()
  let req =
    sprintf "GET %s HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n" path
  let reqBytes = Encoding.ASCII.GetBytes req
  s.Write(reqBytes, 0, reqBytes.Length)
  use ms = new MemoryStream()
  let buf = Array.zeroCreate 4096
  let mutable reading = true
  while reading do
    let n = s.Read(buf, 0, buf.Length)
    if n <= 0 then reading <- false
    else ms.Write(buf, 0, n)
  Encoding.ASCII.GetString(ms.ToArray())

/// Same as `rawGet` but drives an arbitrary verb with a caller-chosen `Host`,
/// a request body and optional extra headers.
let private rawSendWithHost (port : int) (verb : string) (path : string) (host : string) (extraHeaders : (string * string) list) (body : string) : string =
  use client = new TcpClient()
  client.Connect(IPAddress.Loopback, port)
  let s = client.GetStream()
  let sb = StringBuilder()
  sb.Append(sprintf "%s %s HTTP/1.1\r\n" verb path) |> ignore
  sb.Append(sprintf "Host: %s\r\n" host) |> ignore
  sb.Append("Connection: close\r\n") |> ignore
  sb.Append("Content-Type: text/plain\r\n") |> ignore
  sb.Append(sprintf "Content-Length: %d\r\n" (Encoding.ASCII.GetByteCount body)) |> ignore
  for (k, v) in extraHeaders do
    sb.Append(sprintf "%s: %s\r\n" k v) |> ignore
  sb.Append("\r\n") |> ignore
  sb.Append(body) |> ignore
  let reqBytes = Encoding.ASCII.GetBytes (sb.ToString())
  s.Write(reqBytes, 0, reqBytes.Length)
  use ms = new MemoryStream()
  let buf = Array.zeroCreate 4096
  let mutable reading = true
  while reading do
    let n = s.Read(buf, 0, buf.Length)
    if n <= 0 then reading <- false
    else ms.Write(buf, 0, n)
  Encoding.ASCII.GetString(ms.ToArray())

let private rawSend port verb path extraHeaders body =
  rawSendWithHost port verb path "127.0.0.1" extraHeaders body

/// A self-signed, in-memory certificate for the TLS binding, mirroring the one
/// the HTTP/2 ALPN tests build (the shipped `suave.p12` is SHA-1/1024-bit and
/// modern OpenSSL rejects it).
let private loadTestCertificate () =
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

/// Bind the proxy on an ephemeral port over TLS, pointing at `upstreamPort`.
/// Returns the port plus a shutdown action.
let private startTlsProxy (upstreamPort : int) =
  let listener = new TcpListener(IPAddress.Loopback, 0)
  listener.Start()
  let proxyPort = (listener.LocalEndpoint :?> IPEndPoint).Port
  listener.Stop()

  let cts = new CancellationTokenSource()
  let proxyCfg =
    { defaultConfig with
        bindings = [ HttpBinding.create (HTTPS (loadTestCertificate ())) IPAddress.Loopback (uint16 proxyPort) ]
        cancellationToken = cts.Token }
  let ready, serverTask =
    Web.startWebServerAsync proxyCfg (proxy (Uri(sprintf "http://127.0.0.1:%d" upstreamPort)))
  ready |> Async.RunSynchronously |> ignore
  let shutdown () =
    cts.Cancel()
    try serverTask.Wait(TimeSpan.FromSeconds 5.0) |> ignore with _ -> ()
  proxyPort, shutdown

/// Drive a request over TLS, accepting the self-signed test certificate.
let private rawGetTls (port : int) (path : string) (host : string) : string =
  use client = new TcpClient()
  client.Connect(IPAddress.Loopback, port)
  use ssl =
    new SslStream(client.GetStream(), false, RemoteCertificateValidationCallback(fun _ _ _ _ -> true))
  let opts = SslClientAuthenticationOptions()
  opts.TargetHost <- "localhost"
  opts.EnabledSslProtocols <- SslProtocols.Tls12 ||| SslProtocols.Tls13
  ssl.AuthenticateAsClient opts
  let req = sprintf "GET %s HTTP/1.1\r\nHost: %s\r\nConnection: close\r\n\r\n" path host
  let reqBytes = Encoding.ASCII.GetBytes req
  ssl.Write(reqBytes, 0, reqBytes.Length)
  ssl.Flush()
  use ms = new MemoryStream()
  let buf = Array.zeroCreate 4096
  let mutable reading = true
  while reading do
    let n = ssl.Read(buf, 0, buf.Length)
    if n <= 0 then reading <- false
    else ms.Write(buf, 0, n)
  Encoding.ASCII.GetString(ms.ToArray())

/// An upstream that echoes the request headers it received back as the response
/// body, so the test can assert on what the proxy forwarded.
let private startHeaderEchoUpstream () =
  startRawUpstream (fun request ->
    let idx = request.IndexOf("\r\n\r\n")
    let headerBlock = if idx >= 0 then request.Substring(0, idx) else request
    fixedLengthResponse "200 OK" headerBlock)

[<Tests>]
let proxyTests (_ : SuaveConfig) =
  testList "Proxy" [
    // Regression test for https://github.com/SuaveIO/suave/issues/750:
    // when the upstream emits `Transfer-Encoding: chunked`, reading the
    // response content stream transparently de-chunks the body.
    // The proxy must therefore re-chunk on egress and forward exactly one
    // `Transfer-Encoding: chunked` header (and no `Content-Length`), otherwise
    // clients see errors such as
    // `curl: (56) Illegal or missing hexadecimal sequence in chunked-encoding`.
    testCase "forwards a chunked upstream response as a valid chunked response" <| fun _ ->
      let body = "abcdEFGH"
      let upstreamPort, upstreamCts = startChunkedUpstream body

      // Bind the proxy on an ephemeral port to avoid conflicts across tests.
      let listener = new TcpListener(IPAddress.Loopback, 0)
      listener.Start()
      let proxyPort = (listener.LocalEndpoint :?> IPEndPoint).Port
      listener.Stop()

      let proxyCfg =
        { defaultConfig with
            bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" proxyPort ] }
      let proxyApp : WebPart = proxy (Uri(sprintf "http://127.0.0.1:%d" upstreamPort))
      let ctx = runWith proxyCfg proxyApp
      try
        let response = rawGet proxyPort "/anything"

        // Header framing: we should see exactly one Transfer-Encoding: chunked
        // and no Content-Length header (the two are mutually exclusive per RFC 7230).
        let headerEnd = response.IndexOf("\r\n\r\n")
        Expect.isGreaterThan headerEnd 0 "should have a header/body boundary"
        let headerBlock = response.Substring(0, headerEnd)
        let bodyBlock = response.Substring(headerEnd + 4)

        let hasChunkedHeader =
          headerBlock.IndexOf("Transfer-Encoding: chunked", StringComparison.OrdinalIgnoreCase) >= 0
        Expect.isTrue hasChunkedHeader "proxy should forward Transfer-Encoding: chunked"

        let hasContentLength =
          headerBlock.IndexOf("Content-Length:", StringComparison.OrdinalIgnoreCase) >= 0
        Expect.isFalse hasContentLength
          "proxy must not emit Content-Length alongside Transfer-Encoding: chunked"

        // Body framing: single chunk of the full body followed by 0\r\n\r\n
        // terminator. `transferStreamChunked` coalesces the de-chunked bytes it
        // reads from the upstream stream, so we assert on the length prefix and
        // terminator rather than the exact original chunk boundaries.
        Expect.stringContains bodyBlock body "body payload should be present"
        Expect.stringContains bodyBlock "0\r\n\r\n" "chunked terminator should be present"
        Expect.stringContains bodyBlock
          (sprintf "%s\r\n%s\r\n" (body.Length.ToString("X")) body)
          "body should be framed as a single hex-length chunk"
      finally
        upstreamCts.Cancel()
        disposeContext ctx

    testCase "forwards a non-chunked response verbatim" <| fun _ ->
      let body = "hello world"
      let upstreamPort, upstreamCts =
        startRawUpstream (fun _ -> fixedLengthResponse "200 OK" body)
      let proxyPort, ctx = startProxy upstreamPort
      try
        let response = rawGet proxyPort "/plain"

        Expect.stringStarts response "HTTP/1.1 200 OK" "status line should be forwarded"
        let headerEnd = response.IndexOf("\r\n\r\n")
        Expect.isGreaterThan headerEnd 0 "should have a header/body boundary"
        let headerBlock = response.Substring(0, headerEnd)
        let bodyBlock = response.Substring(headerEnd + 4)

        Expect.isFalse
          (headerBlock.IndexOf("Transfer-Encoding", StringComparison.OrdinalIgnoreCase) >= 0)
          "a non-chunked response must not gain a Transfer-Encoding header"

        let contentLengths =
          headerBlock.Split([| "\r\n" |], StringSplitOptions.None)
          |> Array.filter (fun l -> l.StartsWith("Content-Length:", StringComparison.OrdinalIgnoreCase))
        Expect.equal contentLengths.Length 1 "exactly one Content-Length header"
        Expect.stringContains contentLengths.[0] (string body.Length) "Content-Length should match the body"
        Expect.equal bodyBlock body "body should be forwarded verbatim"
      finally
        upstreamCts.Cancel()
        disposeContext ctx

    // With `HttpClient` a non-2xx upstream response is returned normally rather
    // than thrown as a `WebException`, so make sure it is still forwarded.
    testCase "forwards a non-200 upstream status and body" <| fun _ ->
      let body = "nope"
      let upstreamPort, upstreamCts =
        startRawUpstream (fun _ -> fixedLengthResponse "404 Not Found" body)
      let proxyPort, ctx = startProxy upstreamPort
      try
        let response = rawGet proxyPort "/missing"

        Expect.stringStarts response "HTTP/1.1 404 Not Found" "status line should be forwarded"
        Expect.stringContains response body "error body should be forwarded"
      finally
        upstreamCts.Cancel()
        disposeContext ctx

    testCase "forwards a 500 upstream status and body" <| fun _ ->
      let body = "boom"
      let upstreamPort, upstreamCts =
        startRawUpstream (fun _ -> fixedLengthResponse "500 Internal Server Error" body)
      let proxyPort, ctx = startProxy upstreamPort
      try
        let response = rawGet proxyPort "/broken"

        Expect.stringStarts response "HTTP/1.1 500 Internal Server Error" "status line should be forwarded"
        Expect.stringContains response body "error body should be forwarded"
      finally
        upstreamCts.Cancel()
        disposeContext ctx

    testCase "forwards the request body of a POST" <| fun _ ->
      let sent = "name=suave&kind=proxy"
      let upstreamPort, upstreamCts =
        startRawUpstream (fun request ->
          let idx = request.IndexOf("\r\n\r\n")
          let received = if idx >= 0 then request.Substring(idx + 4) else ""
          fixedLengthResponse "200 OK" received)
      let proxyPort, ctx = startProxy upstreamPort
      try
        let response = rawSend proxyPort "POST" "/echo" [] sent

        Expect.stringStarts response "HTTP/1.1 200 OK" "status line should be forwarded"
        Expect.stringContains response sent "the upstream should have received the request body"
      finally
        upstreamCts.Cancel()
        disposeContext ctx

    testCase "forwards the request body of a PUT" <| fun _ ->
      let sent = "put-payload"
      let upstreamPort, upstreamCts =
        startRawUpstream (fun request ->
          let idx = request.IndexOf("\r\n\r\n")
          let received = if idx >= 0 then request.Substring(idx + 4) else ""
          fixedLengthResponse "200 OK" received)
      let proxyPort, ctx = startProxy upstreamPort
      try
        let response = rawSend proxyPort "PUT" "/echo" [] sent

        Expect.stringStarts response "HTTP/1.1 200 OK" "status line should be forwarded"
        Expect.stringContains response sent "the upstream should have received the request body"
      finally
        upstreamCts.Cancel()
        disposeContext ctx

    testCase "adds X-Forwarded-For and forwards request headers" <| fun _ ->
      let upstreamPort, upstreamCts = startHeaderEchoUpstream ()
      let proxyPort, ctx = startProxy upstreamPort
      try
        let response = rawSend proxyPort "POST" "/headers" [ "User-Agent", "suave-test-agent" ] "x"

        Expect.stringContains response "X-Forwarded-For:"
          "the proxy should add an X-Forwarded-For header"
        Expect.stringContains response "suave-test-agent"
          "the proxy should forward the User-Agent header"
        Expect.stringContains response "Content-Type: text/plain"
          "the proxy should forward the request Content-Type on the content headers"
      finally
        upstreamCts.Cancel()
        disposeContext ctx

    testCase "sets X-Forwarded-For to the client address" <| fun _ ->
      let upstreamPort, upstreamCts = startHeaderEchoUpstream ()
      let proxyPort, ctx = startProxy upstreamPort
      try
        let response = rawGet proxyPort "/headers"

        Expect.stringContains response "X-Forwarded-For: 127.0.0.1"
          "X-Forwarded-For should name the connecting client, not the proxy's own host"
      finally
        upstreamCts.Cancel()
        disposeContext ctx

    // A client can put anything in these headers, so they are only believed
    // when the connection comes from a configured trusted proxy.
    testCase "replaces X-Forwarded-* headers sent by an untrusted client" <| fun _ ->
      let upstreamPort, upstreamCts = startHeaderEchoUpstream ()
      let proxyPort, ctx = startProxy upstreamPort
      try
        let response =
          rawSend proxyPort "POST" "/headers"
            [ "X-Forwarded-For", "203.0.113.7"; "X-Forwarded-Host", "edge.example"; "X-Forwarded-Proto", "https"
              "Forwarded", "for=203.0.113.7;proto=https"; "X-Real-IP", "203.0.113.7" ] "x"

        Expect.stringContains response "X-Forwarded-For: 127.0.0.1\r\n" "X-Forwarded-For names only the connecting client"
        Expect.stringContains response "X-Forwarded-Host: 127.0.0.1\r\n" "X-Forwarded-Host is the Host the proxy received"
        Expect.stringContains response "X-Forwarded-Proto: http\r\n" "X-Forwarded-Proto is the scheme the proxy received"
        Expect.isFalse (response.Contains "203.0.113.7") "the client's address claims are dropped"
        Expect.isFalse (response.Contains "edge.example") "the client's host claim is dropped"
      finally
        upstreamCts.Cancel()
        disposeContext ctx

    testCase "extends X-Forwarded-* headers from a trusted proxy" <| fun _ ->
      let upstreamPort, upstreamCts = startHeaderEchoUpstream ()
      let options = { defaultProxyOptions with trustedProxies = [ IPAddress.Loopback ] }
      let proxyPort, ctx = startProxyWith options upstreamPort
      try
        let response =
          rawSend proxyPort "POST" "/headers"
            [ "X-Forwarded-For", "203.0.113.7"; "X-Forwarded-Host", "edge.example"; "X-Forwarded-Proto", "https" ] "x"

        Expect.stringContains response "X-Forwarded-For: 203.0.113.7, 127.0.0.1\r\n"
          "the client we received the request from should be appended to the chain"
        // Suave lowercases the names of the headers it received.
        Expect.stringContains response "x-forwarded-host: edge.example\r\n"
          "a trusted proxy's X-Forwarded-Host should not be overwritten"
        Expect.stringContains response "x-forwarded-proto: https\r\n"
          "a trusted proxy's X-Forwarded-Proto should not be overwritten"
      finally
        upstreamCts.Cancel()
        disposeContext ctx

    // The scheme is derived from the matched binding, so a TLS binding is the
    // only way to exercise the `https` branch.
    testCase "derives X-Forwarded-Proto https from a TLS binding" <| fun _ ->
      let upstreamPort, upstreamCts = startHeaderEchoUpstream ()
      let proxyPort, shutdownProxy = startTlsProxy upstreamPort
      try
        let response = rawGetTls proxyPort "/headers" "client.example"

        Expect.stringStarts response "HTTP/1.1 200 OK" "the request should be proxied over TLS"
        Expect.stringContains response "X-Forwarded-Proto: https"
          "a secure binding should be reported as https"
        Expect.stringContains response "X-Forwarded-Host: client.example"
          "the client's Host should still be preserved in X-Forwarded-Host"
      finally
        upstreamCts.Cancel()
        shutdownProxy ()
  ]

// ---- forwarding through a Suave upstream that reports what it received ----

/// A Suave server on an ephemeral loopback port; stopped on Dispose.
type private EphemeralServer(app : WebPart) =
  let stopping = new CancellationTokenSource()
  let config =
    { defaultConfig with
        bindings = [ HttpBinding.create HTTP IPAddress.Loopback 0us ]
        hideStartupMessage = true
        cancellationToken = stopping.Token }
  let listening, _ = startWebServerAsync config app
  let port = (listening |> Async.RunSynchronously).[0].Value.binding.port
  member _.Uri = Uri(sprintf "http://127.0.0.1:%d" port)
  member _.Url (path : string) = sprintf "http://127.0.0.1:%d%s" port path
  interface IDisposable with member _.Dispose() = stopping.Cancel()

let private withStatus (code : int) (reason : string) : WebPart =
  fun ctx -> succeed { ctx with response = { ctx.response with status = { code = code; reason = reason } } }

let private echo : WebPart =
  request (fun r ->
    let headers = r.headers |> Seq.map (fun (k, v) -> k + ": " + v) |> String.concat "\n"
    Successful.OK (sprintf "%s %s?%s\n%s\n\n%s" r.rawMethod r.rawPath r.rawQuery headers (Encoding.UTF8.GetString r.rawForm)))

let private big = Array.init 1_000_000 (fun i -> byte (i % 251))

let private upstreamApp =
  let path = Filters.path
  choose [
    path "/teapot" >=> Successful.OK "short and stout" >=> withStatus 418 "I'm a teapot"
    path "/multi" >=> Successful.OK "multi" >=> withStatus 207 "Multi-Status"
    path "/big" >=> Successful.ok big
    path "/cookies" >=> Cookie.setCookie (HttpCookie.createKV "a" "1") >=> Cookie.setCookie (HttpCookie.createKV "b" "2") >=> Successful.OK "cookies"
    path "/slow" >=> (fun ctx -> async {
      do! Async.Sleep 2000
      return! Successful.OK "late" ctx })
    path "/no-content" >=> Successful.NO_CONTENT
    Filters.pathStarts "/api/" >=> echo
    echo
  ]

let private httpClient = new Net.Http.HttpClient(new Net.Http.HttpClientHandler(AllowAutoRedirect = false, UseCookies = false))

let private send (server : EphemeralServer) (meth : string) (path : string) (headers : (string * string) list) (body : string option) =
  use message = new Net.Http.HttpRequestMessage(Net.Http.HttpMethod meth, server.Url path)
  for (k, v) in headers do message.Headers.TryAddWithoutValidation(k, v) |> ignore
  body |> Option.iter (fun b -> message.Content <- new Net.Http.StringContent(b, Encoding.UTF8, "application/json"))
  let response = httpClient.Send(message)
  response, response.Content.ReadAsByteArrayAsync().Result

let private text (bytes : byte[]) = Encoding.UTF8.GetString bytes

[<Tests>]
let proxyForwardingTests (_ : SuaveConfig) =
  let errors = Collections.Concurrent.ConcurrentQueue<string>()
  let upstream = lazy (new EphemeralServer(upstreamApp))
  let front =
    lazy (
      let options =
        { defaultProxyOptions with
            timeout = TimeSpan.FromMilliseconds 500.
            onError = fun ctx ex -> errors.Enqueue(ctx.request.path + " " + ex.GetType().Name) }
      new EphemeralServer(proxyWith options upstream.Value.Uri))
  let sendFront meth path headers body = send front.Value meth path headers body

  testSequenced <| testList "Proxy forwarding" [
    testCase "the raw path and query string arrive unchanged" <| fun _ ->
      let _, body = sendFront "GET" "/a/b%2Fc?x=1&y=two%20words" [] None
      Expect.stringStarts (text body) "GET /a/b%2Fc?x=1&y=two%20words\n" "path and query"

    testCase "the upstream's base path prefixes the forwarded path" <| fun _ ->
      use based = new EphemeralServer(proxy (Uri(upstream.Value.Url "/api/")))
      let _, body = send based "GET" "/users?id=1" [] None
      Expect.stringStarts (text body) "GET /api/users?id=1\n" "base path"

    testCase "dot segments cannot escape the base path" <| fun _ ->
      use based = new EphemeralServer(proxy (Uri(upstream.Value.Url "/api")))
      use client = new TcpClient()
      client.Connect(IPAddress.Loopback, based.Uri.Port)
      let s = client.GetStream()
      let req = Encoding.ASCII.GetBytes "GET /%2E%2E/admin HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
      s.Write(req, 0, req.Length)
      use reader = new StreamReader(s)
      Expect.stringStarts (reader.ReadToEnd()) "HTTP/1.1 400" "rejected"

    testCase "end-to-end headers are forwarded, hop-by-hop ones are not" <| fun _ ->
      let _, body =
        sendFront "GET" "/h" [ "x-api-key", "secret"; "Cookie", "sid=abc"; "Authorization", "Bearer t"
                               "Connection", "x-private"; "x-private", "hop"; "Proxy-Authorization", "Basic xyz" ] None
      let received = text body
      for expected in [ "x-api-key: secret"; "cookie: sid=abc"; "authorization: Bearer t" ] do
        Expect.stringContains received expected expected
      for unexpected in [ "x-private"; "proxy-authorization" ] do
        Expect.isFalse (received.Contains unexpected) unexpected

    testCase "bodies are forwarded for every method that has one" <| fun _ ->
      for meth in [ "POST"; "PUT"; "PATCH"; "DELETE" ] do
        let _, body = sendFront meth "/b" [] (Some """{"a":1}""")
        Expect.stringStarts (text body) (meth + " /b?") meth
        Expect.stringContains (text body) "content-type: application/json; charset=utf-8" (meth + " content type")
        Expect.stringEnds (text body) """{"a":1}""" (meth + " body")

    testCase "any status code passes through with its reason phrase" <| fun _ ->
      let teapot, body = sendFront "GET" "/teapot" [] None
      Expect.equal (int teapot.StatusCode) 418 "418"
      Expect.equal teapot.ReasonPhrase "I'm a teapot" "reason"
      Expect.equal (text body) "short and stout" "body"
      let multi, _ = sendFront "GET" "/multi" [] None
      Expect.equal (int multi.StatusCode) 207 "207"
      let empty, _ = sendFront "GET" "/no-content" [] None
      Expect.equal (int empty.StatusCode) 204 "204"

    testCase "a large body streams through intact" <| fun _ ->
      let response, body = sendFront "GET" "/big" [] None
      Expect.equal response.Content.Headers.ContentLength (Nullable 1_000_000L) "length"
      Expect.equal body big "same bytes"

    testCase "every Set-Cookie survives, and Date and Server aren't doubled" <| fun _ ->
      let response, _ = sendFront "GET" "/cookies" [] None
      Expect.equal (response.Headers.GetValues "Set-Cookie" |> Seq.length) 2 "two cookies"
      // NonValidated: .NET would otherwise split "Suave (https://suave.io)" into two product tokens.
      Expect.equal (response.Headers.NonValidated.["Date"].Count) 1 "one Date"
      Expect.equal (response.Headers.NonValidated.["Server"].Count) 1 "one Server"

    testCase "HEAD gets headers and no body" <| fun _ ->
      let response, body = sendFront "HEAD" "/big" [] None
      Expect.equal (int response.StatusCode) 200 "status"
      Expect.equal body.Length 0 "no body"

    testCase "a slow upstream gets a 504, and the error is reported" <| fun _ ->
      let response, _ = sendFront "GET" "/slow" [] None
      Expect.equal (int response.StatusCode) 504 "timeout"
      Expect.exists errors (fun e -> e.StartsWith "/slow") "onError called"

    testCase "an unreachable upstream gets a 502" <| fun _ ->
      let closed = new TcpListener(IPAddress.Loopback, 0)
      closed.Start()
      let port = (closed.LocalEndpoint :?> IPEndPoint).Port
      closed.Stop()
      use server = new EphemeralServer(proxy (Uri(sprintf "http://127.0.0.1:%d" port)))
      let response = httpClient.GetAsync(server.Url "/x").Result
      Expect.equal (int response.StatusCode) 502 "bad gateway"

    testCase "invalid options fail at creation, not on the first request" <| fun _ ->
      Expect.throws (fun () -> proxy (Uri "ftp://example.com") |> ignore) "ftp"
      Expect.throws (fun () -> proxyWith { defaultProxyOptions with timeout = TimeSpan.Zero } (Uri "http://x") |> ignore) "timeout"

    // Suave parses multipart bodies before any WebPart runs, so there is
    // nothing left to forward; this documents that limitation.
    testCase "multipart uploads are not forwarded" <| fun _ ->
      use form = new Net.Http.MultipartFormDataContent()
      form.Add(new Net.Http.StringContent("hello"), "file", "hello.txt")
      let response = httpClient.PostAsync(front.Value.Url "/upload", form).Result
      let received = response.Content.ReadAsStringAsync().Result
      Expect.isFalse (received.Contains "hello") "the file contents are not available to forward"
  ]