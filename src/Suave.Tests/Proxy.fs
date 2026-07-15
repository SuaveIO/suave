module Suave.Tests.Proxy

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Text
open System.Threading
open System.Threading.Tasks

open Expecto

open Suave
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

[<Tests>]
let proxyTests (_ : SuaveConfig) =
  testList "Proxy" [
    // Regression test for https://github.com/SuaveIO/suave/issues/750:
    // when the upstream emits `Transfer-Encoding: chunked`,
    // `HttpWebResponse.GetResponseStream()` transparently de-chunks the body.
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
  ]
