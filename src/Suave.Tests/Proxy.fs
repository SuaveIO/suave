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

/// Bind the proxy on an ephemeral port, pointing at `upstreamPort`.
let private startProxy (upstreamPort : int) =
  let listener = new TcpListener(IPAddress.Loopback, 0)
  listener.Start()
  let proxyPort = (listener.LocalEndpoint :?> IPEndPoint).Port
  listener.Stop()

  let proxyCfg =
    { defaultConfig with
        bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" proxyPort ] }
  let proxyApp : WebPart = proxy (Uri(sprintf "http://127.0.0.1:%d" upstreamPort))
  proxyPort, runWith proxyCfg proxyApp

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

/// Same as `rawGet` but drives an arbitrary verb with a request body and
/// optional extra headers.
let private rawSend (port : int) (verb : string) (path : string) (extraHeaders : (string * string) list) (body : string) : string =
  use client = new TcpClient()
  client.Connect(IPAddress.Loopback, port)
  let s = client.GetStream()
  let sb = StringBuilder()
  sb.Append(sprintf "%s %s HTTP/1.1\r\n" verb path) |> ignore
  sb.Append("Host: 127.0.0.1\r\n") |> ignore
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
      let upstreamPort, upstreamCts =
        startRawUpstream (fun request ->
          let idx = request.IndexOf("\r\n\r\n")
          let headerBlock = if idx >= 0 then request.Substring(0, idx) else request
          fixedLengthResponse "200 OK" headerBlock)
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
  ]
