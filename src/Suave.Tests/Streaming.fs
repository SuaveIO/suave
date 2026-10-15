module Suave.Tests.Streaming

open Expecto

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Text
open System.Threading
open System.Threading.Tasks

open Suave
open Suave.Operators
open Suave.Sockets
open Suave.Testing
open Suave.EventSource

let private endpoint (cfg : SuaveConfig) =
  let binding = SuaveConfig.firstBinding cfg
  string binding.socketBinding.ip, int binding.socketBinding.port

let private sendGet (client : TcpClient) (path : string) =
  let stream = client.GetStream()
  let request = Encoding.ASCII.GetBytes(sprintf "GET %s HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" path)
  stream.Write(request, 0, request.Length)
  stream

let private indexOf (pattern : byte[]) (bytes : byte[]) =
  let rec go i =
    if i + pattern.Length > bytes.Length then -1
    elif Seq.forall (fun j -> bytes.[i + j] = pattern.[j]) [0 .. pattern.Length - 1] then i
    else go (i + 1)
  go 0

/// Send a GET with `Connection: close` and read the raw response until the
/// server closes; returns the response head and the raw body bytes.
let private rawGet (cfg : SuaveConfig) (path : string) =
  let ip, port = endpoint cfg
  use client = new TcpClient(ip, port)
  client.ReceiveTimeout <- 5000
  let stream = sendGet client path
  use ms = new MemoryStream()
  stream.CopyTo ms
  let bytes = ms.ToArray()
  let split = indexOf "\r\n\r\n"B bytes
  if split < 0 then failwithf "no response head in %A" (Encoding.ASCII.GetString bytes)
  Encoding.ASCII.GetString(bytes, 0, split), bytes.[split + 4 ..]

let private headerValues (name : string) (head : string) =
  head.Split("\r\n")
  |> Array.choose (fun line ->
    match line.IndexOf ':' with
    | -1 -> None
    | i when String.Equals(line.Substring(0, i).Trim(), name, StringComparison.OrdinalIgnoreCase) ->
      Some (line.Substring(i + 1).Trim())
    | _ -> None)
  |> List.ofArray

/// The events in an SSE body, after the handshake's padding comment and retry
let private sseEvents (body : byte[]) =
  let text = Encoding.UTF8.GetString body
  let marker = "retry: 2000\n"
  text.Substring(text.IndexOf marker + marker.Length)

/// Connect, read the response until `marker`, then reset the connection; returns
/// once the handler signalled that its write loop stopped, or false after a timeout.
let private disconnectAndWait (cfg : SuaveConfig) (path : string) (marker : byte[]) (stopped : ManualResetEventSlim) =
  let ip, port = endpoint cfg
  let client = new TcpClient(ip, port)
  client.ReceiveTimeout <- 5000
  let stream = sendGet client path
  // Wait for everything written before the handler's loop starts
  use received = new MemoryStream()
  let buf = Array.zeroCreate 4096
  while indexOf marker (received.ToArray()) < 0 do
    let n = stream.Read(buf, 0, buf.Length)
    if n = 0 then failwith "connection closed before the handler started"
    received.Write(buf, 0, n)
  // RST rather than FIN, so the server's next writes fail promptly
  client.LingerState <- LingerOption(true, 0)
  client.Close()
  stopped.Wait(TimeSpan.FromSeconds 10.)

[<Tests>]
let eventSourceTests (cfg : SuaveConfig) =
  let runWithConfig = runWith cfg

  testList "EventSource" [
    testCase "line breaks in data become separate data lines" <| fun _ ->
      let app = handShake (fun out -> send out (Message.create "1" "hello\n\nevent: admin\ndata: injected\r\nx\ry"))
      let ctx = runWithConfig app
      withContext (fun _ ->
        let _, body = rawGet cfg "/"
        Expect.equal (sseEvents body)
          "id: 1\ndata: hello\ndata: \ndata: event: admin\ndata: data: injected\ndata: x\ndata: y\n\n"
          "data must not be able to end the event or inject fields") ctx

    testCase "CR and LF are removed from id, event and comment" <| fun _ ->
      let app =
        handShake (fun out -> task {
          do! comment out "c\r\nevent: admin"
          do! send out (Message.createType "1\nretry: 1" "d" "msg\r\nid: 2") })
      let ctx = runWithConfig app
      withContext (fun _ ->
        let _, body = rawGet cfg "/"
        Expect.equal (sseEvents body)
          ": cevent: admin\nid: 1retry: 1\nevent: msgid: 2\ndata: d\n\n"
          "field values must stay on one line") ctx

    testCase "send raises SocketWriteException once the client is gone" <| fun _ ->
      use stopped = new ManualResetEventSlim(false)
      let failure : exn ref = ref null
      let app =
        handShake (fun out -> task {
          try
            try
              let mutable i = 0
              while i < 1000 do
                do! send out (Message.create (string i) "tick")
                do! Task.Delay 20
                i <- i + 1
            with ex -> failure.Value <- ex
          finally
            stopped.Set() })
      let ctx = runWithConfig app
      withContext (fun _ ->
        Expect.isTrue (disconnectAndWait cfg "/" "retry: 2000\n"B stopped) "the send loop should stop after the client disconnects"
        Expect.isTrue (failure.Value :? SocketWriteException) (sprintf "expected SocketWriteException, got %A" failure.Value)) ctx

    testCase "trySend returns Error once the client is gone" <| fun _ ->
      use stopped = new ManualResetEventSlim(false)
      let result = ref (Ok ())
      let failure : exn ref = ref null
      let app =
        handShake (fun out -> task {
          try
            try
              let mutable i = 0
              while i < 1000 do
                match! trySend out (Message.create (string i) "tick") with
                | Ok () ->
                  do! Task.Delay 20
                  i <- i + 1
                | Error e ->
                  result.Value <- Result.Error e
                  i <- Int32.MaxValue
            with ex -> failure.Value <- ex
          finally
            stopped.Set() })
      let ctx = runWithConfig app
      withContext (fun _ ->
        Expect.isTrue (disconnectAndWait cfg "/" "retry: 2000\n"B stopped) "the send loop should stop after the client disconnects"
        Expect.isNull failure.Value "no exception expected"
        Expect.isError result.Value "trySend should report the failed write") ctx

    testCase "handShake keeps existing headers and sends no CORS header by default" <| fun _ ->
      let app =
        Writers.setHeader "X-Custom" "kept"
        >=> Writers.setHeader "Cache-Control" "no-store"
        >=> handShake (fun _ -> Task.FromResult ())
      let ctx = runWithConfig app
      withContext (fun _ ->
        let head, _ = rawGet cfg "/"
        Expect.stringStarts head "HTTP/1.1 200" "status"
        Expect.equal (headerValues "X-Custom" head) [ "kept" ] "custom header kept"
        Expect.equal (headerValues "Cache-Control" head) [ "no-store" ] "existing Cache-Control kept"
        Expect.equal (headerValues "Content-Type" head) [ "text/event-stream; charset=utf-8" ] "content type"
        Expect.equal (headerValues "Access-Control-Allow-Origin" head) [] "no CORS header unless asked for") ctx
  ]

[<Tests>]
let chunkedWriteTests (cfg : SuaveConfig) =
  let runWithConfig = runWith cfg

  testList "chunked writes" [
    testCase "writeChunk writes the raw bytes with their length" <| fun _ ->
      let app =
        TransferEncoding.chunked (fun conn -> task {
          do! conn.writeChunk [| 0xFFuy; 0xFEuy; 0x00uy |]
          do! conn.writeLastChunk () })
      let ctx = runWithConfig app
      withContext (fun _ ->
        let head, body = rawGet cfg "/"
        Expect.equal (headerValues "Transfer-Encoding" head) [ "chunked" ] "chunked"
        Expect.equal body (Array.concat [ "3\r\n"B; [| 0xFFuy; 0xFEuy; 0x00uy |]; "\r\n0\r\n\r\n"B ]) "raw chunk") ctx

    testCase "writeChunk handles chunks larger than the line buffer" <| fun _ ->
      let data = Array.init 100000 byte
      let app =
        TransferEncoding.chunked (fun conn -> task {
          do! conn.writeChunk data
          do! conn.writeLastChunk () })
      let ctx = runWithConfig app
      let res = reqBytes HttpMethod.GET "/" None ctx
      Expect.equal res data "large chunk round-trips"

    testCase "tryWriteChunk returns Error once the client is gone" <| fun _ ->
      use stopped = new ManualResetEventSlim(false)
      let result = ref (Ok ())
      let failure : exn ref = ref null
      let app =
        TransferEncoding.chunked (fun conn -> task {
          try
            try
              let mutable i = 0
              while i < 1000 do
                match! conn.tryWriteChunk "tick"B with
                | Ok () ->
                  do! Task.Delay 20
                  i <- i + 1
                | Error e ->
                  result.Value <- Result.Error e
                  i <- Int32.MaxValue
            with ex -> failure.Value <- ex
          finally
            stopped.Set() })
      let ctx = runWithConfig app
      withContext (fun _ ->
        Expect.isTrue (disconnectAndWait cfg "/" "\r\n\r\n"B stopped) "the write loop should stop after the client disconnects"
        Expect.isNull failure.Value "no exception expected"
        Expect.isError result.Value "tryWriteChunk should report the failed write") ctx
  ]

[<Tests>]
let chunkedStatusTests (cfg : SuaveConfig) =
  let runWithConfig = runWith cfg

  let writer (conn : Connection) = task {
    do! conn.writeChunk "ok"B
    do! conn.writeLastChunk () }

  testList "TransferEncoding.chunked status and headers" [
    testCase "a fresh response is answered with 200" <| fun _ ->
      let ctx = runWithConfig (TransferEncoding.chunked writer)
      Expect.equal (reqStatusCode HttpMethod.GET "/" None ctx) HttpStatusCode.OK "status"

    testCase "a status set before is kept" <| fun _ ->
      let ctx = runWithConfig (Writers.setStatus HTTP_201 >=> TransferEncoding.chunked writer)
      Expect.equal (reqStatusCode HttpMethod.GET "/" None ctx) HttpStatusCode.Created "status"

    testCase "a 404 can be set after" <| fun _ ->
      let ctx = runWithConfig (TransferEncoding.chunked writer >=> Writers.setStatus HTTP_404)
      Expect.equal (reqStatusCode HttpMethod.GET "/" None ctx) HttpStatusCode.NotFound "status"

    testCase "headers are flushed before the writer runs" <| fun _ ->
      let app =
        TransferEncoding.chunked (fun conn -> task {
          // straight to the transport, bypassing the line buffer
          let! _ = conn.transport.write (Memory<byte>("5\r\nhello\r\n0\r\n\r\n"B))
          () })
      let ctx = runWithConfig app
      Expect.equal (req HttpMethod.GET "/" None ctx) "hello" "body after headers"
  ]