module Suave.Tests.WebSocket

open Expecto

open System
open System.IO
open System.Net.Sockets
open Websocket.Client
open System.Text
open System.Threading

open Suave
open Suave.Operators
open Suave.Sockets
open Suave.WebSocket
open Suave.Testing

type PayloadSize =
  | Bit7 = 125
  | Bit16 = 129
  | Bit32 = 66000

[<Tests>]
let websocketTests cfg =
  let ip, port =
    let binding = SuaveConfig.firstBinding cfg
    string binding.socketBinding.ip,
    int binding.socketBinding.port

  let runWithConfig = runWith cfg

  let reuseableBuffer = Array.zeroCreate 1000

  let websocketAppUrl = "/websocket"
  let websocketApp (webSocket : WebSocket) =
    fun cx -> SocketOp.ofTask(task {
      let mutable continueLoop = true
      while continueLoop do
        let! msgResult = webSocket.read()
        match msgResult with
        | Ok msg ->
          match msg with
          | (Text, data:ByteSegment, true) ->
            let str = Encoding.UTF8.GetString data.Span
            let responseData =
              match str with
              | "BinaryRequest7bit" -> Binary, Memory<_>(Array.create (int PayloadSize.Bit7) 0uy)
              | "BinaryRequest16bit" -> Binary, Memory<_>(Array.create (int PayloadSize.Bit16) 0uy)
              | "BinaryRequest32bit" -> Binary, Memory<_>(Array.create (int PayloadSize.Bit32) 0uy)
              | _ -> Text, data
            let! _ = webSocket.send (fst responseData) (snd responseData) true
            ()
          | (Ping, _, _) ->
            let! _ = webSocket.send Pong (Memory<_>([||])) true
            ()
          | (Close, _, _) ->
            let! _ = webSocket.send Close (Memory<_>([||])) true
            continueLoop <- false
          | _ -> ()
        | Result.Error _ ->
          continueLoop <- false
      return Ok()
      })
  
  let websocketAppReusingBuffersUrl = "/websocketAppReusingBuffers"
  let websocketAppReusingBuffers (webSocket : WebSocket) =
    fun cx -> SocketOp.ofTask(task {
      let mutable continueLoop = true
      while continueLoop do
        let! msgResult = webSocket.readIntoByteSegment (fun lengthRequired -> Memory<_>(reuseableBuffer, 0, lengthRequired))
        match msgResult with
        | Ok msg ->
          match msg with
          | (Text, data, true) ->
            let str = System.Text.Encoding.UTF8.GetString(data.Span)
            let responseData =
              match str with
              | "BinaryRequest7bit" -> Binary, Memory<_>(Array.create (int PayloadSize.Bit7) 0uy)
              | "BinaryRequest16bit" -> Binary, Memory<_>(Array.create (int PayloadSize.Bit16) 0uy)
              | "BinaryRequest32bit" -> Binary, Memory<_>(Array.create (int PayloadSize.Bit32) 0uy)
              | _ -> Text, data
            let! _ = webSocket.send (fst responseData) (snd responseData) true
            ()
          | (Ping, _, _) ->
            let! _ = webSocket.send Pong (Memory<_>([||])) true
            ()
          | (Close, _, _) ->
            let! _ = webSocket.send Close (Memory<_>([||])) true
            continueLoop <- false
          | _ -> ()
        | Result.Error _ ->
          continueLoop <- false
      return Ok()
      })

  let websocketAppSubprotocolUrl = "/websocketAppSubprotocolUrl"
  let websocketAppSupportSubprotocol = "support.suave.io"
  let websocketAppSubprotocol (webSocket: WebSocket) =
    fun cx -> SocketOp.ofTask(task {
      let mutable continueLoop = true
      while continueLoop do
        let! msgResult = webSocket.readIntoByteSegment (fun lengthRequired -> Memory<_>(reuseableBuffer, 0, lengthRequired))
        match msgResult with
        | Ok msg ->
          match msg with
          | (Text, data, true) ->
            let str = System.Text.Encoding.UTF8.GetString(data.Span)
            let responseData =
              match str with
              | "BinaryRequest7bit" -> Binary, Memory<_>(Array.create (int PayloadSize.Bit7) 0uy)
              | "BinaryRequest16bit" -> Binary, Memory<_>(Array.create (int PayloadSize.Bit16) 0uy)
              | "BinaryRequest32bit" -> Binary, Memory<_>(Array.create (int PayloadSize.Bit32) 0uy)
              | "Subprotocol" ->
                match webSocket.subprotocol with
                | Some subprotocol -> Text, Memory<_>(System.Text.Encoding.UTF8.GetBytes subprotocol)
                | None -> Text, Memory<_>([||])
              | _ -> Text, data
            let! _ = webSocket.send (fst responseData) (snd responseData) true
            ()
          | (Ping, _, _) ->
            let! _ = webSocket.send Pong (Memory<_>([||])) true
            ()
          | (Close, _, _) ->
            let! _ = webSocket.send Close (Memory<_>([||])) true
            continueLoop <- false
          | _ -> ()
        | Result.Error _ ->
          continueLoop <- false
      return Ok()
    })

  let webPart = 
    choose 
      [ Filters.path websocketAppUrl >=> handShake websocketApp
        Filters.path websocketAppReusingBuffersUrl >=> handShake websocketAppReusingBuffers
        Filters.path websocketAppSubprotocolUrl >=> handShakeWithSubprotocol (chooseSubprotocol websocketAppSupportSubprotocol) websocketAppSubprotocol ]

  let testByteArray (sentSize:int) (bArray: byte []) =
    if sentSize = bArray.Length then
      Array.forall (fun (ele:byte) ->
        match  0uy.CompareTo(ele) with
        | 0 -> true
        | _ -> false
      ) bArray
    else
      false

  let websocketTestCase webSocketUrl subprotocols testName fn =
    testCase testName <| fun _ ->
      use mre = new ManualResetEvent(false)
      let clientFactory _=
        let client = new System.Net.WebSockets.ClientWebSocket()
        for proto in subprotocols do
          client.Options.AddSubProtocol proto
        client
      use clientWebSocket = new WebsocketClient(new Uri(sprintf "ws://%s:%i%s" ip port webSocketUrl), clientFactory)
      clientWebSocket.IsReconnectionEnabled <- false
      let ctx = runWithConfig webPart
      withContext (fun _ -> fn mre clientWebSocket) ctx

  let websocketTests websocketUrl subprotocols = 
    testList (sprintf "websocket tests for url %s" websocketUrl) [
      websocketTestCase websocketUrl subprotocols "text echo test" <| fun mre clientWebSocket ->
        let message = "Hello Websocket World!"
        let echo : string ref = ref ""

        clientWebSocket.MessageReceived.Subscribe(fun e ->
          echo := e.Text
          mre.Set() |> ignore
        ) |> ignore
        clientWebSocket.Start()|> ignore
        
        clientWebSocket.Send(message) |> ignore
        mre.WaitOne() |> ignore
        let stop = clientWebSocket.Stop(Net.WebSockets.WebSocketCloseStatus.NormalClosure,"Closing")
        stop.Wait()

        Expect.equal echo.Value message "Should be echoed"

      websocketTestCase websocketUrl subprotocols "subprotocol support test" <| fun mre clientWebSocket ->
        let message = "Subprotocol"
        let echo : string ref = ref ""

        clientWebSocket.MessageReceived.Subscribe(fun e ->
          echo.Value <- e.Text
          mre.Set() |> ignore
        )|> ignore
        clientWebSocket.Start() |> ignore
        clientWebSocket.Send(message) |> ignore

        mre.WaitOne() |> ignore
        let stop = clientWebSocket.Stop(Net.WebSockets.WebSocketCloseStatus.NormalClosure,"Closing")
        stop.Wait()

        if Array.isEmpty subprotocols then
          Expect.equal echo.Value message "Should test equal"
        else
          Expect.equal websocketAppSupportSubprotocol echo.Value "Should test equal"

      websocketTestCase websocketUrl [|"unsupport.suave.io"|] "subprotocol unsupport test" <| fun mre clientWebSocket ->
        if websocketUrl = websocketAppSubprotocolUrl then
          let message = "Subprotocol"
          let echo : string ref = ref ""

          clientWebSocket.MessageReceived.Subscribe(fun e ->
            echo.Value <- e.Text
            mre.Set() |> ignore
          ) |> ignore
          clientWebSocket.Start() |> ignore
          clientWebSocket.Send(message) |> ignore

          mre.WaitOne() |> ignore

          Expect.equal echo.Value "" "Should match no protocol"

      websocketTestCase websocketUrl subprotocols "socket binary payload 7bit" <| fun mre clientWebSocket ->
        let message = "BinaryRequest7bit"
        let echo : byte array ref = ref [||]

        clientWebSocket.MessageReceived.Subscribe(fun e ->
          echo.Value <- e.Binary
          mre.Set() |> ignore
        )|> ignore
        clientWebSocket.Start()|> ignore
        clientWebSocket.Send(message) |> ignore

        mre.WaitOne() |> ignore
        let stop = clientWebSocket.Stop(Net.WebSockets.WebSocketCloseStatus.NormalClosure,"Closing")
        stop.Wait()

        Expect.isTrue (testByteArray (int PayloadSize.Bit7) echo.Value) "Should test equal"

      websocketTestCase websocketUrl subprotocols "socket binary payload 16bit" <| fun mre clientWebSocket ->
        let message = "BinaryRequest16bit"
        let echo : byte array ref = ref [||]

        clientWebSocket.MessageReceived.Subscribe(fun e ->
          echo.Value <- e.Binary
          mre.Set() |> ignore
        )|> ignore
        clientWebSocket.Start()|> ignore
        clientWebSocket.Send(message) |> ignore

        mre.WaitOne() |> ignore
        let stop = clientWebSocket.Stop(Net.WebSockets.WebSocketCloseStatus.NormalClosure,"Closing")
        stop.Wait()

        Expect.isTrue (testByteArray (int PayloadSize.Bit16) echo.Value) "Should be echoed"

      websocketTestCase websocketUrl subprotocols "socket binary payload 32bit" <| fun mre clientWebSocket ->
        let message = "BinaryRequest32bit"
        let echo : byte array ref = ref [||]

        clientWebSocket.MessageReceived.Subscribe(fun e ->
          echo:= e.Binary
          mre.Set() |> ignore) |> ignore
        clientWebSocket.Start()|> ignore
        clientWebSocket.Send(message) |> ignore

        mre.WaitOne() |> ignore
        let stop = clientWebSocket.Stop(Net.WebSockets.WebSocketCloseStatus.NormalClosure,"Closing")
        stop.Wait()

        Expect.isTrue (testByteArray (int PayloadSize.Bit32) echo.Value) "Should be echoed"

      websocketTestCase websocketUrl subprotocols "echo large number of messages to client" <| fun mre clientWebSocket ->
        let amountOfMessages = 1000
        let echo = ref []
        let count = ref 0

        clientWebSocket.MessageReceived.Subscribe(fun e ->
          echo:= e.Text :: !echo
          count := !count + 1
          if !count = amountOfMessages then mre.Set() |> ignore
        )|> ignore
        clientWebSocket.Start()|> ignore

        for i = 1 to amountOfMessages do
          clientWebSocket.Send(i.ToString()) |> ignore

        mre.WaitOne() |> ignore

        let stop = clientWebSocket.Stop(Net.WebSockets.WebSocketCloseStatus.NormalClosure,"Closing")
        stop.Wait()

        let expectedMessages = [ for i in amountOfMessages .. -1 .. 1 -> i.ToString() ]

        Expect.equal echo.Value expectedMessages "should be equal"
        Expect.equal (!count) amountOfMessages "received message count on websocket"
    ]
  
  /// Tests that drive the protocol over a raw TCP socket, so they can send
  /// frames a well-behaved client never would and see every byte Suave writes.
  let rawSocketTests =
    let readExactly (stream : Stream) n =
      let buffer = Array.zeroCreate n
      let mutable offset = 0
      while offset < n do
        let read = stream.Read(buffer, offset, n - offset)
        if read = 0 then failwithf "connection closed after %d of %d bytes" offset n
        offset <- offset + read
      buffer

    /// Everything the server writes until it closes the connection.
    let readToEnd (stream : Stream) =
      use rest = new MemoryStream()
      stream.CopyTo rest
      rest.ToArray()

    /// Upgrades a fresh connection to `path` and returns the response head.
    let upgrade (stream : Stream) path =
      let request =
        sprintf "GET %s HTTP/1.1\r\nHost: %s:%i\r\nConnection: Upgrade\r\nUpgrade: websocket\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n" path ip port
        |> Encoding.ASCII.GetBytes
      stream.Write(request, 0, request.Length)
      // Read byte by byte so no frame bytes are swallowed with the head.
      let head = StringBuilder()
      while not (head.ToString().EndsWith "\r\n\r\n") do
        head.Append(char (readExactly stream 1).[0]) |> ignore
      head.ToString()

    /// A masked client frame header announcing `length` payload bytes.
    let frameHeader opcode (length : uint64) =
      [| yield 0x80uy ||| opcode
         if length < 126UL then
           yield 0x80uy ||| byte length
         elif length < 65536UL then
           yield 0x80uy ||| 126uy
           yield byte (length >>> 8)
           yield byte length
         else
           yield 0x80uy ||| 127uy
           for shift in 56 .. -8 .. 0 do yield byte (length >>> shift)
         yield! [| 1uy; 2uy; 3uy; 4uy |] |]

    let clientFrame opcode (payload : byte[]) =
      Array.append (frameHeader opcode (uint64 payload.Length))
                   (payload |> Array.mapi (fun i b -> b ^^^ byte (i % 4 + 1)))

    /// Reads one unmasked server frame with a short payload.
    let readServerFrame (stream : Stream) =
      let head = readExactly stream 2
      let length = int (head.[1] &&& 0x7Fuy)
      Expect.isLessThan length 126 "expected a short server frame"
      head.[0], readExactly stream length

    let closeCode (payload : byte[]) = int payload.[0] * 256 + int payload.[1]

    let rawTest name (config : SuaveConfig) app (f : Stream -> unit) =
      testCase name <| fun _ ->
        let ctx = runWith config (Filters.path "/raw" >=> handShake app)
        withContext (fun _ ->
          use client = new TcpClient(ip, port)
          client.ReceiveTimeout <- 5000
          use stream = client.GetStream()
          let head = upgrade stream "/raw"
          Expect.stringStarts head "HTTP/1.1 101 Switching Protocols" "should complete websocket upgrade"
          Expect.isFalse (head.ToLowerInvariant().Contains "content-type") "101 response should carry no Content-Type"
          f stream) ctx

    /// Echoes frames read with `readIntoByteSegment`, counting buffer requests.
    let echoApp (bufferRequests : int ref) (webSocket : WebSocket) =
      fun _ -> SocketOp.ofTask(task {
        let mutable continueLoop = true
        while continueLoop do
          let! msg = webSocket.readIntoByteSegment (fun n ->
            Interlocked.Increment &bufferRequests.contents |> ignore
            Memory<_>(Array.zeroCreate n))
          match msg with
          | Ok (Binary, data, _) ->
            let! _ = webSocket.send Binary data true
            ()
          | _ -> continueLoop <- false
        return Ok ()
      })

    testList "raw socket" [
      let bufferRequests = ref 0
      rawTest "frame announcing 2 GB is refused with 1009 before any buffer is requested" cfg (echoApp bufferRequests) <| fun stream ->
        let header = frameHeader 2uy (uint64 Int32.MaxValue)
        stream.Write(header, 0, header.Length)
        let opcode, payload = readServerFrame stream
        Expect.equal opcode 0x88uy "server should send a final Close frame"
        Expect.equal (closeCode payload) 1009 "close code should be 1009 (message too big)"
        Expect.equal (readToEnd stream) [||] "server should end the connection"
        Expect.equal bufferRequests.Value 0 "no buffer should be requested for the oversized frame"

      let limitBufferRequests = ref 0
      rawTest "webSocketMaxFrameSize bounds accepted frames" { cfg with webSocketMaxFrameSize = 1024 } (echoApp limitBufferRequests) <| fun stream ->
        let atLimit = Array.create 1024 7uy
        let frame = clientFrame 2uy atLimit
        stream.Write(frame, 0, frame.Length)
        let echoHead = readExactly stream 4
        Expect.equal echoHead [| 0x82uy; 126uy; 4uy; 0uy |] "frame at the limit should be echoed"
        Expect.equal (readExactly stream 1024) atLimit "echoed payload should match"

        let header = frameHeader 2uy 1025UL
        stream.Write(header, 0, header.Length)
        let opcode, payload = readServerFrame stream
        Expect.equal opcode 0x88uy "server should send a final Close frame"
        Expect.equal (closeCode payload) 1009 "close code should be 1009 (message too big)"
        Expect.equal (readToEnd stream) [||] "server should end the connection"
        Expect.equal limitBufferRequests.Value 1 "only the frame at the limit should get a buffer"

      let throwingApp (webSocket : WebSocket) =
        fun _ -> SocketOp.ofTask(task {
          let! _ = webSocket.readIntoByteSegment (fun _ -> failwith "boom from length function")
          return Ok ()
        })
      rawTest "handler exception closes with 1011 and writes no HTTP response" cfg throwingApp <| fun stream ->
        let frame = clientFrame 1uy (Encoding.UTF8.GetBytes "hello")
        stream.Write(frame, 0, frame.Length)
        let opcode, payload = readServerFrame stream
        Expect.equal opcode 0x88uy "server should send a final Close frame"
        Expect.equal (closeCode payload) 1011 "close code should be 1011 (internal error)"
        let rest = Encoding.ASCII.GetString(readToEnd stream)
        Expect.isFalse (rest.Contains "HTTP/1.1") "no HTTP response may follow the upgrade"
        Expect.isFalse (rest.Contains "boom") "exception text must not reach the client"

      let synchronousThrowApp (_ : WebSocket) : HttpContext -> SocketOp<unit> =
        fun _ -> failwith "boom before returning a SocketOp"
      rawTest "synchronous handler exception closes with 1011 and writes no HTTP response" cfg synchronousThrowApp <| fun stream ->
        let opcode, payload = readServerFrame stream
        Expect.equal opcode 0x88uy "server should send a final Close frame"
        Expect.equal (closeCode payload) 1011 "close code should be 1011 (internal error)"
        let rest = Encoding.ASCII.GetString(readToEnd stream)
        Expect.isFalse (rest.Contains "HTTP/1.1") "no HTTP response may follow the upgrade"

      let returnedContextTest name (handShakePart : WebPart) =
        testCase name <| fun _ ->
          let returned = ref None
          use logged = new ManualResetEventSlim(false)
          let logging : WebPart = fun ctx -> async {
            let! r = handShakePart ctx
            returned.Value <- r |> Option.map (fun c -> c.response.status.code, c.response.headers, Filters.logFormat c)
            logged.Set()
            return r }
          let ctx = runWith cfg (Filters.path "/raw" >=> logging)
          withContext (fun _ ->
            use client = new TcpClient(ip, port)
            client.ReceiveTimeout <- 5000
            use stream = client.GetStream()
            let head = upgrade stream "/raw"
            Expect.stringStarts head "HTTP/1.1 101 Switching Protocols" "should complete websocket upgrade"
            Expect.equal (readToEnd stream) [||] "nothing may be written after the 101"
            Expect.isTrue (logged.Wait 5000) "the logging WebPart should see the result") ctx
          match returned.Value with
          | Some (code, headers, line) ->
            Expect.equal code 101 "returned context should carry the 101 that was sent"
            Expect.equal (headers |> List.tryFind (fst >> (=) "Sec-WebSocket-Accept") |> Option.map snd) (Some "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=") "returned context should carry the handshake headers"
            Expect.stringContains line "\" 101 " "logFormat should log 101"
          | None -> failtest "handShake should return a context"

      returnedContextTest "handShake returns a context carrying the 101" (handShake (fun _ _ -> SocketOp.mreturn ()))
      returnedContextTest "handShakeWithSubprotocol returns a context carrying the 101"
        (handShakeWithSubprotocol (fun _ _ -> async.Return None) (fun _ _ -> SocketOp.mreturn ()))

      testCase "handShake answers a request without Upgrade with 400" <| fun _ ->
        let ctx = runWith cfg (Filters.path "/raw" >=> handShake (fun _ _ -> SocketOp.mreturn ()))
        withContext (fun _ ->
          use client = new TcpClient(ip, port)
          client.ReceiveTimeout <- 5000
          use stream = client.GetStream()
          let request = sprintf "GET /raw HTTP/1.1\r\nHost: %s:%i\r\nConnection: close\r\n\r\n" ip port |> Encoding.ASCII.GetBytes
          stream.Write(request, 0, request.Length)
          let response = Encoding.ASCII.GetString(readToEnd stream)
          Expect.stringStarts response "HTTP/1.1 400 Bad Request" "should refuse the handshake") ctx
    ]

  testList "websocket tests" [ websocketTests websocketAppUrl [||];
                               websocketTests websocketAppReusingBuffersUrl [||];
                               websocketTests websocketAppSubprotocolUrl [|websocketAppSupportSubprotocol|];
                               testCase "raw tcp half-close exits websocket handler promptly" <| fun _ ->
                                 use handlerExited = new ManualResetEventSlim(false)
                                 let websocketAppHalfClose (webSocket : WebSocket) =
                                   fun _ -> SocketOp.ofTask(task {
                                     let mutable continueLoop = true
                                     while continueLoop do
                                       let! msgResult = webSocket.read()
                                       match msgResult with
                                       | Ok _
                                       | Result.Error _ -> continueLoop <- false
                                     handlerExited.Set()
                                     return Ok()
                                   })

                                 let ctx = runWithConfig (Filters.path "/websocket-half-close" >=> handShake websocketAppHalfClose)

                                 withContext (fun _ ->
                                   use client = new TcpClient(ip, port)
                                   client.ReceiveTimeout <- 2000
                                   use stream = client.GetStream()
                                   let request =
                                     sprintf "GET /websocket-half-close HTTP/1.1\r\nHost: %s:%i\r\nConnection: Upgrade\r\nUpgrade: websocket\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n" ip port
                                     |> Encoding.ASCII.GetBytes
                                   stream.Write(request, 0, request.Length)
                                   stream.Flush()

                                   use reader = new StreamReader(stream, Encoding.ASCII, false, 1024, true)
                                   let statusLine = reader.ReadLine()
                                   Expect.equal statusLine "HTTP/1.1 101 Switching Protocols" "should complete websocket upgrade"

                                   let rec readHeaders () =
                                     let line = reader.ReadLine()
                                     if not (String.IsNullOrEmpty line) then readHeaders ()
                                   readHeaders ()

                                   client.Client.Shutdown(SocketShutdown.Send)

                                   let exited = handlerExited.Wait(TimeSpan.FromSeconds 2.0)
                                   Expect.isTrue exited "handler should exit shortly after peer half-closes the socket")
                                   ctx
                               rawSocketTests ]
