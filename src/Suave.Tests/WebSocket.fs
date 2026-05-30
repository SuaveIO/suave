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
                                   ctx ]
