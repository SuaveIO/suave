module Suave.Tests.WebSocket

open Expecto

open System
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Threading

open Suave
open Suave.Operators
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open Suave.Utils
open Suave.Tests.TestUtilities
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
    fun cx -> socket {
      let loop = ref true
      while !loop do
        let! msg = webSocket.read()
        match msg with
        | (Text, data, true) ->
          let str = UTF8.toString data
          match str with
          | "BinaryRequest7bit" ->
            let message = ArraySegment(Array.create (int PayloadSize.Bit7) 0uy)
            do! webSocket.send Binary message true
          | "BinaryRequest16bit" ->
            let message = ArraySegment(Array.create (int PayloadSize.Bit16) 0uy)
            do! webSocket.send Binary message true
          | "BinaryRequest32bit" ->
            let message = ArraySegment(Array.create (int PayloadSize.Bit32) 0uy)
            do! webSocket.send Binary message true
          | _ ->
            do! webSocket.send Text (ArraySegment data) true
        | (Ping, _, _) ->
          do! webSocket.send Pong (ArraySegment([||])) true
        | (Close, _, _) ->
          do! webSocket.send Close (ArraySegment([||])) true
          loop := false
        | _ -> ()
      }
  
  let websocketAppReusingBuffersUrl = "/websocketAppReusingBuffers"
  let websocketAppReusingBuffers (webSocket : WebSocket) =
    fun cx -> socket {
      let loop = ref true
      while !loop do
        let! msg = webSocket.readIntoByteSegment (fun lengthRequired -> ArraySegment(reuseableBuffer, 0, lengthRequired))
        match msg with
        | (Text, data, true) ->
          let str = System.Text.Encoding.UTF8.GetString(data.Array, data.Offset, data.Count)
          match str with
          | "BinaryRequest7bit" ->
            let message = ArraySegment(Array.create (int PayloadSize.Bit7) 0uy)
            do! webSocket.send Binary message true
          | "BinaryRequest16bit" ->
            let message = ArraySegment(Array.create (int PayloadSize.Bit16) 0uy)
            do! webSocket.send Binary message true
          | "BinaryRequest32bit" ->
            let message = ArraySegment(Array.create (int PayloadSize.Bit32) 0uy)
            do! webSocket.send Binary message true
          | _ ->
            do! webSocket.send Text data true
        | (Ping, _, _) ->
          do! webSocket.send Pong (ArraySegment([||])) true
        | (Close, _, _) ->
          do! webSocket.send Close (ArraySegment([||])) true
          loop := false
        | _ -> ()
      }

  let websocketAppSubprotocolUrl = "/websocketAppSubprotocolUrl"
  let websocketAppSupportSubprotocol = "support.suave.io"
  let websocketAppSubprotocol (webSocket: WebSocket) =
    fun cx -> socket {
      let loop = ref true
      while !loop do
        let! msg = webSocket.readIntoByteSegment (fun lengthRequired -> ArraySegment(reuseableBuffer, 0, lengthRequired))
        match msg with
        | (Text, data, true) ->
          let str = System.Text.Encoding.UTF8.GetString(data.Array, data.Offset, data.Count)
          match str with
          | "BinaryRequest7bit" ->
            let message = ArraySegment(Array.create (int PayloadSize.Bit7) 0uy)
            do! webSocket.send Binary message true
          | "BinaryRequest16bit" ->
            let message = ArraySegment(Array.create (int PayloadSize.Bit16) 0uy)
            do! webSocket.send Binary message true
          | "BinaryRequest32bit" ->
            let message = ArraySegment(Array.create (int PayloadSize.Bit32) 0uy)
            do! webSocket.send Binary message true
          | "Subprotocol" ->
            match webSocket.subprotocol with
            | Some subprotocol ->
              do! webSocket.send Text (ArraySegment (System.Text.Encoding.UTF8.GetBytes subprotocol)) true
            | None ->
              do! webSocket.send Text (ArraySegment [||]) true
          | _ ->
            do! webSocket.send Text data true
        | (Ping, _, _) ->
          do! webSocket.send Pong (ArraySegment([||])) true
        | (Close, _, _) ->
          do! webSocket.send Close (ArraySegment([||])) true
          loop := false
        | _ -> ()
    }

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

  let testCase webSocketUrl subprotocols testName fn =
    testCase testName <| fun _ ->
      use mre = new ManualResetEvent(false)
      use clientWebSocket = new WebSocketSharp.WebSocket(sprintf "ws://%s:%i%s" ip port webSocketUrl, subprotocols)
      let ctx = runWithConfig webPart
      withContext (fun _ -> fn mre clientWebSocket) ctx
  
  let websocketTests websocketUrl subprotocols = 
    testList (sprintf "websocket tests for url %s" websocketUrl) [
      testCase websocketUrl subprotocols "text echo test" <| fun mre clientWebSocket ->
        let message = "Hello Websocket World!"
        let echo : string ref = ref ""

        clientWebSocket.OnMessage.Add(fun e ->
          echo := e.Data
          mre.Set() |> ignore
        )
        clientWebSocket.Connect()
        clientWebSocket.Send(message)

        mre.WaitOne() |> ignore
        clientWebSocket.Close()
        Expect.equal echo.Value message "Should be echoed"

      testCase websocketUrl subprotocols "subprotocol support test" <| fun mre clientWebSocket ->
        let message = "Subprotocol"
        let echo : string ref = ref ""

        clientWebSocket.OnMessage.Add(fun e ->
          echo:= e.Data
          mre.Set() |> ignore
        )
        clientWebSocket.Connect()
        clientWebSocket.Send(message)

        mre.WaitOne() |> ignore
        clientWebSocket.Close()
        if Array.isEmpty subprotocols then
          Expect.equal echo.Value message "Should test equal"
        else
          Expect.equal websocketAppSupportSubprotocol echo.Value "Should test equal"

      testCase websocketUrl [|"unsupport.suave.io"|] "subprotocol unsupport test" <| fun mre clientWebSocket ->
        let message = "Subprotocol"
        let mutable code : uint16 = 0us

        clientWebSocket.OnClose.Add(fun e ->
          code <- e.Code
          mre.Set() |> ignore
        )
        clientWebSocket.Connect()

        mre.WaitOne() |> ignore
        Expect.equal code (uint16 WebSocketSharp.CloseStatusCode.ProtocolError) "Should test equal"

      testCase websocketUrl subprotocols "socket binary payload 7bit" <| fun mre clientWebSocket ->
        let message = "BinaryRequest7bit"
        let echo : byte array ref = ref [||]

        clientWebSocket.OnMessage.Add(fun e ->
          echo:= e.RawData
          mre.Set() |> ignore
        )
        clientWebSocket.Connect()
        clientWebSocket.Send(message)

        mre.WaitOne() |> ignore
        clientWebSocket.Close()
        Expect.isTrue (testByteArray (int PayloadSize.Bit7) echo.Value) "Should test equal"

      testCase websocketUrl subprotocols "socket binary payload 16bit" <| fun mre clientWebSocket ->
        let message = "BinaryRequest16bit"
        let echo : byte array ref = ref [||]

        clientWebSocket.OnMessage.Add(fun e ->
          echo:= e.RawData
          mre.Set() |> ignore
        )
        clientWebSocket.Connect()
        clientWebSocket.Send(message)

        mre.WaitOne() |> ignore
        clientWebSocket.Close()
        Expect.isTrue (testByteArray (int PayloadSize.Bit16) echo.Value) "Should be echoed"

      testCase websocketUrl subprotocols "socket binary payload 32bit" <| fun mre clientWebSocket ->
        let message = "BinaryRequest32bit"
        let echo : byte array ref = ref [||]

        clientWebSocket.OnMessage.Add(fun e ->
          echo:= e.RawData
          mre.Set() |> ignore)
        clientWebSocket.Connect()
        clientWebSocket.Send(message)

        mre.WaitOne() |> ignore
        clientWebSocket.Close()
        Expect.isTrue (testByteArray (int PayloadSize.Bit32) echo.Value) "Should be echoed"

      testCase websocketUrl subprotocols "echo large number of messages to client" <| fun mre clientWebSocket ->
        let amountOfMessages = 1000
        let echo = ref []
        let count = ref 0

        clientWebSocket.OnMessage.Add(fun e ->
          echo:= e.Data :: !echo
          count := !count + 1
          if !count = amountOfMessages then mre.Set() |> ignore
        )
        clientWebSocket.Connect()

        for i = 1 to amountOfMessages do
          clientWebSocket.Send(i.ToString())

        mre.WaitOne() |> ignore
        clientWebSocket.Close()

        let expectedMessages = [ for i in amountOfMessages .. -1 .. 1 -> i.ToString() ]

        Expect.equal echo.Value expectedMessages "should be equal"
        Expect.equal (!count) amountOfMessages "received message count on websocket"
    ]
  
  testList "websocket tests" [ websocketTests websocketAppUrl [||]
                               websocketTests websocketAppReusingBuffersUrl [||]
                               websocketTests websocketAppSubprotocolUrl [|websocketAppSupportSubprotocol|]]