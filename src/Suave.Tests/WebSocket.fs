module Suave.Tests.WebSocket

open Fuchu

open System
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Threading

open Suave
open Suave.Operators
open Suave.Http
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open Suave.Web
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
            let message = Array.create (int PayloadSize.Bit7) 0uy
            do! webSocket.send Binary message true
          | "BinaryRequest16bit" ->  
            let message = Array.create (int PayloadSize.Bit16) 0uy
            do! webSocket.send Binary message true
          | "BinaryRequest32bit" ->  
            let message = Array.create (int PayloadSize.Bit32) 0uy
            do! webSocket.send Binary message true
          | _ -> 
            do! webSocket.send Text data true
        | (Ping, _, _) ->
          do! webSocket.send Pong [||] true
        | (Close, _, _) ->
          do! webSocket.send Close [||] true
          loop := false
        | _ -> ()
      }

  let webPart = Applicatives.path "/websocket" >=> handShake websocketApp

  let testByteArray (sentSize:int) (bArray: byte []) = 
    if sentSize = bArray.Length then
      Array.forall (fun (ele:byte) -> 
        match  0uy.CompareTo(ele) with
        | 0 -> true
        | _ -> false
      ) bArray
    else 
      false

  testList "websocket tests" [
    testCase "text echo test" <| fun _ ->
      let ctx = runWithConfig webPart

      withContext (fun _ ->
        let mre = new ManualResetEvent(false)
        let message = "Hello Websocket World!"
        let echo : string ref = ref ""

        use clientWebSocket = new WebSocketSharp.WebSocket(sprintf "ws://%s:%i/websocket" ip port)

        clientWebSocket.OnMessage.Add(fun e ->
          echo := e.Data
          mre.Set() |> ignore
        )
        clientWebSocket.Connect()
        clientWebSocket.Send(message)

        mre.WaitOne() |> ignore
        clientWebSocket.Close()
        Assert.Equal("should be equal", echo.Value , message)
        ) ctx
    testCase "socket binary payload 7bit" <| fun _ ->
      let ctx = runWithConfig webPart

      withContext (fun _ ->
        let mre = new ManualResetEvent(false)
        let message = "BinaryRequest7bit"
        let echo : byte array ref = ref [||]

        use clientWebSocket = new WebSocketSharp.WebSocket(sprintf "ws://%s:%i/websocket" ip port)

        clientWebSocket.OnMessage.Add(fun e ->
          echo:= e.RawData
          mre.Set() |> ignore
        )
        clientWebSocket.Connect()
        clientWebSocket.Send(message)

        mre.WaitOne() |> ignore
        clientWebSocket.Close()
        Assert.Equal("should be equal", testByteArray (int PayloadSize.Bit7) echo.Value , true)
        ) ctx
    testCase "socket binary payload 16bit" <| fun _ ->
      let ctx = runWithConfig webPart

      withContext (fun _ ->
        let mre = new ManualResetEvent(false)
        let message = "BinaryRequest16bit"
        let echo : byte array ref = ref [||]

        use clientWebSocket = new WebSocketSharp.WebSocket(sprintf "ws://%s:%i/websocket" ip port)

        clientWebSocket.OnMessage.Add(fun e ->
          echo:= e.RawData
          mre.Set() |> ignore
        )
        clientWebSocket.Connect()
        clientWebSocket.Send(message)

        mre.WaitOne() |> ignore
        clientWebSocket.Close()
        Assert.Equal("should be equal", testByteArray (int PayloadSize.Bit16) echo.Value , true)
        ) ctx
    testCase "socket binary payload 32bit" <| fun _ ->
      let ctx = runWithConfig webPart

      withContext (fun _ ->
        let mre = new ManualResetEvent(false)
        let message = "BinaryRequest32bit"
        let echo : byte array ref = ref [||]

        use clientWebSocket = new WebSocketSharp.WebSocket(sprintf "ws://%s:%i/websocket" ip port)

        clientWebSocket.OnMessage.Add(fun e ->
          echo:= e.RawData
          mre.Set() |> ignore)
        clientWebSocket.Connect()
        clientWebSocket.Send(message)

        mre.WaitOne() |> ignore
        clientWebSocket.Close()
        Assert.Equal("should be equal", testByteArray (int PayloadSize.Bit32) echo.Value , true)
        ) ctx
  ]