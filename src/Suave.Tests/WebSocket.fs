module Suave.Tests.WebSocket

open Fuchu

open System
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Threading

open Suave
open Suave.Types
open Suave.Http
open Suave.Sockets
open Suave.WebSocket
open Suave.Utils
open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let websocket_tests =

  let runWithConfig = runWith defaultConfig

  let websocketApp (webSocket : WebSocket) =
    fun cx -> socket{
      let loop = ref true
      while !loop do
        let! msg = webSocket.read()
        match msg with
        | (Text, data, true) ->
          let str = UTF8.toString data
          do! webSocket.send Text data true
        | (Ping, _, _) ->
          do! webSocket.send Pong [||] true
        | (Close, _, _) ->
          do! webSocket.send Close [||] true
          loop := false
        | _ -> ()
      }

  let webPart =  Applicatives.path "/websocket" >>= handShake websocketApp

  testList "websocket tests" [
    testCase "echo test" <| fun _ ->
      let ctx = runWithConfig webPart

      withContext (fun _ ->

        let message = "Hello Websocket World!"

        use clientWebSocket = new WebSocketSharp.WebSocket("ws://127.0.0.1:8083/websocket")

        clientWebSocket.OnMessage.Add(fun e -> Assert.Equal("should be equal", e.Data , message))
        clientWebSocket.Connect()
        clientWebSocket.Send(message)

        // HACK: wait a little bit for the call-back
        Thread.Sleep(3000)
        clientWebSocket.Close()) ctx
  ]