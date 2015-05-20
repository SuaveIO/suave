
open Suave
open Suave.Types
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Successful
open Suave.Http.Files
open Suave.Http.RequestErrors
open Suave.Logging
open Suave.Web
open Suave.Utils

open System
open System.Net

open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket

let echo (webSocket : WebSocket) =
  fun cx -> socket {
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

let app : WebPart =
  choose [
    path "/websocket" >>= handShake echo
    GET >>= choose [ path "/" >>= file "index.html"; browseHome ];
    NOT_FOUND "Found no handlers."
    ]

[<EntryPoint>]
let main _ =
  startWebServer { defaultConfig with logger = Loggers.ConsoleWindowLogger LogLevel.Verbose } app
  0