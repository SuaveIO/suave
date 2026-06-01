open Suave
open Suave.Operators
open Suave.Filters
open Suave.Files
open Suave.RequestErrors

open System
open System.Text

open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open System.Collections.Concurrent
open System.Timers
open System.Threading
open System.Reflection

type private Subscriber =
  { ws            : WebSocket
    mutable lastSeenMs : int64
    forceShutdown : unit -> unit }

let private nowMs () = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

let private buildForceShutdown (ws : WebSocket) : unit -> unit =
  try
    let connField =
      ws.GetType().GetFields(BindingFlags.Instance ||| BindingFlags.NonPublic)
      |> Array.tryFind (fun f -> f.FieldType = typeof<Connection>)
    match connField with
    | None -> fun () -> ()
    | Some f ->
      fun () ->
        try
          let conn = f.GetValue(ws) :?> Connection
          // ITransport.shutdown returns a ValueTask in Suave 3.x; ignore
          // the result. Any failure means the socket is already gone.
          conn.transport.shutdown() |> ignore
        with _ -> ()
  with _ -> fun () -> ()

type Broadcaster(?heartbeatMs : int, ?idleTimeoutMs : int) =
  let subscribers = ConcurrentDictionary<Guid, Subscriber>()
  let heartbeatMs   = defaultArg heartbeatMs   30_000
  let idleTimeoutMs = defaultArg idleTimeoutMs 90_000

  let drop (id : Guid) (reason : string) =
    match subscribers.TryRemove id with
    | true, s ->
      printfn "[hub-drop] id=%O reason=%s remaining=%d" id reason subscribers.Count
      // Force the underlying socket closed so a wedged reader unblocks
      // and the handler's `finally` actually runs. Safe to call even if
      // the socket is already gone.
      s.forceShutdown ()
    | _ -> ()

  let trySend (id : Guid) (s : Subscriber) (op : Opcode) (seg : ByteSegment) =
    task {
      try
        let! r = s.ws.send op seg true
        match r with
        | Ok () -> ()
        | Result.Error _ -> drop id "send-error"
      with _ -> drop id "send-exception"
    } :> System.Threading.Tasks.Task

  let tick _ =
    let now = nowMs ()
    let empty = ByteSegment [||]
    let snapshot = subscribers |> Seq.toArray
    let mutable pinged = 0
    let mutable dropped = 0
    for KeyValue(id, s) in snapshot do
      if now - s.lastSeenMs > int64 idleTimeoutMs then
        // No frame from the peer in the idle window. Even on a
        // half-closed TCP socket the client should have answered our
        // last Ping with a Pong; absence means the connection is dead.
        drop id "idle-watchdog"
        dropped <- dropped + 1
      else
        trySend id s Ping empty |> ignore
        pinged <- pinged + 1
    printfn "[hub-tick] count=%d pinged=%d dropped=%d" subscribers.Count pinged dropped

  let heartbeat =
    new Timer(TimerCallback(tick), null, heartbeatMs, heartbeatMs)

  // Counters so we can sample Publish-rate without per-call logging.
  let mutable publishCount = 0L
  let mutable lastPublishLogMs = nowMs ()

  member internal _.Touch(id : Guid) =
    match subscribers.TryGetValue id with
    | true, s -> s.lastSeenMs <- nowMs ()
    | _ -> ()

  member _.Subscribe(ws : WebSocket) : Guid =
    let id = Guid.NewGuid()
    let s =
      { ws            = ws
        lastSeenMs    = nowMs ()
        forceShutdown = buildForceShutdown ws }
    subscribers.[id] <- s
    id

  member _.Unsubscribe(id : Guid) =
    subscribers.TryRemove(id) |> ignore

  member _.Count = subscribers.Count

  /// Fire-and-forget broadcast of a UTF-8 text payload.
  member this.Publish(json : string) =
    let bytes = Encoding.UTF8.GetBytes json
    let segment = ByteSegment bytes
    let snapshot = subscribers |> Seq.toArray
    for KeyValue(id, s) in snapshot do
      trySend id s Text segment |> ignore
    let n = System.Threading.Interlocked.Increment(&publishCount)
    // Log once a second with rate + fan-out size so we can see how much
    // work zombies multiply.
    let now = nowMs ()
    if now - lastPublishLogMs >= 1000L then
      lastPublishLogMs <- now
      printfn "[hub-publish] total=%d subs=%d bytes=%d" n snapshot.Length bytes.Length

  interface IDisposable with
    member _.Dispose() = heartbeat.Dispose()

let ws (hub:Broadcaster) (webSocket : WebSocket) (context: HttpContext) =
  socket {
    let id = hub.Subscribe webSocket
    // if `loop` is set to false, the server will stop receiving messages
    let mutable loop = true

    while loop do
      // the server will wait for a message to be received without blocking the thread
      let! msg = webSocket.read()

      match msg with
      // the message has type (Opcode * byte [] * bool)
      //
      // Opcode type:
      //   type Opcode = Continuation | Text | Binary | Reserved | Close | Ping | Pong
      //
      // byte [] contains the actual message
      //
      // the last element is the FIN byte, explained later
      | (Text, data, true) ->
        // the message can be converted to a string
        let str = Encoding.UTF8.GetString data.Span
        let response = sprintf "response to %s" str

        // the response needs to be converted to a ByteSegment
        let byteResponse =
          response
          |> System.Text.Encoding.ASCII.GetBytes
          |> ByteSegment

        // the `send` function sends a message back to the client
        do! webSocket.send Text byteResponse true

      | (Close, _, _) ->
        let emptyResponse = [||] |> ByteSegment
        do! webSocket.send Close emptyResponse true

      | (Ping, data, _) ->
        do! webSocket.send Pong data true

      | (Pong, data, _) ->
        Console.WriteLine("Received Pong with data: {0}", Encoding.UTF8.GetString data.Span)

        // after sending a Close message, stop the loop
        loop <- false

      | _ -> ()
    }

/// An example of explictly fetching websocket errors and handling them in your codebase.
let wsWithErrorHandling (hub:Broadcaster) (webSocket : WebSocket) (context: HttpContext) = 

   let exampleDisposableResource = { new IDisposable with member __.Dispose() = printfn "Resource needed by websocket connection disposed" }
   let websocketWorkflow = ws hub webSocket context
   
   SocketOp.ofTask(task {
    let! successOrError = websocketWorkflow
    match successOrError with
    // Success case
    | Ok() -> ()
    // Error case
    | Result.Error(error) ->
        // Example error handling logic here
        printfn "Error: [%A]" error
        exampleDisposableResource.Dispose()
        
    return successOrError
   })

let app : WebPart = 
  let hub = new Broadcaster()
  choose [
    path "/websocket" >=> handShake (ws hub)
    path "/websocketWithSubprotocol" >=> handShakeWithSubprotocol (chooseSubprotocol "test") (ws hub)
    path "/websocketWithError" >=> handShake (wsWithErrorHandling hub)
    GET >=> choose [ path "/" >=> file "index.html"; browseHome ]
    NOT_FOUND "Found no handlers." ]

[<EntryPoint>]
let main _ =
  startWebServer defaultConfig app
  0

//
// The FIN byte:
//
// A single message can be sent separated by fragments. The FIN byte indicates the final fragment. Fragments
//
// As an example, this is valid code, and will send only one message to the client:
//
// do! webSocket.send Text firstPart false
// do! webSocket.send Continuation secondPart false
// do! webSocket.send Continuation thirdPart true
//
// More information on the WebSocket protocol can be found at: https://tools.ietf.org/html/rfc6455#page-34
//
