module CounterDemo

module private Helpers =
  let (<!>) a b =
    match a with
    | Choice1Of2 x -> Choice1Of2 x
    | Choice2Of2 _ -> b

  let (<.>) a b =
    match a with
    | Choice1Of2 x -> x
    | Choice2Of2 _ -> b

  /// Maybe convert to int32 from string
  let muint32 (str:string) =
    match System.UInt32.TryParse str with
    | true, i -> Choice1Of2 i
    | _       -> Choice2Of2 "couldn't convert to int32"

open Suave
open Suave.Sockets
open Suave.Sockets.Control
open Suave.Http
open Suave.EventSource
open Suave.Utils

open Helpers

let counterDemo (req : HttpRequest) (out : Connection) =

  let write i =
    socket {
      let msg = { id = i; data = string i; ``type`` = None }
      do! msg |> send out
      return! SocketOp.ofAsync (Async.Sleep 100)
    }

  socket {
    let lastEvtId =
      (req.header "last-event-id" |> Choice.bind muint32) <!>
      ((req.queryParam "lastEventId") |> Choice.bind muint32) <.>
      100u

    let actions =
      Seq.unfold
        (fun i -> if i = 0u then None else Some(write (string i), i-1u))
        (lastEvtId - 1u)

    for a in actions do
      do! a
    return out }