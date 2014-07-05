module CounterDemo

module private Helpers =
  let (<!>) a b =
    match a with
    | None -> b
    | Some x -> Some x

  let (<.>) a b =
    match a with
    | None -> b
    | Some x -> x

  /// Maybe convert to int32 from string
  let muint32 str =
    match System.UInt32.TryParse str with
    | true, i -> Some i
    | _       -> None

open Suave
open Suave.Socket
open Suave.Types
open Suave.Http
open Suave.Http.EventSource

open Helpers

let counter_demo (req : HttpRequest) (out : Connection) =

  let write i =
    socket {
      let msg = Message.Create(id = i, data = string i)
      do! msg |> send out
      return! async {
        do! Async.Sleep 100
        return Choice1Of2 () } }

  socket {
    let last_evt_id =
      (look_up req.headers "last-event-id" |> bind muint32) <!>
      (look_up req.query "lastEventId" |> bind muint32) <.>
      100u

    let actions =
      Seq.unfold
        (fun i -> if i = 0u then None else Some(write (string i), i-1u))
        (last_evt_id - 1u)

    for a in actions do
      do! a }