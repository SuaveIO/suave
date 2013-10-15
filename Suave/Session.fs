module Suave.Session

open System
open Types
open Http

/// Cookie-based session support
let session_support (request : HttpRequest) =
  //sessions could expire, we need a timestamp .. to know when the session expires
  //we will probably also need a job that monitors the session and deletes the expired ones
  //to sessions survive restart they could be file based like php.. that would be kind of slow
  //lookup the session id in the cookies
  let sessionId =
    match request.Cookies ? suave_session_id with
    | Some(attr) -> snd(attr.[0])
    | None -> Guid.NewGuid().ToString()

  request.SessionId <- sessionId
  set_cookie (sprintf "%s=%s" "suave_session_id" sessionId) request |> ignore
  Some request

open System.Collections.Generic
open System.Collections.Concurrent

/// Static dictionary of sessions
let session_map = new ConcurrentDictionary<string,ConcurrentDictionary<string,obj>>()

/// Get the session from the HttpRequest
let session (request : HttpRequest) =
  let sessionId = request.SessionId
  if String.IsNullOrEmpty sessionId then failwith "session_support was not called"
  if not (session_map.ContainsKey sessionId) then
    session_map.TryAdd(sessionId,new ConcurrentDictionary<string,obj>()) |> ignore
  session_map.[sessionId]
