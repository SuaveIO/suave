module Suave.Session

open System
open Types
open Http

/// Cookie-based session support
let session_support (ctx : HttpContext) =
  let request = ctx.request
  //sessions could expire, we need a timestamp .. to know when the session expires
  //we will probably also need a job that monitors the session and deletes the expired ones
  //to sessions survive restart they could be file based like php.. that would be kind of slow
  //lookup the session id in the cookies
  let sessionId =
    match request.cookies ? suave_session_id with
    | Some(attr) -> snd(attr.[0])
    | None -> Guid.NewGuid().ToString()

  request.session_id <- sessionId
  Writers.set_cookie { name = "suave_session_id"
    ; value = sessionId
    ; path = Some "/"
    ; domain = None
    ; secure = true
    ; http_only = false
    ; expires = Some (Globals.utc_now().AddMinutes(30.0)) //cookie expires in 30 minutes
    ; version = None } ctx |> ignore
  Some ctx

open System.Collections.Concurrent

/// Get the session from the HttpRequest
/// WARNING!! Here be dragons; just a reference implementation - usage will tie your
/// code to a single server and if you load balance you will fail.
/// Use similar code in production, but with a proper backing store, such as memcached
/// or Riak to support stateless webs.
let session (request : HttpRequest) =
  let sessionId = request.session_id
  if String.IsNullOrEmpty sessionId then failwith "session_support was not called"
  if not (Globals.session_map.ContainsKey sessionId) then
    Globals.session_map.TryAdd(sessionId, new ConcurrentDictionary<string, obj>()) |> ignore
  Globals.session_map.[sessionId]
