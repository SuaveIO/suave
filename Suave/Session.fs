module Suave.Session

open System
open System.Collections.Concurrent
open System.Security.Cryptography
open System.Text
open System.Runtime.Caching
open Types
open Http

[<Literal>]
let ServerKeyLength = 64

[<Literal>]
let SessionIdLength = 40

[<Literal>]
let SessionHttpCookie = "suave_session_id"

/// This is the cookie that the client-side can use to check whether the
/// user is logged on or not. It's only a marker, which is used for this
/// purpose; the real auth cookie is the SessionHttpCookie, which isn't
/// available to client-side.
[<Literal>]
let SessionJsCookie = "suave_session_enabled"

/// The Id used in `context.user_state` to save the session id for downstream
/// web parts.
[<Literal>]
let SessionIdKey = "suave_session_id"

module internal Utils =
  /// This is used to pack base64 in a cookie; generates a degenerated base64 string
  let base64_headers bytes =
    let base64 = Convert.ToBase64String bytes
    base64.Replace('+','-').Replace('/','_').Trim([| '=' |])

  /// Extracts the actual session id and the mac value from the cookie's data.
  let split_session_id (si : string) =
    let id  = si.Substring(0, SessionIdLength)
    let mac = si.Substring(SessionIdLength)
    id, mac

  /// Returns a list of the hmac data to use, from the request.
  let hmac_data id (request : HttpRequest) =
    [ yield id
      yield request.ipaddr.ToString()
      yield match request.headers %% "user-agent" with
            | None -> ""
            | Some ua -> ua
    ]

/// Validates session_id was not tampered
let validate (session_map : MemoryCache) key (session_id : string) (ctx : HttpContext) =
  if session_id.Length < SessionIdLength then
    false
  else
    let id, mac_given = Utils.split_session_id session_id
    let mac_calc = Utils.hmac_data id ctx.request |> Crypto.hmac' key |> Utils.base64_headers
    String.eq_time_cmp_ord mac_given mac_calc && session_map.Contains id

/// TODO: same as HttpContext.session below??
let get_session (session_map : MemoryCache) session_id =
  let state_bag = 
    lock session_map (fun _->
      if not (session_map.Contains session_id) then
        failwith "invalid session id."
      session_map.[session_id]) :?> ConcurrentDictionary<string, obj>
  let get = 
    fun s -> if state_bag.ContainsKey s then Some (state_bag.[s] :?> 'a) else None
  let set = fun s v -> state_bag.[s] <- v
  get, set

/// The default session provider is an in-process session state provider
type DefaultSessionProvider() =

  let key = Crypto.generate_key' ServerKeyLength
  let session_map = MemoryCache.Default

  interface ISessionProvider with
    member this.Generate(expiration : TimeSpan, ctx : HttpContext) =
      let session_id = Crypto.generate_key' SessionIdLength
      let dict = new ConcurrentDictionary<string, obj> ()
      lock session_map (fun _ ->
        session_map.Add(session_id, dict :> obj,
                        Globals.utc_now().Add expiration)
        |> ignore)
      let hmac_data = Utils.hmac_data session_id ctx.request
      sprintf "%s%s" session_id (Crypto.hmac' key hmac_data |> Utils.base64_headers)

    member this.Validate(s : string, ctx : HttpContext) =
      validate session_map key s ctx

    member this.Session(s : string)  =
      get_session session_map (s.Substring(0, SessionIdLength))

/// Cookie-based session support
let session_support (time_span : TimeSpan) =
  context (fun ({ request = req; runtime = { session_provider = sp } } as ctx) ->
    let cookies = Parsing.get_cookies req.headers
    let session_id =
      match look_up cookies SessionIdKey with
      | Some attr ->
        if ctx.runtime.session_provider.Validate (attr, ctx) then
          attr
        else
          ctx.runtime.session_provider.Generate (time_span, ctx)
      | None ->
        ctx.runtime.session_provider.Generate (time_span, ctx)

    let http_cookie, js_cookie =
      let expiry = Some (Globals.utc_now().Add time_span)
      { HttpCookie.mk' SessionHttpCookie session_id with expires = expiry; http_only = true },
      { HttpCookie.mk' SessionJsCookie   "true"     with expires = expiry; http_only = false }

    Writers.set_cookie http_cookie >>=
      Writers.set_cookie js_cookie >>=
      Writers.set_user_data SessionIdKey (box session_id))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpContext =

  let session (ctx : HttpContext) : SessionStore<'a> =
    let sessionId = ctx.user_state.[SessionIdKey] :?> string
    if String.IsNullOrEmpty sessionId then failwith "session_support was not called"
    else ctx.runtime.session_provider.Session sessionId