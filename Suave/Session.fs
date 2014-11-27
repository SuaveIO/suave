namespace Suave

module Session =

  open System
  open System.Collections.Concurrent
  open System.Security.Cryptography
  open System.Text
  open System.Runtime.Caching
  open Types
  open Http

  /// This is used to pack base64 in a cookie
  let internal base64 s =
    let base64 = Convert.ToBase64String(s)
    base64.Replace('+','-').Replace('/','_').Trim([| '=' |])

  /// Implements generic constant-time comparison vs timing attacks
  let slow_equals (a : byte array) (b : byte array) =
    let diff = ref ((uint32 a.Length) ^^^ (uint32 b.Length))
    let min = min a.Length b.Length
    for i = 0 to min - 1 do
      diff := !diff ||| (uint32 (a.[i] ^^^ b.[i]))
    !diff = 0ul

  /// Generates a cryptographically strong id of size id_size
  let internal strong_new_id =
    let rng = new RNGCryptoServiceProvider() //This type is thread safe.
    let chars = "abcdefghijklmnopqrstuvwuxyz0123456789"
    let charsLen = chars.Length
    fun id_size ->
      let random = Array.create<byte> id_size 0uy
      rng.GetBytes(random)
      let result = new StringBuilder(id_size)
      random |> Array.iter (fun (b : byte) -> result.Append chars.[int b % charsLen] |> ignore )
      result.ToString()

  /// Calculates an HMAC to verify authenticity of the cookie and prevents against session replay
  let hmac (id : string) (remote_ip : string) (key : string) =
    let builder = new StringBuilder(id, 512)
    builder.Append remote_ip  |> ignore
    use hmac = new HMACSHA256(Encoding.UTF8.GetBytes(key))
    let hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(builder.ToString()))
    base64 hash

  let session_id_length = 40

  /// Validates sessionId was not tampered
  let validate (session_map : MemoryCache) key (sessionId : string) (ctx : HttpContext) =

    if sessionId.Length < session_id_length then
      false
    else
      let id  = sessionId.Substring( 0, session_id_length)
      let mac = sessionId.Substring( session_id_length)

      let mac1 = hmac id (ctx.request.ipaddr.ToString()) key

      slow_equals (UTF8.bytes mac) (UTF8.bytes mac1) && session_map.Contains id

  let get_session (session_map : MemoryCache) sessionId =
    let state_bag = 
      lock session_map (fun _->
        if not (session_map.Contains sessionId) then
          failwith "invalid session id."
        session_map.[sessionId]) :?> ConcurrentDictionary<string, obj>
    let get = 
      fun s -> if state_bag.ContainsKey s then Some (state_bag.[s] :?> 'a) else None
    let set = fun s v -> state_bag.[s] <- v
    get, set

  /// The default session provider is an in-process session state provider
  type DefaultSessionProvider() =

    let key = strong_new_id 64

    let session_map = MemoryCache.Default

    interface ISessionProvider with

      member this.Generate(expiration : TimeSpan, ctx : HttpContext) =
        let session_id = strong_new_id session_id_length
        let dict = new ConcurrentDictionary<string, obj> ()
        lock session_map (fun _ ->
          session_map.Add(session_id, dict :> obj,
                          Globals.utc_now().Add expiration)
          |> ignore)
        let id = session_id + hmac session_id  (ctx.request.ipaddr.ToString()) key
        id

      member this.Validate(s : string, ctx : HttpContext) =
        validate session_map key s ctx

      member this.Session(s : string)  =
        get_session session_map  (s.Substring(0, session_id_length))

  /// Cookie-based session support
  let session_support (time_span : TimeSpan) = fun (ctx : HttpContext) -> async {

    let request = ctx.request
    let cookies = Parsing.get_cookies request.headers
    let session_id =
      match cookies ? suave_session_id with
      | Some attr ->
        if ctx.runtime.session_provider.Validate (attr, ctx) then
          attr
        else
          ctx.runtime.session_provider.Generate (time_span, ctx)
      | None ->
        ctx.runtime.session_provider.Generate (time_span, ctx)

    let cookie, ctx' =
      { HttpCookie.mk' "suave_session_id" session_id
        with http_only = true
             expires   = Some (Globals.utc_now().Add time_span) },
      { ctx with user_state = ctx.user_state |> Map.add "session_id" (box session_id) }

    return! Writers.set_cookie cookie ctx'
    }

  let session (ctx : HttpContext) : SessionStore<'a> =
    let sessionId = ctx.user_state.["session_id"] :?> string
    if String.IsNullOrEmpty sessionId then failwith "session_support was not called"
    else ctx.runtime.session_provider.Session sessionId
