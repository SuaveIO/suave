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
  let hmac (id : string) (user_agent : string) (remote_ip : string) (key : string) =
    let builder = new StringBuilder(id, 512)
    builder.Append remote_ip  |> ignore
    builder.Append user_agent |> ignore
    use hmac = new HMACSHA1(Encoding.UTF8.GetBytes(key))
    let hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(builder.ToString()))
    base64 hash

  let session_id_lenght = 40

  /// Validates sessionId was not tampered
  let validate (session_map : MemoryCache) key (sessionId : string) (ctx : HttpContext) =

    if sessionId.Length < session_id_lenght then 
      false
    else
      let id  = sessionId.Substring( 0, session_id_lenght)
      let mac = sessionId.Substring( session_id_lenght)

      let mac1 = hmac id  (Option.get ctx.request.headers?``user-agent``) (ctx.connection.ipaddr.ToString()) key

      String.CompareOrdinal(mac, mac1) = 0 && session_map.Contains id

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
        let session_id = strong_new_id session_id_lenght 
        let dict = new ConcurrentDictionary<string, obj> ()
        dict.TryAdd ("_suave_session_time_stamp", DateTime.UtcNow) |> ignore
        lock session_map (fun _ -> session_map.Add(session_id, dict :> obj, Globals.utc_now().Add expiration) |> ignore)
        let id = session_id + hmac session_id (Option.get ctx.request.headers?``user-agent``) (ctx.connection.ipaddr.ToString()) key
        id

      member this.Validate(s : string, ctx : HttpContext) =
        validate session_map key s ctx

      member this.Session(s : string)  = get_session session_map  (s.Substring(0, session_id_lenght))

  /// Cookie-based session support
  let session_support (time_span : TimeSpan) = fun (ctx : HttpContext) ->

    let request = ctx.request

    let sessionId =
      match request.cookies ? suave_session_id with
      | Some(attr) ->
        let v = snd(attr.[0])
        if ctx.runtime.session_provider.Validate (v, ctx) then
          v
        else
          ctx.runtime.session_provider.Generate (time_span, ctx)
      | None ->
        ctx.runtime.session_provider.Generate (time_span, ctx)

    request.session_id <- sessionId
    Writers.set_cookie { name = "suave_session_id"
      ; value = sessionId
      ; path = Some "/"
      ; domain = None
      ; secure = false
      ; http_only = false
      ; expires = Some (Globals.utc_now().Add time_span)
      ; version = None } ctx |> Some

  let session (ctx : HttpContext) : SessionStore<'a> =
    let sessionId = ctx.request.session_id
    if String.IsNullOrEmpty sessionId then failwith "session_support was not called"
    else ctx.runtime.session_provider.Session sessionId
    
