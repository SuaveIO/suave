namespace Suave

module Cookie =

  open System
  open System.Text
  open System.Globalization

  open Suave
  open Suave.Types
  open Suave.Http
  open Suave.Logging
  open Suave.Utils

  type CookieLife =
    | Session
    | MaxAge of TimeSpan

  type CookieError =
    | NoCookieFound of string
    | DecryptionError of Crypto.SecretboxDecryptionError

  let parse_cookies (s : string) : HttpCookie list =
    s.Split(';')
    |> Array.toList
    |> List.map (fun (cookie : string) ->
        let parts = cookie.Split('=')
        HttpCookie.mk' (String.trim parts.[0]) (String.trim parts.[1]))

  let parse_result_cookie (s : string) : HttpCookie =
    let parse_expires (str : string) =
      DateTimeOffset.ParseExact(str, "R", CultureInfo.InvariantCulture)
    s.Split(';')
    |> Array.map (fun (x : string) ->
        let parts = x.Split('=')
        if parts.Length > 1 then
          parts.[0].Trim(), parts.[1].Trim()
        else
          parts.[0].Trim(), "")
    |> Array.fold (fun (iter, (cookie : HttpCookie)) -> function
        | name, value when iter = 0 -> iter + 1, { cookie with name = name
                                                               value = value }
        | "Domain", domain          -> iter + 1, { cookie with domain = Some domain }
        | "Path", path              -> iter + 1, { cookie with path = Some path }
        | "Expires", expires        -> iter + 1, { cookie with expires = Some (parse_expires expires) }
        | "HttpOnly", _             -> iter + 1, { cookie with http_only = true }
        | "Secure", _               -> iter + 1, { cookie with secure = true }
        | _                         -> iter + 1, cookie)
        (0, { HttpCookie.empty with http_only = false }) // default when parsing
    |> snd

  module HttpRequest =

    let cookies (x : HttpRequest) =
      x.headers
      |> List.filter (fun (name, _) -> name.Equals "cookie")
      |> List.flat_map (snd >> parse_cookies)
      |> List.fold (fun cookies cookie ->
          cookies |> Map.add cookie.name cookie)
          Map.empty

  module HttpResult =

    let cookies (x : HttpResult) =
      x.headers
      |> List.filter (fst >> (String.eq_ord_ci "Set-Cookie"))
      /// duplicate headers are comma separated
      |> List.flat_map (snd >> String.split ',' >> List.map String.trim)
      |> List.map parse_result_cookie
      |> List.fold (fun cookies cookie ->
          cookies |> Map.add cookie.name cookie)
          Map.empty

  let private client_cookie_from (http_cookie : HttpCookie) =
    let ccn = String.Concat [ http_cookie.name; "-client" ]
    { HttpCookie.mk' ccn http_cookie.name
        with http_only = false
             secure    = http_cookie.secure
             expires   = http_cookie.expires }

  /// Set +relative_expiry time span on the expiry time of the http cookie
  /// and generate a corresponding client-side cookie with the same expiry, that
  /// has as its data, the cookie name of the http cookie.
  let private sliding_expiry (relative_expiry : CookieLife) (http_cookie : HttpCookie) =
    let cookie_name = http_cookie.name
    let expiry =
      match relative_expiry with
      | Session -> None
      | MaxAge ts  -> Some (Globals.utc_now().Add ts)
    let http_cookie = { http_cookie with expires = expiry }
    http_cookie, client_cookie_from http_cookie

  let set_cookie (cookie : HttpCookie) (ctx : HttpContext) =
    let not_set_cookie : string * string -> bool =
      fst >> (String.eq_ord_ci "Set-Cookie" >> not)
    let cookie_headers =
      ctx.response
      |> HttpResult.cookies // get current cookies
      |> Map.put cookie.name cookie // possibly overwrite
      |> Map.toList
      |> List.map snd // get HttpCookie-s
      |> List.map HttpCookie.to_header
    let headers' =
      cookie_headers
      |> List.fold (fun headers header ->
          ("Set-Cookie", header) :: headers)
          (ctx.response.headers |> List.filter not_set_cookie)
    { ctx with response = { ctx.response with headers = headers' } }
    |> succeed

  let unset_cookie (cookie_name : string) =
    let start_epoch = DateTimeOffset(1970, 1, 1, 0, 0, 1, TimeSpan.Zero) |> Some
    let string_value = HttpCookie.to_header { HttpCookie.mk' cookie_name "x" with expires = start_epoch }
    Writers.setHeader "Set-Cookie" string_value

  let set_pair (http_cookie : HttpCookie) (client_cookie : HttpCookie) : HttpPart =
    context (fun { runtime = { logger = logger } } ->
      Log.log logger "Suave.Cookie.set_pair" LogLevel.Debug
        (sprintf "setting cookie '%s' len '%d'" http_cookie.name http_cookie.value.Length)
      succeed)
    >>= set_cookie http_cookie >>= set_cookie client_cookie

  let unset_pair http_cookie_name : HttpPart =
    unset_cookie http_cookie_name >>= unset_cookie (String.Concat [ http_cookie_name; "-client" ])

  type CookiesState =
    { server_key      : ServerKey
      cookie_name     : string
      user_state_key  : string
      relative_expiry : CookieLife
      secure          : bool }

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CookiesState =

    let mk server_key cookie_name user_state_key relative_expiry secure =
      { server_key      = server_key
        cookie_name     = cookie_name
        user_state_key  = user_state_key
        relative_expiry = relative_expiry
        secure          = secure }

  let generate_cookies server_key cookie_name relative_expiry secure plain_data =
    let enc, _ = Bytes.cookieEncoding
    match Crypto.secretbox server_key plain_data with
    | Choice1Of2 cookie_data ->
      let encoded_data = enc cookie_data
      { HttpCookie.mk' cookie_name encoded_data
          with http_only = true
               secure    = secure }
      |> sliding_expiry relative_expiry
    | err -> failwithf "internal error on encryption %A" err

  let read_cookies key cookie_name cookies =
    let _, dec = Bytes.cookieEncoding
    let found =
      cookies
      |> Map.tryFind cookie_name
      |> Choice.from_option (NoCookieFound cookie_name)
      |> Choice.map (fun c -> c, c |> (HttpCookie.value >> dec))
    match found with
    | Choice1Of2 (cookie, cipher_data) ->
      cipher_data
      |> Crypto.secretboxOpen key
      |> Choice.map_2 DecryptionError
      |> Choice.map (fun plain_text -> cookie, plain_text)
    | Choice2Of2 x -> Choice2Of2 x

  let refresh_cookies relative_expiry http_cookie : HttpPart =
    sliding_expiry relative_expiry http_cookie ||> set_pair

  let update_cookies (csctx : CookiesState) f_plain_text : HttpPart =
    context (fun ({ runtime = { logger = logger }} as ctx) ->
      let plain_text' =
        match read_cookies csctx.server_key csctx.cookie_name (ctx.response |> HttpResult.cookies) with
        | Choice1Of2 (_, plain_text) ->
          Log.log logger "Suave.Cookie.update_cookies" LogLevel.Debug "update_cookies - existing"
          f_plain_text (Some plain_text)
        | Choice2Of2 _ ->
          Log.log logger "Suave.Cookie.update_cookies" LogLevel.Debug "update_cookies - first time"
          f_plain_text None

      /// Since the contents will completely change every write, we simply re-generate the cookie
      generate_cookies csctx.server_key csctx.cookie_name
                       csctx.relative_expiry csctx.secure
                       plain_text'
      ||> set_pair
      >>= Writers.setUserData csctx.user_state_key plain_text')

  let cookie_state (csctx : CookiesState)
                   // unit -> plain text to store OR something to run of your own!
                   (no_cookie : unit -> Choice<byte [], HttpPart>)
                   (decryption_failure   : _ -> Choice<byte [], HttpPart>)
                   (f_success : HttpPart)
                   : HttpPart =
    context (fun ({ runtime = { logger = logger }} as ctx) ->

      let log = Log.log logger "Suave.Cookie.cookie_state" LogLevel.Debug

      let set_cookies plain_text =
        let http_cookie, client_cookie =
          generate_cookies csctx.server_key csctx.cookie_name
                           csctx.relative_expiry csctx.secure
                           plain_text
        set_pair http_cookie client_cookie >>=
          Writers.setUserData csctx.user_state_key plain_text

      match read_cookies csctx.server_key csctx.cookie_name (ctx.request |> HttpRequest.cookies) with
      | Choice1Of2 (http_cookie, plain_text) ->
        log "existing cookie"
        refresh_cookies csctx.relative_expiry http_cookie
          >>= Writers.setUserData csctx.user_state_key plain_text
          >>= f_success

      | Choice2Of2 (NoCookieFound _) ->
        match no_cookie () with
        | Choice1Of2 plain_text ->
          log "no existing cookie, setting text"
          set_cookies plain_text >>= f_success
        | Choice2Of2 wp_kont ->
          log "no existing cookie, calling app continuation"
          wp_kont

      | Choice2Of2 (DecryptionError err) ->
        log (sprintf "decryption error: %A" err)
        match decryption_failure err with
        | Choice1Of2 plain_text ->
          log "existing, broken cookie, setting cookie text anew"
          set_cookies plain_text >>= f_success
        | Choice2Of2 wp_kont    ->
          log "existing, broken cookie, unsetting it, forwarding to given failure web part"
          wp_kont >>= unset_pair csctx.cookie_name)