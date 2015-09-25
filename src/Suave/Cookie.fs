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

  let parseCookies (s : string) : HttpCookie list =
    s.Split([|';'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map (fun (cookie : string) ->
        let parts = cookie.Split([|'='|], StringSplitOptions.RemoveEmptyEntries)
        if parts.Length > 1 then
          Some (HttpCookie.mkKV (String.trim parts.[0]) (String.trim parts.[1]))
        else
          None)
    |> List.choose id

  let parseResultCookie (s : string) : HttpCookie =
    let parseExpires (str : string) =
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
        | "Expires", expires        -> iter + 1, { cookie with expires = Some (parseExpires expires) }
        | "HttpOnly", _             -> iter + 1, { cookie with httpOnly = true }
        | "Secure", _               -> iter + 1, { cookie with secure = true }
        | _                         -> iter + 1, cookie)
        (0, { HttpCookie.empty with httpOnly = false }) // default when parsing
    |> snd

  type HttpRequest with

    member x.cookies =
      x.headers
      |> List.filter (fun (name, _) -> name.Equals "cookie")
      |> List.collect (snd >> parseCookies)
      |> List.fold (fun cookies cookie ->
          cookies |> Map.add cookie.name cookie)
          Map.empty

  type HttpResult with

    member x.cookies =
      x.headers
      |> List.filter (fst >> (String.eqOrdCi "Set-Cookie"))
      /// duplicate headers are comma separated
      |> List.collect (snd >> String.split ',' >> List.map String.trim)
      |> List.map parseResultCookie
      |> List.fold (fun cookies cookie ->
          cookies |> Map.add cookie.name cookie)
          Map.empty

  let private clientCookieFrom (httpCookie : HttpCookie) =
    let ccn = String.Concat [ httpCookie.name; "-client" ]
    { HttpCookie.mkKV ccn httpCookie.name
        with httpOnly = false
             secure    = httpCookie.secure
             expires   = httpCookie.expires }

  /// Set +relativeExpiry time span on the expiry time of the http cookie
  /// and generate a corresponding client-side cookie with the same expiry, that
  /// has as its data, the cookie name of the http cookie.
  let private slidingExpiry (relativeExpiry : CookieLife) (httpCookie : HttpCookie) =
    let cookieName = httpCookie.name
    let expiry =
      match relativeExpiry with
      | Session -> None
      | MaxAge ts  -> Some (Globals.utcNow().Add ts)
    let httpCookie = { httpCookie with expires = expiry }
    httpCookie, clientCookieFrom httpCookie

  let setCookie (cookie : HttpCookie) (ctx : HttpContext) =
    let notSetCookie : string * string -> bool =
      fst >> (String.eqOrdCi "Set-Cookie" >> not)
    let cookieHeaders =
      ctx.response.cookies
      |> Map.put cookie.name cookie // possibly overwrite
      |> Map.toList
      |> List.map snd // get HttpCookie-s
      |> List.map HttpCookie.toHeader
    let headers' =
      cookieHeaders
      |> List.fold (fun headers header ->
          ("Set-Cookie", header) :: headers)
          (ctx.response.headers |> List.filter notSetCookie)
    { ctx with response = { ctx.response with headers = headers' } }
    |> succeed

  let unsetCookie (cookieName : string) =
    let startEpoch = DateTimeOffset(1970, 1, 1, 0, 0, 1, TimeSpan.Zero) |> Some
    let stringValue = HttpCookie.toHeader { HttpCookie.mkKV cookieName "x" with expires = startEpoch }
    Writers.setHeader "Set-Cookie" stringValue

  let setPair (httpCookie : HttpCookie) (clientCookie : HttpCookie) : WebPart =
    context (fun { runtime = { logger = logger } } ->
      Log.log logger "Suave.Cookie.setPair" LogLevel.Debug
        (sprintf "setting cookie '%s' len '%d'" httpCookie.name httpCookie.value.Length)
      succeed)
    >>= setCookie httpCookie >>= setCookie clientCookie

  let unsetPair httpCookieName : WebPart =
    unsetCookie httpCookieName >>= unsetCookie (String.Concat [ httpCookieName; "-client" ])

  type CookiesState =
    { serverKey      : ServerKey
      cookieName     : string
      userStateKey   : string
      relativeExpiry : CookieLife
      secure         : bool }

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CookiesState =

    let mk serverKey cookieName userStateKey relativeExpiry secure =
      { serverKey      = serverKey
        cookieName     = cookieName
        userStateKey   = userStateKey
        relativeExpiry = relativeExpiry
        secure         = secure }

  let generateCookies serverKey cookieName relativeExpiry secure plainData =
    let enc, _ = Bytes.cookieEncoding
    match Crypto.secretbox serverKey plainData with
    | Choice1Of2 cookieData ->
      let encodedData = enc cookieData
      { HttpCookie.mkKV cookieName encodedData
          with httpOnly = true
               secure    = secure }
      |> slidingExpiry relativeExpiry
    | err -> failwithf "internal error on encryption %A" err

  let readCookies key cookieName cookies =
    let _, dec = Bytes.cookieEncoding
    let found =
      cookies
      |> Map.tryFind cookieName
      |> Choice.ofOption (NoCookieFound cookieName)
      |> Choice.map (fun c -> c, c.value |> dec)
    match found with
    | Choice1Of2 (cookie, cipherData) ->
      cipherData
      |> Crypto.secretboxOpen key
      |> Choice.mapError DecryptionError
      |> Choice.map (fun plainText -> cookie, plainText)
    | Choice2Of2 x -> Choice2Of2 x

  let refreshCookies relativeExpiry httpCookie : WebPart =
    slidingExpiry relativeExpiry httpCookie ||> setPair

  let updateCookies (csctx : CookiesState) fPlainText : WebPart =
    context (fun ctx ->
      let logger = ctx.runtime.logger
      let plainText =
        match readCookies csctx.serverKey csctx.cookieName ctx.response.cookies with
        | Choice1Of2 (_, plainText) ->
          Log.log logger "Suave.Cookie.updateCookies" LogLevel.Debug "updateCookies - existing"
          fPlainText (Some plainText)
        | Choice2Of2 _ ->
          Log.log logger "Suave.Cookie.updateCookies" LogLevel.Debug "updateCookies - first time"
          fPlainText None

      /// Since the contents will completely change every write, we simply re-generate the cookie
      generateCookies csctx.serverKey csctx.cookieName
                       csctx.relativeExpiry csctx.secure
                       plainText
      ||> setPair
      >>= Writers.setUserData csctx.userStateKey plainText)

  let cookieState (csctx : CookiesState)
                   // unit -> plain text to store OR something to run of your own!
                   (noCookie : unit -> Choice<byte [], WebPart>)
                   (decryptionFailure   : _ -> Choice<byte [], WebPart>)
                   (fSuccess : WebPart)
                   : WebPart =
    context (fun ctx ->

      let log = Log.log ctx.runtime.logger "Suave.Cookie.cookieState" LogLevel.Debug

      let setCookies plainText =
        let httpCookie, clientCookie =
          generateCookies csctx.serverKey csctx.cookieName
                           csctx.relativeExpiry csctx.secure
                           plainText
        setPair httpCookie clientCookie >>=
          Writers.setUserData csctx.userStateKey plainText

      match readCookies csctx.serverKey csctx.cookieName ctx.request.cookies with
      | Choice1Of2 (httpCookie, plainText) ->
        log "existing cookie"
        refreshCookies csctx.relativeExpiry httpCookie
          >>= Writers.setUserData csctx.userStateKey plainText
          >>= fSuccess

      | Choice2Of2 (NoCookieFound _) ->
        match noCookie () with
        | Choice1Of2 plainText ->
          log "no existing cookie, setting text"
          setCookies plainText >>= fSuccess
        | Choice2Of2 wp_kont ->
          log "no existing cookie, calling app continuation"
          wp_kont

      | Choice2Of2 (DecryptionError err) ->
        log (sprintf "decryption error: %A" err)
        match decryptionFailure err with
        | Choice1Of2 plainText ->
          log "existing, broken cookie, setting cookie text anew"
          setCookies plainText >>= fSuccess
        | Choice2Of2 wpKont    ->
          log "existing, broken cookie, unsetting it, forwarding to given failure web part"
          wpKont >>= unsetPair csctx.cookieName)