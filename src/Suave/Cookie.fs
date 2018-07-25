namespace Suave

module Cookie =

  open System
  open System.Text
  open System.Globalization
  open Suave.Operators
  open Suave.Logging
  open Suave.Logging.Message
  open Suave.Utils

  type CookieLife =
    | Session
    | MaxAge of duration:TimeSpan

  type CookieError =
    | NoCookieFound of cookieName:string
    | DecryptionError of error:Crypto.SecretboxDecryptionError

  let parseCookies (s : string) : HttpCookie list =
    s.Split([|';'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map (String.trim)
    |> List.map (fun (cookie : string) ->
        match cookie.IndexOf("=") with
        | idx when idx + 1 = cookie.Length ->
          // no value is set
          None

        | idx when idx > 1 ->
          let name = String.trim (cookie.Substring(0, idx))
          let value = String.trim (cookie.Substring(idx + 1))
          Some (HttpCookie.createKV name value)

        | _ ->
          None)
    |> List.choose id

  let parseResultCookie (s : string) : HttpCookie =
    let parseExpires (str : string) =
      DateTimeOffset.ParseExact(str, "R", CultureInfo.InvariantCulture)
    let parseSameSite (str : string) =
      match str with
      | "Strict" -> Some Strict
      | "Lax" -> Some Lax
      | _ -> None
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
        | "SameSite", sameSite      -> iter + 1, { cookie with sameSite = parseSameSite sameSite}
        | _                         -> iter + 1, cookie)
        (0, { HttpCookie.empty with httpOnly = false }) // default when parsing
    |> snd

  type HttpRequest with

    /// Get yourself a dictionary of cookie-name to Cookie.
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
      |> List.filter (fst >> (String.equalsOrdinalCI Headers.Fields.Response.setCookie))
      |> List.map (snd >> String.trim)
      |> List.map parseResultCookie
      |> List.fold (fun cookies cookie ->
          cookies |> Map.add cookie.name cookie)
          Map.empty

  let private clientCookieFrom (httpCookie : HttpCookie) =
    let ccn = String.Concat [ httpCookie.name; "-client" ]
    { HttpCookie.createKV ccn httpCookie.name
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
      fst >> (String.equalsOrdinalCI Headers.Fields.Response.setCookie >> not)

    let cookieHeaders =
      ctx.response.cookies
      |> Map.add cookie.name cookie // possibly overwrite
      |> Map.toList
      |> List.map snd // get HttpCookie-s
      |> List.map HttpCookie.toHeader

    let headers' =
      cookieHeaders
      |> List.fold (fun headers header ->
          (Headers.Fields.Response.setCookie, header) :: headers)
          (ctx.response.headers |> List.filter notSetCookie)

    if cookie.value.Length > 4096 then
      ctx.runtime.logger.warn (
        eventX "Cookie {cookieName} has {cookieBytes} which is too large! Lengths over 4 096 bytes risk corruption in some browsers; consider alternate storage"
        >> setFieldValue "cookieName" cookie.name
        >> setFieldValue "cookieBytes" cookie.value.Length)


    succeed { ctx with response = { ctx.response with headers = headers' } }
      

  let unsetCookie (cookieName : string) =
    let startEpoch = DateTimeOffset(1970, 1, 1, 0, 0, 1, TimeSpan.Zero) |> Some
    let stringValue = HttpCookie.toHeader { HttpCookie.createKV cookieName "x" with expires = startEpoch }
    Writers.addHeader Headers.Fields.Response.setCookie stringValue

  let setPair (httpCookie : HttpCookie) (clientCookie : HttpCookie) : WebPart =
    context (fun ctx ->
      ctx.runtime.logger.debug (
        eventX "Setting {cookieName} to value of {cookieBytes}"
        >> setFieldValue "cookieName" httpCookie.name
        >> setFieldValue "cookieBytes" httpCookie.value.Length
        >> setSingleName "Suave.Cookie.setPair")

      succeed)
    >=> setCookie httpCookie
    >=> setCookie clientCookie

  let unsetPair httpCookieName : WebPart =
    unsetCookie httpCookieName >=> unsetCookie (String.Concat [ httpCookieName; "-client" ])

  type CookiesState =
    { serverKey      : ServerKey
      cookieName     : string
      userStateKey   : string
      relativeExpiry : CookieLife
      secure         : bool }

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CookiesState =

    let create serverKey cookieName userStateKey relativeExpiry secure =
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
      { HttpCookie.createKV cookieName encodedData
          with httpOnly = true
               secure   = secure }
      |> slidingExpiry relativeExpiry

    | Choice2Of2 err ->
      failwithf "Suave internal error on encryption %A" err

  /// Tries to read the cookie by `cookieName` from the mapping of cookie-name
  /// to cookie. If it exists, it is decrypted with the `cryptoKey`.
  let readCookies cryptoKey cookieName (cookies : Map<string, HttpCookie>) =
    let _, dec = Bytes.cookieEncoding
    let found =
      cookies
      |> Map.tryFind cookieName
      |> Choice.ofOption (NoCookieFound cookieName)
      |> Choice.map (fun c -> c, c.value |> dec)

    match found with
    | Choice1Of2 (cookie, cipherData) ->
      cipherData
      |> Crypto.secretboxOpen cryptoKey
      |> Choice.mapSnd DecryptionError
      |> Choice.map (fun plainText -> cookie, plainText)

    | Choice2Of2 x ->
      Choice2Of2 x

  let refreshCookies relativeExpiry httpCookie : WebPart =
    slidingExpiry relativeExpiry httpCookie ||> setPair

  let updateCookies (csctx : CookiesState) fPlainText : WebPart =
    context (fun ctx ->
      let debug message =
        ctx.runtime.logger.debug (
          eventX message
          >> setSingleName "Suave.Cookie.updateCookies")

      let plainText =
        match readCookies csctx.serverKey csctx.cookieName ctx.response.cookies with
        | Choice1Of2 (_, plainText) ->
          debug "Existing cookie"
          fPlainText (Some plainText)

        | Choice2Of2 _ ->
          debug "First time"
          fPlainText None

      /// Since the contents will completely change every write, we simply re-generate the cookie
      generateCookies csctx.serverKey csctx.cookieName
                       csctx.relativeExpiry csctx.secure
                       plainText
      ||> setPair
      >=> Writers.setUserData csctx.userStateKey plainText)

  let cookieState (csctx : CookiesState)
                   // unit -> plain text to store OR something to run of your own!
                   (noCookie : unit -> Choice<byte [], WebPart>)
                   (decryptionFailure   : _ -> Choice<byte [], WebPart>)
                   (fSuccess : WebPart)
                   : WebPart =
    context (fun ctx ->
      let debug message =
        ctx.runtime.logger.debug (
          eventX message
          >> setSingleName "Suave.Cookie.cookieState")

      let setCookies plainText =
        let httpCookie, clientCookie =
          generateCookies csctx.serverKey csctx.cookieName
                          csctx.relativeExpiry csctx.secure
                          plainText

        setPair httpCookie clientCookie
          >=> Writers.setUserData csctx.userStateKey plainText

      match readCookies csctx.serverKey csctx.cookieName ctx.request.cookies with
      | Choice1Of2 (httpCookie, plainText) ->
        debug "Existing cookie"
        refreshCookies csctx.relativeExpiry httpCookie
          >=> Writers.setUserData csctx.userStateKey plainText
          >=> fSuccess

      | Choice2Of2 (NoCookieFound _) ->
        match noCookie () with
        | Choice1Of2 plainText ->
          debug "No existing cookie, setting text"
          setCookies plainText >=> fSuccess

        | Choice2Of2 wp_kont ->
          debug "No existing cookie, calling WebPart continuation"
          wp_kont

      | Choice2Of2 (DecryptionError err) ->
        debug (sprintf "decryption error: %A" err)
        match decryptionFailure err with
        | Choice1Of2 plainText ->
          debug "Existing, broken cookie, setting cookie text anew"
          setCookies plainText >=> fSuccess
        | Choice2Of2 wpKont    ->
          debug "Existing, broken cookie, unsetting it, forwarding to given failure web part"
          wpKont >=> unsetPair csctx.cookieName)
