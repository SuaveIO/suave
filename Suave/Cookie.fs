module Suave.Cookie

open System
open System.Text
open System.Globalization

open Suave
open Suave.Types
open Suave.Http
open Suave.Log

type CookieLife =
  | Session
  | MaxAge of TimeSpan

type CookieError =
  | NoCookieFound of string (* cookie id *)
  | DecryptionError of Crypto.SecretboxDecryptionError

/// Parse the cookie's name and data in the string into a dictionary.
let parse_cookie (s : string) : HttpCookie =
  let parse_expires (str : string) =
    DateTimeOffset.ParseExact(str, "R", CultureInfo.InvariantCulture)
  s.Split(';')
  |> Array.map (fun (x : string) ->
      let parts = x.Split('=')
      if parts.Length > 1 then
        parts.[0].Trim(), parts.[1].Trim()
      else
        parts.[0], "")
  |> Array.fold (fun (iter, (cookie : HttpCookie)) -> function
      | name, value when iter = 0 -> iter + 1, { cookie with name = name; value = value }
      | "Domain", domain          -> iter + 1, { cookie with domain = Some domain }
      | "Path", path              -> iter + 1, { cookie with path = Some path }
      | "Expires", expires        -> iter + 1, { cookie with expires = Some (parse_expires expires) }
      | "HttpOnly", _             -> iter + 1, { cookie with http_only = true }
      | "Secure", _               -> iter + 1, { cookie with secure = true }
      | _                         -> iter + 1, cookie)
      (0, HttpCookie.empty)
  |> snd


module HttpRequest =

  /// Finds the cookies of the request, or an empty Map otherwise, if
  /// there are no cookies.
  let cookies (x : HttpRequest) =
    x.headers
    |> List.filter (fun (name, _) -> name.Equals "cookie")
    |> List.map (snd >> parse_cookie)
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

/// Unsets the cookies, thereby unauthenticating the user.
let internal unset_cookies http_cookie_name : WebPart =
  Writers.unset_cookie http_cookie_name >>=
    Writers.unset_cookie (String.Concat [ http_cookie_name; "-client" ])

/// Sets the cookies to the HttpResponse
let internal set_cookies (http_cookie : HttpCookie) (client_cookie : HttpCookie) : WebPart =
  Writers.set_cookie http_cookie >>=
    Writers.set_cookie client_cookie


/// A DTO structure for passing the right parameters to the XXX_cookies functions
/// in this module.
type CookiesState =
  { server_key      : byte []
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

/// Generate one server-side cookie, and another client-side cookie with
/// name "${server-side-name}-client"
let generate_cookies server_key cookie_name relative_expiry secure plain_data =
  let enc, _ = Bytes.cookie_encoding
  match Crypto.secretbox server_key plain_data with
  | Choice1Of2 cookie_data ->
    let encoded_data = enc cookie_data
    { HttpCookie.mk' cookie_name encoded_data
        with http_only = true
             secure    = secure }
    |> sliding_expiry relative_expiry
  | err -> failwithf "internal error on encryption %A" err

/// Tries to read the cookie of the given name from the HttpContext, and
/// returns the cookie and its plaintext value if successful.
let read_cookies key cookie_name ctx =
  let _, dec = Bytes.cookie_encoding
  let found =
    ctx.request
    |> HttpRequest.cookies
    |> Map.tryFind cookie_name
    |> Choice.from_option (NoCookieFound cookie_name)
    |> Choice.map (fun c -> c, c |> (HttpCookie.value >> dec))
  match found with
  | Choice1Of2 (cookie, cipher_data) ->
    cipher_data
    |> Crypto.secretbox_open key
    |> Choice.map_2 DecryptionError
    |> Choice.map (fun plain_text -> cookie, plain_text)
  | Choice2Of2 x -> Choice2Of2 x

/////////////// WEB PARTS ////////////////

/// Bumps the expiry dates for all the cookies.
let refresh_cookies relative_expiry http_cookie : WebPart =
  sliding_expiry relative_expiry http_cookie ||> set_cookies

let update_cookies (csctx : CookiesState) f_plain_text : WebPart =
  context (fun ({ runtime = { logger = logger }} as ctx) ->
    let plain_text' =
      match read_cookies csctx.server_key csctx.cookie_name ctx with
      | Choice1Of2 (_, plain_text) ->
        Log.log logger "Suave.Session.Cookies.update_cookies" Debug
          (sprintf "update_cookies - existing value: '%s'" (plain_text |> UTF8.to_string'))
        f_plain_text (Some plain_text)
      | Choice2Of2 _ ->
        Log.log logger "Suave.Session.Cookies.update_cookies" Debug "update_cookies - first time"
        f_plain_text None

    Log.log logger "Suave.Session.Cookies.update_cookies" Debug
      (sprintf "update_cookies - setting '%s'"
        (plain_text' |> UTF8.to_string'))
    /// Since the contents will completely change every write, we simply re-generate the cookie
    generate_cookies csctx.server_key csctx.cookie_name
                     csctx.relative_expiry csctx.secure
                     plain_text'
    ||> set_cookies
    >>= Writers.set_user_data csctx.user_state_key plain_text')

let cookie_state (csctx : CookiesState)
                 // unit -> plain text to store OR something to run of your own!
                 (no_cookie : unit -> Choice<byte [], WebPart>)
                 (decryption_failure   : _ -> WebPart)
                 : WebPart =
  context (fun ({ runtime = { logger = logger }} as ctx) ->
    match read_cookies csctx.server_key csctx.cookie_name ctx with
    | Choice1Of2 (http_cookie, plain_text) ->
      refresh_cookies csctx.relative_expiry http_cookie >>=
        Writers.set_user_data csctx.user_state_key plain_text

    | Choice2Of2 (NoCookieFound _) ->
      match no_cookie () with
      | Choice1Of2 plain_text ->
        Log.log logger "Suave.Session.Cookies.cookie_state" Debug
          (sprintf "setting '%s' with value '%s'" csctx.cookie_name (UTF8.to_string' plain_text))
        let http_cookie, client_cookie =
          generate_cookies csctx.server_key csctx.cookie_name
                           csctx.relative_expiry csctx.secure
                           plain_text
        set_cookies http_cookie client_cookie >>=
          Writers.set_user_data csctx.user_state_key plain_text
      | Choice2Of2 wp_kont -> wp_kont

    | Choice2Of2 (DecryptionError err) ->
      Log.log logger "Suave.Session.Cookies.cookie_state" Debug
        (sprintf "decryption error: %A" err)
      unset_cookies csctx.cookie_name >>=
        decryption_failure err)
