namespace Suave

module Cookie =
  open System
  open System.Text
  open System.Globalization
  open Suave.Utils
  open Suave.Types

  type CookieLife =
    | Session
    | MaxAge of TimeSpan

  type CookieError =
    /// Gives you the cookie id
    | NoCookieFound of string
    | DecryptionError of Crypto.SecretboxDecryptionError

  /// Parse the cookie's name and data in the string into a dictionary.
  val parseCookies : cookieString:string -> HttpCookie list

  val parseResultCookie : cookieString:string -> HttpCookie

  type HttpRequest with

    /// Finds the cookies of the request, or an empty Map otherwise, if
    /// there are no cookies.
    member cookies : Map<string, HttpCookie>

  type HttpResult with

    member cookies : Map<string, HttpCookie>

  val setCookie : cookie:HttpCookie -> WebPart
  val unsetCookie : name:string -> WebPart

  /// Sets the cookies to the HttpResult
  val setPair   : httpCookie:HttpCookie -> clientCookie:HttpCookie -> WebPart
  val unsetPair : httpCookieName:string -> WebPart

  /// A DTO structure for passing the right parameters to the XXX_cookies functions
  /// in this module.
  type CookiesState =
    { serverKey      : ServerKey
      cookieName     : string
      userStateKey   : string
      relativeExpiry : CookieLife
      secure         : bool }

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CookiesState =

    val mk : serverKey:ServerKey ->
             cookieName:string ->
             userStateKey:string ->
             relativeExpiry:CookieLife ->
             secure:bool ->
             CookiesState


  /// Generate one server-side cookie, and another client-side cookie with
  /// name "${server-side-name}-client"
  val generateCookies : serverKey:ServerKey ->
                        cookieName:string ->
                        relativeExpiry:CookieLife ->
                        secure:bool ->
                        plainData:byte[] ->
                        HttpCookie * HttpCookie

  /// Tries to read the cookie of the given name from the HttpContext, and
  /// returns the cookie and its plaintext value if successful.
  val readCookies : key:ServerKey ->
                    cookieName:string ->
                    cookies:Map<string, HttpCookie> ->
                    Choice<HttpCookie * byte [], CookieError>

  /// Bumps the expiry dates for all the cookies.
  val refreshCookies : expiry:CookieLife ->
                       cookie:HttpCookie ->
                       WebPart

  val updateCookies :  csctx:CookiesState ->
                       f_plain_text : (byte [] option -> byte []) ->
                       WebPart
  
  val cookieState : csctx:CookiesState ->
                    noCookie :(unit -> Choice<byte [], WebPart>) ->
                    decryptionFailure:(Crypto.SecretboxDecryptionError -> Choice<byte [], WebPart>) ->
                    f_success:WebPart ->
                    WebPart


  /// Obsolete
  [<Obsolete("Renamed to parseCookies'")>]
  val parse_cookies : cookieString:string -> HttpCookie list
  /// Obsolete
  [<Obsolete("Renamed to parseResultCookie'")>]
  val parse_result_cookie : cookieString:string -> HttpCookie

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpRequest =
  /// Obsolete
    [<Obsolete("Use the .cookies property instead'")>]
    val cookies : HttpRequest -> Map<string, HttpCookie>

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpResult =
  /// Obsolete
    [<Obsolete("Use the .cookies property instead'")>]
    val cookies : HttpResult -> Map<string, HttpCookie>

  /// Obsolete
  [<Obsolete("Renamed to setCookie'")>]
  val set_cookie : cookie:HttpCookie -> WebPart
  /// Obsolete
  [<Obsolete("Renamed to unsetCookie'")>]
  val unset_cookie : name:string -> WebPart
  /// Obsolete
  [<Obsolete("Renamed to setPair'")>]
  val set_pair   : httpCookie:HttpCookie -> clientCookie:HttpCookie -> WebPart
  /// Obsolete
  [<Obsolete("Renamed to unsetPair'")>]
  val unset_pair : httpCookieName:string -> WebPart
  /// Obsolete
  [<Obsolete("Renamed to generateCookies'")>]
  val generate_cookies : serverKey:ServerKey -> cookieName:string -> relativeExpiry:CookieLife -> secure:bool -> plainData:byte[] -> HttpCookie * HttpCookie
  /// Obsolete
  [<Obsolete("Renamed to readCookies'")>]
  val read_cookies : key:ServerKey -> cookieName:string -> cookies:Map<string, HttpCookie> -> Choice<HttpCookie * byte [], CookieError>
  /// Obsolete
  [<Obsolete("Renamed to refreshCookies'")>]
  val refresh_cookies : expiry:CookieLife -> cookie:HttpCookie -> WebPart
  /// Obsolete
  [<Obsolete("Renamed to updateCookies'")>]
  val update_cookies :  csctx:CookiesState -> f_plain_text : (byte [] option -> byte []) -> WebPart
  /// Obsolete
  [<Obsolete("Renamed to cookieState'")>]
  val cookie_state : csctx:CookiesState -> noCookie :(unit -> Choice<byte [], WebPart>) -> decryptionFailure:(Crypto.SecretboxDecryptionError -> Choice<byte [], WebPart>) -> f_success:WebPart -> WebPart
