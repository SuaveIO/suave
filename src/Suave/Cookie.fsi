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

  val setCookie : cookie:HttpCookie -> HttpPart
  val unsetCookie : name:string -> HttpPart

  /// Sets the cookies to the HttpResult
  val setPair   : httpCookie:HttpCookie -> clientCookie:HttpCookie -> HttpPart
  val unsetPair : httpCookieName:string -> HttpPart

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
                       HttpPart

  val updateCookies :  csctx:CookiesState ->
                       f_plain_text : (byte [] option -> byte []) ->
                       HttpPart
  
  val cookieState : csctx:CookiesState ->
                    noCookie :(unit -> Choice<byte [], HttpPart>) ->
                    decryptionFailure:(Crypto.SecretboxDecryptionError -> Choice<byte [], HttpPart>) ->
                    f_success:HttpPart ->
                    HttpPart