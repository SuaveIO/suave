namespace Suave

/// The cookie module is geared towards the server having the client store state
/// but the client not being able to read that state.
module Cookie =
  open System
  open System.Text
  open System.Globalization
  open Suave.Utils

  /// For how long should the cookie persist in the client's browser?
  type CookieLife =
    /// Until the browser session ends.
    | Session
    /// For the duration of the passed TimeSpan.
    | MaxAge of duration:TimeSpan

  /// There was an error reading or decrypting the cookie.
  type CookieError =
    /// No cookie was found by the given cookie name.
    | NoCookieFound of cookieName:string
    /// The cookie existed but Suave was unable to decrypt its contents. Have
    /// you the same key across servers and over time and deployments? Unless
    /// you've specified the server key in your configuration, you won't.
    | DecryptionError of error:Crypto.SecretboxDecryptionError

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

    val create : serverKey:ServerKey
               -> cookieName:string
               -> userStateKey:string
               -> relativeExpiry:CookieLife
               -> secure:bool -> CookiesState

  /// Generate one server-side cookie, and another client-side cookie with
  /// name "${server-side-name}-client"
  val generateCookies : serverKey:ServerKey
                      -> cookieName:string
                      -> relativeExpiry:CookieLife
                      -> secure:bool
                      -> plainData:byte[]
                      -> HttpCookie * HttpCookie

  /// Tries to read the cookie of the given name from the HttpContext, and
  /// returns the cookie and its plaintext value if successful.
  val readCookies : key:ServerKey
                  -> cookieName:string
                  -> cookies:Map<string, HttpCookie>
                  -> Choice<HttpCookie * byte [], CookieError>

  /// Bumps the expiry dates for all the cookies.
  val refreshCookies : expiry:CookieLife
                     -> cookie:HttpCookie
                     -> WebPart

  val updateCookies :  csctx:CookiesState ->
                       fPlainText : (byte [] option -> byte []) -> WebPart

  val cookieState : csctx:CookiesState
                  -> noCookie:(unit -> Choice<byte [], WebPart>)
                  -> decryptionFailure:(Crypto.SecretboxDecryptionError -> Choice<byte [], WebPart>)
                  -> fSuccess:WebPart
                  -> WebPart
