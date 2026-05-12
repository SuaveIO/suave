module Suave.Authentication

open System
open Suave
open Suave.Cookie
open Suave.RequestErrors
open Suave.Utils.Crypto

/// The key of the username placed in the userState map if present in the
/// request
val UserNameKey : string

/// <summary><para>
/// Perform basic authentication on the request, applying a predicate
/// to check the request for authentication tokens such as 'username'
/// and 'password'. Otherwise, if failing, challenge the client again.
/// </para><para>
/// </para><para>
/// </para></summary>
/// <remarks>
/// </remarks>
val authenticateBasic : f:(string * string -> bool) -> protectedPart:WebPart -> WebPart

/// <summary><para>
/// Perform basic authentication on the request, applying an asynchronous
/// predicate to check the request for authentication tokens such as 
/// 'username' and 'password'. Otherwise, if failing, challenge the client again.
/// </para><para>
/// </para><para>
/// </para></summary>
/// <remarks>
/// </remarks>
val authenticateBasicAsync : f:(string * string -> bool Async) -> protectedPart:WebPart -> WebPart

val SessionAuthCookie : string

/// The key used in `context.user_state` to save the session id for downstream
/// web parts.
val StateStoreType : string

val SessionIdLength : int

val authenticate : relativeExpiry:CookieLife
                 -> secure:bool
                 -> missingCookie:(unit -> Choice<byte[], WebPart>)
                 -> decryptionFailure:(SecretboxDecryptionError -> Choice<byte [], WebPart>)
                 -> fSuccess:WebPart
                 -> WebPart

val authenticateWithLogin : relativeExpiry:CookieLife
                          -> loginPage:string
                          -> fSuccess:WebPart
                          -> WebPart

/// Deauthenticates, or 'logs out' the user
val deauthenticate : WebPart

/// Deauthenticates the user and then sends them to path specified by loginPage string
val deauthenticateWithLogin : loginPage :string
                            -> WebPart

/// Set server-signed cookies to make the response contain a cookie
/// with a valid session id. It's worth having in mind that when you use this web
/// part, you're setting cookies on the response; so you'll need to have the
/// client re-send a request if you require authentication for it, after this
/// web part has run.
///
/// Parameters:
///  - `relativeExpiry`: how long does the authentication cookie last?
/// - `secure`: HttpsOnly?
///
/// Always succeeds.
val authenticated : relativeExpiry:CookieLife
                  -> secure:bool
                  -> WebPart

module HttpContext =

  /// Read the session id from the HttpContext
  val sessionId : ctx:HttpContext -> string option
