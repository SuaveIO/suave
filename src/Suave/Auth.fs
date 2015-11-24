module Suave.Auth

open System
open System.Text

open Suave.Cookie
open Suave.Logging
open Suave.Utils

module internal Utils =
  /// Generates a string key from the available characters with the given key size
  /// in characters. Note that this key is not cryptographically as random as a pure
  /// random number generator would produce as we only use a small subset alphabet.
  let generateReadableKey (keySize : int) =
    let arr = Array.zeroCreate<byte> keySize |> Crypto.randomize
    let alpha = "abcdefghijklmnopqrstuvwuxyz0123456789"
    let result = new StringBuilder(keySize)
    arr
    |> Array.iter (fun (b : byte) -> result.Append alpha.[int b % alpha.Length] |> ignore)
    result.ToString()

[<Literal>]
let SessionAuthCookie = "auth"

/// The key used in `context.user_state` to save the session id for downstream
/// web parts.
[<Literal>]
let StateStoreType = "Suave.Auth"

[<Literal>]
let SessionIdLength = 40

/// Extracts the actual session id and the mac value from the cookie's data.
let parseData (textBlob : string) =
  match textBlob.Split '\n' with
  | [| sessionId; ipAddress; userAgent |] ->
    sessionId
  | _ -> failwith "internal error; should not have successfully decrypted data"

/// Returns a list of the hmac data to use, from the request.
let generateData (ctx : HttpContext) =
  let sessionId = Utils.generateReadableKey SessionIdLength
  String.concat "\n"
    [ sessionId
      ctx.clientIpTrustProxy.ToString()
      ctx.request.header "user-agent" |> Choice.orDefault ""
    ]

let authenticate relativeExpiry secure
                 missingCookie
                 (decryptionFailure : Crypto.SecretboxDecryptionError -> Choice<byte [], WebPart>)
                 (fSuccess : WebPart)
                 : WebPart =
  context (fun ctx ->
    Log.log ctx.runtime.logger "Suave.Auth.authenticate" LogLevel.Debug "authenticating"

    cookieState
      { serverKey      = ctx.runtime.serverKey
        cookieName     = SessionAuthCookie
        userStateKey   = StateStoreType
        relativeExpiry = relativeExpiry
        secure         = secure }
      missingCookie
      decryptionFailure
      fSuccess)

let authenticateWithLogin relativeExpiry loginPage fSuccess : WebPart =
  authenticate relativeExpiry false
               (fun () -> Choice2Of2(Redirection.FOUND loginPage))
               (sprintf "%A" >> RequestErrors.BAD_REQUEST >> Choice2Of2)
               fSuccess

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
let authenticated relativeExpiry secure : WebPart =
  context (fun ctx ->
    let data = generateData ctx |> UTF8.bytes
    authenticate relativeExpiry secure
                 (fun _ -> Choice1Of2 data)
                 (fun _ -> Choice1Of2 data)
                 succeed)

//  let deauthenticate : WebPart =
//    Cookies.unset_cookies
  
module HttpContext =

  let sessionId x =
    x.userState
    |> Map.tryFind StateStoreType
    |> Option.map (fun x -> x :?> string |> parseData)