module Suave.Auth

open System
open System.Text

open Suave
open Suave.Types
open Suave.Cookie
open Suave.Http
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
let generateData (request : HttpRequest) =
  let sessionId = Utils.generateReadableKey SessionIdLength
  String.concat "\n"
    [ sessionId
      request.ipaddr.ToString()
      request.header "user-agent" |> Option.orDefault ""
    ]

let authenticate relativeExpiry secure
                 missingCookie
                 (decryptionFailure : Crypto.SecretboxDecryptionError -> Choice<byte [], WebPart>)
                 (f_success : WebPart)
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
      f_success)

let authenticateWithLogin relativeExpiry loginPage f_success : WebPart =
  authenticate relativeExpiry false
               (fun () -> Choice2Of2(Redirection.FOUND loginPage))
               (sprintf "%A" >> RequestErrors.BAD_REQUEST >> Choice2Of2)
               f_success

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
  context (fun { request = req } ->
    let data = generateData req |> UTF8.bytes
    authenticate relativeExpiry secure
                 (fun _ -> Choice1Of2 data)
                 (fun _ -> Choice1Of2 data)
                 Suave.Http.succeed)

//  let deauthenticate : WebPart =
//    Cookies.unset_cookies
  
module HttpContext =

  let sessionId x =
    x.userState
    |> Map.tryFind StateStoreType
    |> Option.map (fun x -> x :?> string |> parseData)

  /// Obsolete
  [<Obsolete("Renamed to sessionId'")>]
  let session_id x = sessionId x

/// Obsolete
[<Obsolete("Renamed to parseData'")>]
let parse_data textBlob = parseData textBlob 
/// Obsolete
[<Obsolete("Renamed to generateData'")>]
let generate_data request = generateData request
/// Obsolete
[<Obsolete("Renamed to authenticateWithLogin'")>]
let authenticate' relativeExpiry login_page f_success = authenticateWithLogin relativeExpiry login_page f_success
