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
  let generate_readable_key (key_size : int) =
    let arr = Array.zeroCreate<byte> key_size |> Crypto.randomize
    let alpha = "abcdefghijklmnopqrstuvwuxyz0123456789"
    let result = new StringBuilder(key_size)
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
let parse_data (text_blob : string) =
  match text_blob.Split '\n' with
  | [| session_id; ip_address; user_agent |] ->
    session_id
  | _ -> failwith "internal error; should not have successfully decrypted data"

/// Returns a list of the hmac data to use, from the request.
let generate_data (request : HttpRequest) =
  let session_id = Utils.generate_readable_key SessionIdLength
  String.concat "\n"
    [ session_id
      request.ipaddr.ToString()
      request.headers %% "user-agent" |> Option.or_default ""
    ]

let authenticate relative_expiry secure
                 missing_cookie
                 (decryption_failure : Crypto.SecretboxDecryptionError -> Choice<byte [], HttpPart>)
                 (f_success : HttpPart)
                 : HttpPart =
  context (fun ({ runtime = { logger = logger }} as ctx) ->
    Log.log logger "Suave.Auth.authenticate" LogLevel.Debug "authenticating"

    cookie_state
      { server_key      = ctx.runtime.serverKey
        cookie_name     = SessionAuthCookie
        user_state_key  = StateStoreType
        relative_expiry = relative_expiry
        secure          = secure }
      missing_cookie
      decryption_failure
      f_success)

let authenticate' relative_expiry login_page f_success : HttpPart =
  authenticate relative_expiry false
               (fun () -> Choice2Of2(Redirection.FOUND login_page))
               (sprintf "%A" >> RequestErrors.BAD_REQUEST >> Choice2Of2)
               f_success

/// Set server-signed cookies to make the response contain a cookie
/// with a valid session id. It's worth having in mind that when you use this web
/// part, you're setting cookies on the response; so you'll need to have the
/// client re-send a request if you require authentication for it, after this
/// web part has run.
///
/// Parameters:
///  - `relative_expiry`: how long does the authentication cookie last?
/// - `secure`: HttpsOnly?
///
/// Always succeeds.
let authenticated relative_expiry secure : HttpPart =
  context (fun { request = req } ->
    let data = generate_data req |> UTF8.bytes
    authenticate relative_expiry secure
                 (fun _ -> Choice1Of2 data)
                 (fun _ -> Choice1Of2 data)
                 Suave.Http.succeed)

//  let deauthenticate : HttpPart =
//    Cookies.unset_cookies
  
module HttpContext =

  let session_id x =
    x.userState
    |> Map.tryFind StateStoreType
    |> Option.map (fun x -> x :?> string |> parse_data)
