module Suave.Authentication

open System
open System.Text
open Suave.RequestErrors
open Suave.Utils
open Suave.Logging
open Suave.Logging.Message
open Suave.Cookie
open Suave.State.CookieStateStore
open Suave.Operators

let UserNameKey = "userName"

let internal tryParseBasicAuthenticationToken (rawHeader : string) =
  match rawHeader.Split(' ') with
  | [| basic; tokenBase64 |] when String.equalsOrdinalCI "basic" basic ->
    match ASCII.tryDecodeBase64 tokenBase64 with
    | Some token ->
      match token.IndexOf(':') with
      | -1 ->
        None
      | i ->
        let username = token.Substring(0, i)
        let password = token.Substring(i + 1)
        Some (username, password)
    | None ->
      None
  | _ ->
    None

let inline private addUserName username ctx =
  if ctx.userState.ContainsKey UserNameKey then
    ctx.userState.[UserNameKey] <- box username
  else
    ctx.userState.Add(UserNameKey, box username)
  ctx

let authenticateBasicAsync f protectedPart ctx =
  async {
    let p = ctx.request
    match p.header "authorization" with
    | Choice1Of2 header ->
      match tryParseBasicAuthenticationToken header with
      | Some (username, password) ->
          let! authenticated = f (username, password)
          if authenticated then
            return! protectedPart (addUserName username ctx)
          else
            return! challenge ctx
      | None ->
          return! challenge ctx
    | Choice2Of2 _ ->
      return! challenge ctx
  }

let authenticateBasic f protectedPart ctx =
  authenticateBasicAsync (f >> async.Return) protectedPart ctx

module internal Utils =
  /// Generates a string key from the available characters with the given key size
  /// in characters. Note that this key is not cryptographically as random as a pure
  /// random number generator would produce as we only use a small subset alphabet.
  let generateReadableKey (keySize : int) =
    let arr = Array.zeroCreate<byte> keySize |> Crypto.randomize
    let alpha = "abcdefghijklmnopqrstuvwxyz0123456789"
    let result = new StringBuilder(keySize)
    arr
    |> Array.iter (fun (b : byte) -> result.Append alpha.[int b % alpha.Length] |> ignore)
    result.ToString()

let SessionAuthCookie = "auth"

let StateStoreType = "Suave.Auth"

let SessionIdLength = 40

/// Extracts the actual session id and the mac value from the cookie's data.
let parseData (textBlob : string) =
  match textBlob.Split '\n' with
  | [| sessionId; ipAddress; userAgent |] ->
    sessionId

  | _ ->
    failwith "internal error; should not have successfully decrypted data"

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
    ctx.runtime.logger.debug (
      eventX "Authenticating"
      >> setSingleName "Suave.Auth.authenticate")

    let state =
      { serverKey      = ctx.runtime.serverKey
        cookieName     = SessionAuthCookie
        userStateKey   = StateStoreType
        relativeExpiry = relativeExpiry
        secure         = secure }

    cookieState state missingCookie decryptionFailure fSuccess)

let authenticateWithLogin relativeExpiry loginPage fSuccess : WebPart =
  let decryptionFailure  = (fun s -> s.ToString()) >> RequestErrors.BAD_REQUEST >> Choice2Of2
  authenticate relativeExpiry false
               (fun () -> Choice2Of2(Redirection.FOUND loginPage))
               decryptionFailure
               fSuccess

let authenticated relativeExpiry secure : WebPart =
  context (fun ctx ->
    let data = generateData ctx |> Encoding.UTF8.GetBytes
    authenticate relativeExpiry secure
                 (fun _ -> Choice1Of2 data)
                 (fun _ -> Choice1Of2 data)
                 succeed)

let deauthenticate : WebPart =
 unsetPair SessionAuthCookie
 >=> unsetPair StateCookie

let deauthenticateWithLogin loginPage : WebPart =
 deauthenticate
 >=> Redirection.FOUND loginPage

module HttpContext =

  let sessionId ctx =
    match ctx.userState.TryGetValue StateStoreType with
    | true, x ->
      Some (x :?> byte[] |> Encoding.UTF8.GetString |> parseData)
    | _,_ -> None
