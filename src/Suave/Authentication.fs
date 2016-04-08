﻿module Suave.Authentication

open System.Text
open Suave.RequestErrors
open Suave.Utils
open Suave.Logging
open Suave.Cookie
open Suave.Operators
open Suave.State.CookieStateStore

let UserNameKey = "userName"

let internal parseAuthenticationToken (token : string) =
  let parts = token.Split (' ')
  let enc = parts.[1].Trim()
  let decoded = ASCII.decodeBase64 enc
  let indexOfColon = decoded.IndexOf(':')
  (parts.[0].ToLower(), decoded.Substring(0,indexOfColon), decoded.Substring(indexOfColon+1))

let inline private addUserName username ctx = { ctx with userState = ctx.userState |> Map.add UserNameKey (box username) }

let authenticateBasic f (protectedPart : WebPart) (ctx : HttpContext) =
  let p = ctx.request
  match p.header "authorization" with
  | Choice1Of2 header ->
    let (typ, username, password) = parseAuthenticationToken header
    if (typ.Equals("basic")) && f (username, password) then
      protectedPart (addUserName username ctx)
    else
      challenge (addUserName username ctx)
  | Choice2Of2 _ ->
    challenge ctx

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

let authenticated relativeExpiry secure : WebPart =
  context (fun ctx ->
    let data = generateData ctx |> UTF8.bytes
    authenticate relativeExpiry secure
                 (fun _ -> Choice1Of2 data)
                 (fun _ -> Choice1Of2 data)
                 succeed)

let deauthenticate _ : WebPart =
  unsetPair SessionAuthCookie 
  >=> unsetPair StateCookie

let deauthenticateWithLogin loginPage : WebPart =
  deauthenticate()
  >=> Redirection.FOUND loginPage


  
module HttpContext =

  let sessionId x =
    x.userState
    |> Map.tryFind StateStoreType
    |> Option.map (fun x -> x :?> string |> parseData)
