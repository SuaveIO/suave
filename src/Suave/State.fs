module Suave.State

open Suave.Types
open Suave.Http
open Suave.Log
open Suave.Cookie
open Suave.Logging

module CookieStateStore =

  open System
  open System.IO
  open System.Collections.Generic
  open Nessos.FsPickler

  /// The user state key for the state store. Always "Suave.State.CookieStateStore". 
  [<Literal>]
  let StateStoreType = "Suave.State.CookieStateStore"

  /// The cookie name for the state store. Always "st".
  [<Literal>]
  let StateCookie = "st"


  let private binarySerializer = FsPickler.CreateBinarySerializer ()
  let private encodeMap (map : Map<string, obj>) : byte [] =
    binarySerializer.Pickle map

  let private decodeMap (bytes : byte []) : Map<string, obj> =
    binarySerializer.UnPickle<Map<string, obj>> bytes

  let write relativeExpiry (key : string) (value : 'T) =
    context (fun ctx ->
      log ctx.runtime.logger "Suave.State.CookieStateStore.write" LogLevel.Debug (sprintf "writing to key '%s'" key)
      updateCookies
        { serverKey      = ctx.runtime.serverKey
          cookieName     = StateCookie
          userStateKey   = StateStoreType
          relativeExpiry = relativeExpiry
          secure         = false }
        (function
         | None      ->
           log ctx.runtime.logger "Suave.State.CookieStateStore.write" LogLevel.Debug "in fPlainText, no existing"
           Map.empty |> Map.add key (box value) |> encodeMap
         | Some data ->
           let m = decodeMap data
           log ctx.runtime.logger "Suave.State.CookieStateStore.write" LogLevel.Debug
             (sprintf "in fPlainText, has existing %A" m)
           m |> Map.add key (box value) |> encodeMap))

  let stateful relativeExpiry secure : WebPart =
    context (fun ctx ->
      log ctx.runtime.logger "Suave.State.CookieStateStore.stateful" LogLevel.Debug "ensuring cookie state"

      let cipherTextCorrupt =
        sprintf "%A" >> RequestErrors.BAD_REQUEST >> Choice2Of2

      let setExpiry : WebPart =
        Writers.setUserData (StateStoreType + "-expiry") relativeExpiry

      cookieState
        { serverKey      = ctx.runtime.serverKey
          cookieName     = StateCookie
          userStateKey   = StateStoreType
          relativeExpiry = relativeExpiry
          secure         = secure }
        (fun () -> Choice1Of2(Map.empty<string, obj> |> encodeMap))
        cipherTextCorrupt
        setExpiry)

  ///
  ///
  /// Only save the state for the duration of the browser session.
  let statefulForSession : WebPart =
    stateful Session false

  module HttpContext =

    let private mkStateStore (userState : Map<string, obj>) (ss : obj) =
      { new StateStore with
          member x.get key =
            decodeMap (ss :?> byte []) |> Map.tryFind key
            |> Option.map (fun x -> Convert.ChangeType(x, typeof<'T>) :?> 'T)
          member x.set key value =
            let expiry = userState |> Map.find (StateStoreType + "-expiry") :?> CookieLife
            write expiry key value
          }

    /// Read the session store from the HttpContext.
    let state (ctx : HttpContext) =
      ctx.userState
      |> Map.tryFind StateStoreType
      |> Option.map (mkStateStore ctx.userState)

/// This module contains the implementation for the memory-cache backed session
/// state store, when the memory cache is global for the server.
module MemoryCacheStateStore =
  open System
  open System.Runtime.Caching
  open System.Collections.Concurrent

  /// This key will be present in HttpContext.userState and will contain the
  /// MemoryCache instance.
  [<Literal>]
  let StateStoreType = "Suave.State.MemoryCacheStateStore"

  [<Literal>]
  let UserStateIdKey = "Suave.State.MemoryCacheStateStore-id"

  [<Literal>]
  let StateCookie = "mc-st"

  module HttpContext =

    /// Try to find the state id of the HttpContext.
    let stateId ctx =
      ctx.userState
      |> Map.tryFind UserStateIdKey
      |> Option.map (fun x -> x :?> string)
      |> Option.get

    /// Read the session store from the HttpContext.
    let state (ctx : HttpContext) =
      ctx.userState
      |> Map.tryFind StateStoreType
      |> Option.map (fun ss -> ss :?> StateStore)
      |> Option.get

  let private wrap (sessionMap : MemoryCache) relativeExpiry sessionId =
    let exp = function
      | Session   -> CacheItemPolicy()
      | MaxAge ts -> CacheItemPolicy(SlidingExpiration = ts)

    let stateBag =
      lock sessionMap (fun _->
        if sessionMap.Contains sessionId then
          sessionMap.Get sessionId
          :?> ConcurrentDictionary<string, obj>
        else
          let cd = new ConcurrentDictionary<string, obj>()
          sessionMap.Set(CacheItem(sessionId, cd), exp relativeExpiry)
          cd)

    { new StateStore with
        member x.get key =
          if stateBag.ContainsKey key then
            Some (stateBag.[key] :?> 'T)
          else None
        member x.set key value =
          stateBag.[key] <- value
          succeed }

  let stateful relativeExpiry : WebPart =
    let stateStore = wrap (MemoryCache.Default) relativeExpiry
    context (fun ctx ->
      let stateId = ctx |> HttpContext.stateId
      Writers.setUserData StateStoreType (stateStore stateId))

  let DefaultExpiry = TimeSpan.FromMinutes 30. |> MaxAge