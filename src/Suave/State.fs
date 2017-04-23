module Suave.State

open Suave.Utils
open Suave.Cookie
open Suave.Logging
open Suave.Logging.Message

/// A session store is a reader and a writer function pair keyed on strings.
type StateStore =
  /// Get an item from the state store
  abstract get<'T> : string -> 'T option
  /// Set an item in the state store
  abstract set<'T> : string -> 'T -> WebPart

module CookieStateStore =

  open System
  open System.IO
  open System.Collections.Generic

  /// The user state key for the state store. Always "Suave.State.CookieStateStore".
  [<Literal>]
  let StateStoreType = "Suave.State.CookieStateStore"

  /// The cookie name for the state store. Always "st".
  [<Literal>]
  let StateCookie = "st"

  let write relativeExpiry (cookieName : string) (value : 'T) =
    context (fun ctx ->
      let event message =
        eventX message
        >> setFieldValue "cookieName" cookieName
        >> setSingleName "Suave.State.CookieStateStore.write"

      let debug eventFactory =
        ctx.runtime.logger.debug eventFactory

      let cookieState =
        { serverKey      = ctx.runtime.serverKey
          cookieName     = StateCookie
          userStateKey   = StateStoreType
          relativeExpiry = relativeExpiry
          secure         = false }

      debug (event "Writing to {cookieName}")
      updateCookies cookieState (function
         | None ->
           debug (event "In fPlainText, no existing cookie")

           Map.empty
           |> Map.add cookieName (box value)
           |> ctx.runtime.cookieSerialiser.serialise

         | Some data ->
           let m = ctx.runtime.cookieSerialiser.deserialise data
           debug (event "In fPlainText, has existing {cookie}"
                  >> setFieldValue "cookie" m)

           m
           |> Map.add cookieName (box value)
           |> ctx.runtime.cookieSerialiser.serialise))

  let stateful relativeExpiry secure : WebPart =
    context (fun ctx ->
      ctx.runtime.logger.debug (
        eventX "Ensuring cookie state"
        >> setSingleName "Suave.State.CookieStateStore.stateful")

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
        (fun () -> Choice1Of2(Map.empty<string, obj> |> ctx.runtime.cookieSerialiser.serialise))
        cipherTextCorrupt
        setExpiry)

  ///
  /// Only save the state for the duration of the browser session.
  let statefulForSession : WebPart =
    stateful Session false

  module HttpContext =

    let private createStateStore (serialiser : CookieSerialiser) (userState : Map<string, obj>) (ss : obj) =
      { new StateStore with
          member x.get key =
            ss 
            :?> byte []
            |> serialiser.deserialise
            |> Map.tryFind key
            |> Option.map unbox
          member x.set key value =
            let expiry = userState |> Map.find (StateStoreType + "-expiry") :?> CookieLife
            write expiry key value
          }

    /// Read the session store from the HttpContext.
    let state (ctx : HttpContext) =
      ctx.userState
      |> Map.tryFind StateStoreType
      |> Option.map (createStateStore ctx.runtime.cookieSerialiser ctx.userState)

#if SYSTEM_RUNTIME_CACHING
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
#endif
