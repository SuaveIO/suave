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

  let private encodeMap (map : Map<string, obj>) =
    let pickler = FsPickler.CreateBinary ()
    use ms = new MemoryStream()
    pickler.Serialize(ms, map)
    ms.ToArray()

  let private decodeMap bytes : Map<string, obj> =
    let pickler = FsPickler.CreateBinary ()
    use ms = new MemoryStream()
    ms.Write (bytes, 0, bytes.Length)
    ms.Seek (0L, SeekOrigin.Begin) |> ignore
    pickler.Deserialize ms

  let write relativeExpiry (key : string) (value : 'T) =
    context (fun ({ runtime = { logger = logger }} as ctx) ->
      log logger "Suave.State.CookieStateStore.write" LogLevel.Debug (sprintf "writing to key '%s'" key)
      updateCookies
        { serverKey      = ctx.runtime.serverKey
          cookieName     = StateCookie
          userStateKey  = StateStoreType
          relativeExpiry = relativeExpiry
          secure          = false }
        (function
         | None      ->
           log logger "Suave.State.CookieStateStore.write" LogLevel.Debug "in f_plain_text, no existing"
           Map.empty |> Map.add key (box value) |> encodeMap
         | Some data ->
           let m = decodeMap data
           log logger "Suave.State.CookieStateStore.write" LogLevel.Debug
             (sprintf "in f_plain_text, has existing %A" m)
           m |> Map.add key (box value) |> encodeMap))

  let stateful relativeExpiry secure : HttpPart =
    context (fun ({ runtime = { logger = logger }} as ctx) ->
      log logger "Suave.State.CookieStateStore.stateful" LogLevel.Debug "ensuring cookie state"

      let cipherTextCorrupt =
        sprintf "%A" >> RequestErrors.BAD_REQUEST >> Choice2Of2

      let setExpiry : HttpPart =
        Writers.setUserData (StateStoreType + "-expiry") relativeExpiry

      cookieState
        { serverKey      = ctx.runtime.serverKey
          cookieName     = StateCookie
          userStateKey  = StateStoreType
          relativeExpiry = relativeExpiry
          secure          = secure }
        (fun () -> Choice1Of2(Map.empty<string, obj> |> encodeMap))
        cipherTextCorrupt
        setExpiry)

  ///
  ///
  /// Only save the state for the duration of the browser session.
  let statefulForSession : HttpPart =
    stateful Session false

  module HttpContext =

    let private mk_state_store (user_state : Map<string, obj>) (ss : obj) =
      { new StateStore with
          member x.get key =
            decodeMap (ss :?> byte []) |> Map.tryFind key
            |> Option.map (fun x -> Convert.ChangeType(x, typeof<'T>) :?> 'T)
          member x.set key value =
            let expiry = user_state |> Map.find (StateStoreType + "-expiry") :?> CookieLife
            write expiry key value
          }

    /// Read the session store from the HttpContext.
    let state (ctx : HttpContext) =
      ctx.userState
      |> Map.tryFind StateStoreType
      |> Option.map (mk_state_store ctx.userState)

/// This module contains the implementation for the memory-cache backed session
/// state store, when the memory cache is global for the server.
module MemoryCacheStateStore =
  open System
  open System.Runtime.Caching
  open System.Collections.Concurrent

  /// This key will be present in HttpContext.user_state and will contain the
  /// MemoryCache instance.
  [<Literal>]
  let StateStoreType = "Suave.State.MemoryCacheStateStore"

  [<Literal>]
  let UserStateIdKey = "Suave.State.MemoryCacheStateStore-id"

  [<Literal>]
  let StateCookie = "mc-st"

  module HttpContext =

    /// Try to find the state id of the HttpContext.
    let state_id ctx =
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
      
  let private wrap (session_map : MemoryCache) relativeExpiry session_id =
    let exp = function
      | Session   -> CacheItemPolicy()
      | MaxAge ts -> CacheItemPolicy(SlidingExpiration = ts)

    let state_bag =
      lock session_map (fun _->
        if session_map.Contains session_id then
          session_map.Get session_id
          :?> ConcurrentDictionary<string, obj>
        else
          let cd = new ConcurrentDictionary<string, obj>()
          session_map.Set(CacheItem(session_id, cd), exp relativeExpiry)
          cd)

    { new StateStore with
        member x.get key =
          if state_bag.ContainsKey key then
            Some (state_bag.[key] :?> 'T)
          else None
        member x.set key value =
          state_bag.[key] <- value
          succeed }

  let stateful relativeExpiry : HttpPart =
    let state_store = wrap (MemoryCache.Default) relativeExpiry
    context (fun ctx ->
      let state_id = ctx |> HttpContext.state_id
      Writers.setUserData StateStoreType (state_store state_id))

  let DefaultExpiry = TimeSpan.FromMinutes 30. |> MaxAge