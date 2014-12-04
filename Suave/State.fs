module Suave.State

open Suave.Types
open Suave.Http
open Suave.Log
open Suave.Cookie

module CookieStateStore =
  open System
  open System.IO
  open System.Runtime.Serialization.Json
  open System.Collections.Generic

  /// "Suave.State.CookieStateStore"
  [<Literal>]
  let StateStoreType = "Suave.State.CookieStateStore"

  /// "st"
  [<Literal>]
  let StateCookie = "st"

  let encode_map m =
    let dcs = DataContractJsonSerializer(m.GetType())
    use ms = new MemoryStream()
    dcs.WriteObject(ms, m)
    ms.ToArray()

  let decode_map bytes =
    let dcs = DataContractJsonSerializer(typeof<Dictionary<string, string>>)
    use ms = new MemoryStream()
    ms.Write(bytes, 0, bytes.Length)
    ms.Seek(0L, SeekOrigin.Begin) |> ignore
    dcs.ReadObject(ms) :?> Dictionary<string, string>

  // TODO: this is a buggy proof of concept serialisation from .Net, consider
  // reworking it to e.g. Fleece
  let write relative_expiry key value =
    context (fun ({ runtime = { logger = logger }} as ctx) ->
//      log logger "Suave.State.CookieStateStore.write" Debug 
//        (sprintf "updating key '%s' with value '%s'" key value)
      log logger "Suave.State.CookieStateStore.write" Debug (sprintf "writing to key '%s'" key)
      update_cookies
        { server_key      = ctx.runtime.server_key
          cookie_name     = StateCookie
          user_state_key  = StateStoreType
          relative_expiry = relative_expiry
          secure          = false }
        (function
         | None ->
           let d = Dictionary<string, string>()
           d.Add (key, value)
           encode_map d
         | Some obj_str ->
           let d = decode_map obj_str
           d.Add (key, value)
           encode_map d))

  let stateful relative_expiry secure : WebPart =
    context (fun ctx ->
      cookie_state
        { server_key      = ctx.runtime.server_key
          cookie_name     = StateCookie
          user_state_key  = StateStoreType
          relative_expiry = relative_expiry
          secure          = secure }
        (fun () -> Choice1Of2("{}" |> UTF8.bytes))
        (sprintf "%A" >> RequestErrors.BAD_REQUEST))
    >>= Writers.set_user_data (StateStoreType + "-expiry") relative_expiry

  ///
  ///
  /// Only save the state for the duration of the browser session.
  let stateful' : WebPart =
    stateful Session false

  module HttpContext =

    let private mk_state_store (user_state : Map<string, obj>) (ss : obj) =
      { new StateStore with
          member x.get key =
            let m = decode_map (ss :?> byte [])
            match m.TryGetValue key with
            | false, _ -> None
            | true, value -> Some value
            |> Option.map (fun x -> Convert.ChangeType(x, typeof<'a>) :?> 'a)
          member x.set key value =
            let expiry = user_state |> Map.find (StateStoreType + "-expiry") :?> CookieLife
            write expiry key (value.ToString()) // TODO: handle gratiously all types of values
            }

    /// Read the session store from the HttpContext.
    let state (ctx : HttpContext) =
      ctx.user_state
      |> Map.tryFind StateStoreType
      |> Option.map (mk_state_store ctx.user_state)

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
      ctx.user_state
      |> Map.tryFind UserStateIdKey
      |> Option.map (fun x -> x :?> string)
      |> Option.get

    /// Read the session store from the HttpContext.
    let state (ctx : HttpContext) =
      ctx.user_state
      |> Map.tryFind StateStoreType
      |> Option.map (fun ss -> ss :?> StateStore)
      |> Option.get
      
  let private wrap (session_map : MemoryCache) relative_expiry session_id =
    let exp = function
      | Session -> CacheItemPolicy()
      | MaxAge ts     -> CacheItemPolicy(SlidingExpiration = ts)

    let state_bag =
      lock session_map (fun _->
        if session_map.Contains session_id then
          session_map.Get session_id
          :?> ConcurrentDictionary<string, obj>
        else
          let cd = new ConcurrentDictionary<string, obj>()
          session_map.Set(CacheItem(session_id, cd), exp relative_expiry)
          cd)

    { new StateStore with
        member x.get key =
          if state_bag.ContainsKey key then
            Some (state_bag.[key] :?> 'a)
          else None
        member x.set key value =
          state_bag.[key] <- value
          succeed }

  let stateful relative_expiry : WebPart =
    let state_store = wrap (MemoryCache.Default) relative_expiry
    context (fun ctx ->
      let state_id = ctx |> HttpContext.state_id
      Writers.set_user_data StateStoreType (state_store state_id))

  let DefaultExpiry = TimeSpan.FromMinutes 30. |> MaxAge