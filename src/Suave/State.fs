module Suave.State

open Suave.Cookie

open System.Text.Json

/// A session store is a reader and a writer function pair keyed on strings.
type StateStore =
  /// Get an item from the state store
  abstract get<'T> : string -> 'T option
  /// Set an item in the state store
  abstract set<'T> : string -> 'T -> WebPart
  // Unset an item in the state store
  abstract unset : string -> WebPart

module CookieStateStore =

  /// The user state key for the state store. Always "Suave.State.CookieStateStore".
  [<Literal>]
  let StateStoreType = "Suave.State.CookieStateStore"

  /// The cookie name for the state store. Always "st".
  [<Literal>]
  let StateCookie = "st"

  let write relativeExpiry (cookieName : string) (value : 'T) =
    context (fun ctx ->

      let cookieState =
        { serverKey      = ctx.runtime.serverKey
          cookieName     = StateCookie
          userStateKey   = StateStoreType
          relativeExpiry = relativeExpiry
          secure         = false }

      updateCookies cookieState (function
         | None ->
           Map.empty
           |> Map.add cookieName (box value)
           |> ctx.runtime.cookieSerialiser.serialise

         | Some data ->
           try
             let m = ctx.runtime.cookieSerialiser.deserialise data

             m
             |> Map.add cookieName (box value)
             |> ctx.runtime.cookieSerialiser.serialise
           with ex ->
             Map.empty
             |> Map.add cookieName (box value)
             |> ctx.runtime.cookieSerialiser.serialise))


  let remove relativeExpiry (cookieName : string) =
    context (fun ctx ->

      let cookieState =
        { serverKey      = ctx.runtime.serverKey
          cookieName     = StateCookie
          userStateKey   = StateStoreType
          relativeExpiry = relativeExpiry
          secure         = false }

      updateCookies cookieState (function
        | None ->          
            Map.empty
            |> ctx.runtime.cookieSerialiser.serialise
          
        | Some data ->
            let m =
                try
                    ctx.runtime.cookieSerialiser.deserialise data
                with _ ->
                    Map.empty

            // Although not strictly needed, this allows us to avoid unnecessarily
            // re-serialising the same data if the key is not present.
            if m |> Map.containsKey cookieName then
                try
                    m
                    |> Map.remove cookieName   // Remove the key if we have gotten this far.
                    |> ctx.runtime.cookieSerialiser.serialise
                with _ ->
                    // Return the original data on failure.
                    data
            else
                // Otherwise, just return the original (serialised) data.
                data))

  let stateful relativeExpiry secure : WebPart =
    context (fun ctx ->

      let cipherTextCorrupt =
        (fun s -> s.ToString()) >> RequestErrors.BAD_REQUEST >> Choice2Of2

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

    open System.Collections.Generic

    let private createStateStore (serialiser : CookieSerialiser) (userState : Dictionary<string, obj>) (ss : obj) =
      { new StateStore with
          member x.get key =
            let map =
              try 
                serialiser.deserialise (ss :?> byte [])
              with ex ->
                Map.empty

            map
            |> Map.tryFind key
            |> Option.map (fun z -> JsonSerializer.Deserialize(z :?> JsonElement))
          member x.set key value =
            let expiry = userState.[(StateStoreType + "-expiry")] :?> CookieLife
            write expiry key value
          member x.unset key =
            let expiry = userState.[(StateStoreType + "-expiry")] :?> CookieLife
            remove expiry key
          }

    /// Read the session store from the HttpContext.
    let state (ctx : HttpContext) =
      match ctx.userState.TryGetValue StateStoreType with
      | true, x -> Some (createStateStore ctx.runtime.cookieSerialiser ctx.userState x)
      | _, _ -> None
