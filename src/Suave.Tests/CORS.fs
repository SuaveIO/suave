module Suave.Tests.CORS

open System
open System.Net
open System.Net.Http

open Fuchu

open Suave
open Suave.Logging
open Suave.Cookie
open Suave.State.CookieStateStore
open Suave.Http
open Suave.Web
open Suave.Types
open Suave.Http.Successful
open Suave.Http.Applicatives
open Suave.Http.RequestErrors
open Suave.Http.Writers
open Suave.Http.CORS

open Suave.Testing

// For true CORS testing, a browser is necessary. Under the testing, in certain scenarios, the CORS module will return success, where in
// reality, the browser will deny the request because the proper headers were not set.

let eq msg a b =
  Assert.Equal(msg, a, b)

let corsConfig = { CORSConfig.allowedUris = InclusiveOption.All; allowedMethods = InclusiveOption.All; allowCookies = true; exposeHeaders = true; maxAge = Some 3600; }
let corsWithDefaultConfig = cors defaultCORSConfig

[<Tests>]
let tests cfg =
  let runWithConfig = runWith { cfg with logger = Loggers.saneDefaultsFor LogLevel.Warn }
  testList "CORS tests" [
    testCase "can make CORS request" <| fun _ ->
      
      let setHeaders (request : HttpRequestMessage) =
        request.Headers.Add("Origin", "SomeOrigin")
        request.Headers.Add("Access-Control-Request-Method", "GET")
        request
         
      let asserts (result : HttpResponseMessage) =
        eq "Content" "Success" (result.Content.ToString())
        eq "Some Header" "Header Value" (result.Headers.GetValues("Some Header") |> Seq.head)
      
      // TODO: Get a list of flows following the CORS specification

      let composedApp =
        path "/cors" >>= corsWithDefaultConfig >>= OK "Success"

      runWithConfig composedApp |> reqResp HttpMethod.OPTIONS "/cors" "" None None DecompressionMethods.None setHeaders asserts
    ]