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

// Does the request have an Origin header? 
//  N: Not a valid CORS request. 
//  Y: Is the method OPTIONS?
//    N: This is an actual request.
//    Y: Is there an Access-Control-Request-Method?
//      N: This is an actual request.
//      Y: This is a preflight request.  

[<Tests>]
let tests cfg =
  let runWithConfig = runWith { cfg with logger = Loggers.saneDefaultsFor LogLevel.Warn }
  testList "Preflight tests" [
    testCase "Can make valid preflight CORS request - without Access-Control-Request-Header" <| fun _ ->
      
      let setHeaders (request : HttpRequestMessage) =
        request.Headers.Add("Origin", "http://someorigin.com")
        request.Headers.Add("Access-Control-Request-Method", "GET")
        request
         
      let asserts (result : HttpResponseMessage) =
        let content = result.Content.ReadAsStringAsync().Result
        eq "Content" "" content // Preflight request should not return content
        eq "Access-Control-Request-Method Header" "GET" (result.Headers.GetValues("Access-Control-Request-Method") |> Seq.head)
        eq "Access-Control-Max-Age Header" "3600" (result.Headers.GetValues("Access-Control-Max-Age") |> Seq.head)
      
      let composedApp =
        path "/cors" 
        >>= 
          corsWithDefaultConfig 
        >>= 
          OK "Success"

      runWithConfig composedApp |> reqResp HttpMethod.OPTIONS "/cors" "" None None DecompressionMethods.None setHeaders asserts

    testCase "Can make valid preflight CORS request - with Access-Control-Request-Header" <| fun _ ->
      ()
  ]