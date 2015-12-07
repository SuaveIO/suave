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

type Assert with
  static member EmptyHeader (msg : string, header : string, response : HttpResponseMessage) =
    match response.Headers.TryGetValues header with
    | false, _ -> ()
    | true, _ -> Tests.failtest msg

let eq msg a b =
  Assert.Equal(msg, a, b)

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

  let origin = "http://someorigin.com"

  let composedApp corsConfig : WebPart = 
    let config = 
      match corsConfig with
      | None -> corsWithDefaultConfig
      | Some c -> c
    
    path "/cors" >>= choose [
      OPTIONS >>= config >>= NO_CONTENT
      GET >>= config >>= OK "Success" ]

  testList "CORS tests" [
    testList "Preflight tests" [
      testCase "Can make valid preflight CORS request - without Access-Control-Request-Header" <| fun _ ->
      
        let setHeaders (request : HttpRequestMessage) =
          request.Headers.Add("Origin", origin)
          request.Headers.Add("Access-Control-Request-Method", "GET")
          request
         
        let asserts (result : HttpResponseMessage) =
          let content = result.Content.ReadAsStringAsync().Result
          eq "Content" "" content // Preflight request should not return content
          eq "Access-Control-Allow-Origin header" origin (result.Headers.GetValues("Access-Control-Allow-Origin") |> Seq.head)
          eq "Access-Control-Allow-Methods header" "*" (result.Headers.GetValues("Access-Control-Allow-Methods") |> Seq.head)
          eq "Access-Control-Allow-Credentials header" "True" (result.Headers.GetValues("Access-Control-Allow-Credentials") |> Seq.head)
      
        runWithConfig (composedApp None) |> reqResp HttpMethod.OPTIONS "/cors" "" None None DecompressionMethods.None setHeaders asserts

      testCase "Can make valid preflight CORS request - with Access-Control-Request-Header" <| fun _ ->
      
        let allowedHeaders = "Header1, Header2, Header3"

        let setHeaders (request : HttpRequestMessage) =
          request.Headers.Add("Origin", origin)
          request.Headers.Add("Access-Control-Request-Method", "GET")
          request.Headers.Add("Access-Control-Request-Headers", allowedHeaders)
          request
         
        let asserts (result : HttpResponseMessage) =
          let content = result.Content.ReadAsStringAsync().Result
          eq "Content" "" content // Preflight request should not return content
          eq "Access-Control-Allow-Origin header" origin (result.Headers.GetValues("Access-Control-Allow-Origin") |> Seq.head)
          eq "Access-Control-Allow-Methods header" "*" (result.Headers.GetValues("Access-Control-Allow-Methods") |> Seq.head)
          eq "Access-Control-Allow-Credentials header" "True" (result.Headers.GetValues("Access-Control-Allow-Credentials") |> Seq.head)
          eq "Access-Control-Allow-Headers header" allowedHeaders (result.Headers.GetValues("Access-Control-Allow-Headers") |> Seq.head)
      
        runWithConfig (composedApp None) |> reqResp HttpMethod.OPTIONS "/cors" "" None None DecompressionMethods.None setHeaders asserts

      testCase "Can make valid preflight CORS request - with Access-Control-Max-Age" <| fun _ ->
      
        let corsConfig = { defaultCORSConfig with maxAge = Some 3600; }

        let setHeaders (request : HttpRequestMessage) =
          request.Headers.Add("Origin", origin)
          request.Headers.Add("Access-Control-Request-Method", "GET")
          request
         
        let asserts (result : HttpResponseMessage) =
          let content = result.Content.ReadAsStringAsync().Result
          eq "Content" "" content // Preflight request should not return content
          eq "Access-Control-Allow-Origin header" origin (result.Headers.GetValues("Access-Control-Allow-Origin") |> Seq.head)
          eq "Access-Control-Allow-Methods header" "*" (result.Headers.GetValues("Access-Control-Allow-Methods") |> Seq.head)
          eq "Access-Control-Allow-Credentials header" "True" (result.Headers.GetValues("Access-Control-Allow-Credentials") |> Seq.head)
          eq "Access-Control-Max-Age header" "3600" (result.Headers.GetValues("Access-Control-Max-Age") |> Seq.head)
      
        runWithConfig (composedApp (Some (cors corsConfig))) |> reqResp HttpMethod.OPTIONS "/cors" "" None None DecompressionMethods.None setHeaders asserts ]
    testList "Actual requests tests" [
      testCase "Can make valid CORS request with default CORS config" <| fun _ ->
      
        let setHeaders (request : HttpRequestMessage) =
          request.Headers.Add("Origin", origin)
          request.Headers.Add("Access-Control-Request-Method", "GET")
          request
         
        let asserts (result : HttpResponseMessage) =
          let content = result.Content.ReadAsStringAsync().Result
          eq "Content" "Success" content
          eq "Access-Control-Allow-Origin header" origin (result.Headers.GetValues("Access-Control-Allow-Origin") |> Seq.head)
          eq "Access-Control-Allow-Methods header" "*" (result.Headers.GetValues("Access-Control-Allow-Methods") |> Seq.head)
          eq "Access-Control-Allow-Credentials header" "True" (result.Headers.GetValues("Access-Control-Allow-Credentials") |> Seq.head)
      
        runWithConfig (composedApp None) |> reqResp HttpMethod.GET "/cors" "" None None DecompressionMethods.None setHeaders asserts
      
      testCase "CORS request denied when Origin not allowed" <| fun _ ->
        
        let corsConfig = { defaultCORSConfig with allowedUris = InclusiveOption.Some [ "http://someotherdomain.com" ] }

        let setHeaders (request : HttpRequestMessage) =
          request.Headers.Add("Origin", origin)
          request.Headers.Add("Access-Control-Request-Method", "GET")
          request
         
        let asserts (result : HttpResponseMessage) =
          let content = result.Content.ReadAsStringAsync().Result
          eq "Content" "Success" content
          Assert.EmptyHeader("Should not have Access-Control-Allow-Origin header", "Access-Control-Allow-Origin", result)
          Assert.EmptyHeader("Should not have Access-Control-Allow-Methods header", "Access-Control-Allow-Methods", result)
          Assert.EmptyHeader("Should not have Access-Control-Allow-Credentials header", "Access-Control-Allow-Credentials", result)
      
        runWithConfig (composedApp (Some (cors corsConfig))) |> reqResp HttpMethod.GET "/cors" "" None None DecompressionMethods.None setHeaders asserts
    ]
  ]