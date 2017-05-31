module Suave.Tests.CORS

open System
open System.Net
open System.Net.Http
open Expecto
open Suave.Tests.TestUtilities
open Suave
open Suave.Successful
open Suave.Operators
open Suave.Filters
open Suave.Logging
open Suave.Cookie
open Suave.State.CookieStateStore
open Suave.CORS
open Suave.Testing

// For true CORS testing, a browser is necessary. Under the testing, in certain scenarios, the CORS module will return success, where in
// reality, the browser will deny the request because the proper headers were not set.

module Expect =
  let EmptyHeader (msg : string, header : string, response : HttpResponseMessage) =
    match response.Headers.TryGetValues header with
    | false, _ -> ()
    | true, _ -> Tests.failtest msg

let eq msg a b =
  Expect.equal b a msg

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
  let runWithConfig = runWith { cfg with logger = Targets.create LogLevel.Warn [| "Suave"; "Tests"; "CORS" |] }

  let origin = "http://someorigin.com"

  let composedApp corsConfig : WebPart =
    let config =
      match corsConfig with
      | None -> corsWithDefaultConfig
      | Some c -> c

    path "/cors" >=> choose [
      OPTIONS >=> config >=> NO_CONTENT
      GET >=> config >=> OK "Success" ]

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
          eq "Access-Control-Allow-Credentials header" "true" (result.Headers.GetValues("Access-Control-Allow-Credentials") |> Seq.head)

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
          eq "Access-Control-Allow-Credentials header" "true" (result.Headers.GetValues("Access-Control-Allow-Credentials") |> Seq.head)
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
          eq "Access-Control-Allow-Credentials header" "true" (result.Headers.GetValues("Access-Control-Allow-Credentials") |> Seq.head)
          eq "Access-Control-Max-Age header" "3600" (result.Headers.GetValues("Access-Control-Max-Age") |> Seq.head)

        runWithConfig (composedApp (Some (cors corsConfig))) |> reqResp HttpMethod.OPTIONS "/cors" "" None None DecompressionMethods.None setHeaders asserts
        
      testCase "Can make valid preflight CORS request - doesn't contain header when cookies aren't allowed" <| fun _ ->
        
        let corsConfig = { defaultCORSConfig with allowCookies = false }

        let setHeaders (request : HttpRequestMessage) =
          request.Headers.Add("Origin", origin)
          request

        let asserts (result : HttpResponseMessage) =
          eq "Access-Control-Allow-Credentials header" false (result.Headers.Contains("Access-Control-Allow-Credentials"))

        runWithConfig (composedApp (Some(cors corsConfig))) |> reqResp HttpMethod.OPTIONS "/cors" "" None None DecompressionMethods.None setHeaders asserts
      ]

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
          eq "Access-Control-Allow-Credentials header" "true" (result.Headers.GetValues("Access-Control-Allow-Credentials") |> Seq.head)

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
          Expect.EmptyHeader("Should not have Access-Control-Allow-Origin header", "Access-Control-Allow-Origin", result)
          Expect.EmptyHeader("Should not have Access-Control-Allow-Methods header", "Access-Control-Allow-Methods", result)
          Expect.EmptyHeader("Should not have Access-Control-Allow-Credentials header", "Access-Control-Allow-Credentials", result)

        runWithConfig (composedApp (Some (cors corsConfig))) |> reqResp HttpMethod.GET "/cors" "" None None DecompressionMethods.None setHeaders asserts

      testCase "Can respond with predefined Access-Control-Expose-Headers value" <| fun _ ->

        let corsConfig = { defaultCORSConfig with exposeHeaders = InclusiveOption.Some ["Header1"; "Header2"] }

        let setHeaders (request : HttpRequestMessage) =
          request.Headers.Add("Origin", origin)
          request

        let asserts (result : HttpResponseMessage) =
          let content = result.Content.ReadAsStringAsync().Result
          eq "Access-Control-Expose-Headers header" "Header1, Header2" (result.Headers.GetValues("Access-Control-Expose-Headers") |> Seq.head)

        runWithConfig (composedApp (Some (cors corsConfig))) |> reqResp HttpMethod.GET "/cors" "" None None DecompressionMethods.None setHeaders asserts
    ]
  ]
