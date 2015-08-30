module Suave.Tests.Owin

open Fuchu

open System
open System.Collections.Generic
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text

open Suave
open Suave.Types
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Writers
open Suave.Owin

open Suave.Tests.TestUtilities
open Suave.Testing

let eq msg a b =
  Assert.Equal(msg, b, a)

[<Tests>]
let unit =
  testList "infrastructure" [
    testCase "DeltaDictionary" <| fun () ->
      ()
    ]

[<Tests>]
let endToEnd =
  let runWithConfig = runWith defaultConfig

  let owinApp : WebPart =
    let owinApp (env : OwinEnvironment) =
      let hello = "Hello, OWIN!"B

      env.[OwinConstants.responseStatusCode] <- box 201

      let responseHeaders : IDictionary<string, string[]> = unbox env.[OwinConstants.responseHeaders]
      responseHeaders.["Content-Type"] <- [| "text/plain; charset=utf-8" |]

      let responseStream : IO.Stream = unbox env.[OwinConstants.responseBody]
      responseStream.Write(hello, 0, hello.Length)
      async.Return ()

    OwinAppFunc.ofOwin owinApp

  let composedApp =
    path "/"
      >>= setHeader "X-Custom-Before" "Before OWIN"
      >>= owinApp
      >>= Writers.setHeader "X-Custom-After" "After OWIN"

  testList "e2e" [
    testCase "Hello, OWIN!" <| fun _ ->
      let asserts (result : HttpResponseMessage) =
        eq "Content-Type" "text/plain; charset=utf-8" (result.Content.Headers.ContentType.ToString())
        eq "Http Status Code" HttpStatusCode.Created result.StatusCode
        eq "Content Length" ("Hello, OWIN!"B.LongLength) (result.Content.Headers.ContentLength.Value)
        eq "Contents" "Hello, OWIN!" (result.Content.ReadAsStringAsync().Result)
        eq "Headers set before the OWIN app func, are sent"
           (true, ["Before OWIN"] :> _ seq)
           (result.Headers.TryGetValues("X-Custom-Before"))
        eq "Headers after before the OWIN app func, are sent"
           (true, ["After OWIN"] :> _ seq)
           (result.Headers.TryGetValues("X-Custom-After"))

      runWithConfig composedApp |> reqResp Types.GET "/" "" None None DecompressionMethods.GZip id asserts
    ]
    