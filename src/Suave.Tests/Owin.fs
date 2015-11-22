module Suave.Tests.Owin

open Fuchu

open System
open System.Collections.Generic
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text

open Suave
open Suave.Http
open Suave.AsyncOption.Operators
open Suave.Http.Applicatives
open Suave.Http.Writers
open Suave.Owin

open Suave.Tests.TestUtilities
open Suave.Testing

let eq msg a b =
  Assert.Equal(msg, a, b)

let eqs msg bs aas =
  Assert.Equal(msg, bs |> List.ofSeq, aas |> List.ofSeq)

let throws msg matcher fn =
  try fn () with e when matcher e -> ()

[<Tests>]
let owinUnit =
  let create (m : (string * string) list) =
    OwinApp.DeltaDictionary(m) :> IDictionary<string, string[]>

  let createOwin () =
    let request = { HttpRequest.empty with ``method`` = HttpMethod.PUT }
    new OwinApp.OwinDictionary("/", { HttpContext.empty with request = request })
    :> IDictionary<string, obj>

  testList "infrastructure" [
    testList "DeltaDictionary" [
      testCase "construct & Delta" <| fun () ->
        let subject = create ["a", "a-1"]
        eq "has a" [|"a-1"|] subject.["a"]

      testCase "case insensitive lookup" <| fun () ->
        let subject = create ["a", "a-1"]
        eq "has a" [|"a-1"|] subject.["A"]

      testCase "interaction set" <| fun _ ->
        let subject = create []
        subject.["b"] <- [| "b-1"; "b-2" |]
        eqs "has b" ["b-1"; "b-2"] subject.["b"]

      testCase "interaction set/remove" <| fun _ ->
        let subject = create ["a", "a-1"]
        eqs "has a" ["a-1"] subject.["a"]

        eqs "has only a"
           ([ "a" ] :> _ seq)
           (subject.Keys :> _ seq)

        subject.["b"] <- [| "b-1"; "b-2" |]
        eqs "has b" ["b-1"; "b-2"] subject.["b"]

        eq "can remove b once" true (subject.Remove("b"))
        throws "key not found exception on b"
               (function :? KeyNotFoundException -> true | _ -> false)
               (fun _ -> subject.["b"] |> ignore)
        eq "cannot remove b twice" false (subject.Remove("b"))

        subject.["b"] <- [| "b-3" |]
        eqs "has b once more" ["b-3"] subject.["b"]

        subject.["b"] <- [| "b-4" |]
        eqs "can change b after remove" ["b-4"] (subject.["b"])
        eq "can remove b after change b after remove" true (subject.Remove("b"))
        
        subject.["c"] <- [| "c-1" |]
        eqs "has a, c"
            ["a"; "c"]
            (subject.Keys :> _ seq)

        eq "can remove a once" true (subject.Remove("a"))
        eq "cannot remove a twice" false (subject.Remove("a"))

        eq "cannot remove what's never been there" false (subject.Remove("x"))

        subject.["a"] <- [| "a-1" |]
        eqs "has a once more" ["a-1"] subject.["a"]
      ]

    testList "OwinDictionary" [
      testCase "read/write HttpMethod" <| fun _ ->
        let subj = createOwin ()
        eq "method" "PUT" (subj.[OwinConstants.requestMethod] |> unbox)
        subj.[OwinConstants.requestMethod] <- "get"

      testCase "read/write custom" <| fun _ ->
        let subj = createOwin ()
        subj.["testing.MyKey"] <- "oh yeah"
        eq "read back" "oh yeah" (subj.["testing.MyKey"] |> unbox)

      testCase "case insensitive lookup for OWIN key" <| fun _ ->
        let subj = createOwin ()
        subj.["owin.RequestPath"] <- "/owin"
        eq "read back" "/owin" (subj.["owin.requestPath"] |> unbox)

      testCase "case insensitive lookup for custom key" <| fun _ ->
        let subj = createOwin ()
        subj.["testing.MyKey"] <- "oh yeah"
        eq "read back" "oh yeah" (subj.["Testing.MyKey"] |> unbox)
      ]
    ]

[<Tests>]
let owinEndToEnd cfg =
  let runWithConfig = runWith cfg

  let owinHelloWorld (env : OwinEnvironment) =
    let hello = "Hello, OWIN!"B

    env.[OwinConstants.responseStatusCode] <- box 201

    // set content type, new reference, invalid charset
    let responseHeaders : IDictionary<string, string[]> = unbox env.[OwinConstants.responseHeaders]
    responseHeaders.["Content-Type"] <- [| "application/json; charset=utf-1" |]

    // overwrite invalid 1, new reference, invalid charset
    let responseHeaders' : IDictionary<string, string[]> = unbox env.[OwinConstants.responseHeaders]
    responseHeaders'.["Content-Type"] <- [| "text/plain; charset=utf-2" |]

    // overwrite invalid 2, old reference, invalid charset
    responseHeaders.["Content-Type"] <- [| "application/json; charset=utf-3" |]

    // overwrite final, new reference
    let responseHeaders'' : IDictionary<string, string[]> = unbox env.[OwinConstants.responseHeaders]
    responseHeaders''.["Content-Type"] <- [| "text/plain; charset=utf-8" |]

    let responseStream : IO.Stream = unbox env.[OwinConstants.responseBody]
    responseStream.Write(hello, 0, hello.Length)
    async.Return ()

  let composedApp =
    path "/owin"
      >=> setHeader "X-Custom-Before" "Before OWIN"
      >=> OwinApp.ofApp "/" owinHelloWorld
      >=> setHeader "X-Custom-After" "After OWIN"

  testList "e2e" [
    testCase "Hello, OWIN!" <| fun _ ->
      let asserts (result : HttpResponseMessage) =
        eq "Content-Type" "text/plain; charset=utf-8" (result.Content.Headers.ContentType.ToString())
        eq "Http Status Code" HttpStatusCode.Created result.StatusCode
        eq "Content Length" ("Hello, OWIN!"B.LongLength) (result.Content.Headers.ContentLength.Value)
        eq "Contents" "Hello, OWIN!" (result.Content.ReadAsStringAsync().Result)

        match result.Headers.TryGetValues("X-Custom-Before") with
        | true, actual ->
          eqs "Headers set before the OWIN app func, are sent"
              ["Before OWIN"]
              actual
        | false, _ -> Tests.failtest "X-Custom-Before is missing"

        match result.Headers.TryGetValues("X-Custom-After") with
        | true, actual ->
          eqs "Headers after before the OWIN app func, are sent"
              ["After OWIN"]
              actual
        | false, _ -> Tests.failtest "X-Custom-After is missing"

      runWithConfig composedApp |> reqResp HttpMethod.GET "/owin" "" None None DecompressionMethods.GZip id asserts

    testCase "Empty OWIN app should return 200 OK" <| fun _ ->
      let owinDefaults (env : OwinEnvironment) =
        async.Return ()

      let composedApp =
        OwinApp.ofApp "/" owinDefaults

      let asserts (result : HttpResponseMessage) =
        eq "Http Status Code" HttpStatusCode.OK result.StatusCode
        eq "Reason Phrase" "OK" result.ReasonPhrase

      runWithConfig composedApp |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.GZip id asserts

    testCase "Empty OwinAppFunc should return 200 OK" <| fun _ ->
      let owinDefaults = OwinAppFunc(fun env ->
        Threading.Tasks.Task.FromResult() :> Threading.Tasks.Task)

      let composedApp =
        OwinApp.ofAppFunc "/" owinDefaults

      let asserts (result : HttpResponseMessage) =
        eq "Http Status Code" HttpStatusCode.OK result.StatusCode
        eq "Reason Phrase" "OK" result.ReasonPhrase

      runWithConfig composedApp |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.GZip id asserts

    testCase "Completed Async signals completion with status code" <| fun _ ->
      let noContent (env : OwinEnvironment) =
        env.[OwinConstants.responseStatusCode] <- box 204
        async.Return ()

      let composedApp =
        OwinApp.ofApp "/" noContent

      let asserts (result : HttpResponseMessage) =
        eq "Http Status Code" HttpStatusCode.NoContent result.StatusCode
        eq "Reason Phrase set by server" "No Content" result.ReasonPhrase

      runWithConfig composedApp |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.GZip id asserts

    testCase "Completed Async signals completion with status code and headers" <| fun _ ->
      let noContent (env : OwinEnvironment) =
        env.[OwinConstants.responseStatusCode] <- box 204
        let responseHeaders : IDictionary<string, string[]> = unbox env.[OwinConstants.responseHeaders]
        responseHeaders.["Content-Type"] <- [| "text/plain; charset=utf-8" |]
        async.Return ()

      let composedApp =
        OwinApp.ofApp "/" noContent

      let asserts (result : HttpResponseMessage) =
        eq "Http Status Code" HttpStatusCode.NoContent result.StatusCode
        eq "Reason Phrase set by server" "No Content" result.ReasonPhrase
        eq "Content-Type" "text/plain; charset=utf-8" (result.Content.Headers.ContentType.ToString())

      runWithConfig composedApp |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.GZip id asserts

    testCase "Custom reason phrase" <| fun _ ->
      let noContent (env : OwinEnvironment) =
        env.[OwinConstants.responseStatusCode] <- box 204
        env.[OwinConstants.responseReasonPhrase] <- box "Nothing to see here"
        async.Return ()

      let composedApp =
        OwinApp.ofApp "/" noContent

      let asserts (result : HttpResponseMessage) =
        eq "Http Status Code" HttpStatusCode.NoContent result.StatusCode
        // TO CONSIDER: allow to change reason phrase
        // eq "Reason Phrase" "Nothing to see here" result.ReasonPhrase

      runWithConfig composedApp |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.GZip id asserts

    testCase "OWIN middleware runs as a Suave app" <| fun _ ->
      // This test case exists to show that a middleware will mount into Suave as an application.
      let noContent = OwinMidFunc(fun next -> OwinAppFunc(fun env ->
        env.[OwinConstants.responseStatusCode] <- box 204
        Threading.Tasks.Task.FromResult() :> Threading.Tasks.Task
        ))

      let composedApp =
        OwinApp.ofMidFunc "/" noContent

      let asserts (result : HttpResponseMessage) =
        eq "Http Status Code" HttpStatusCode.NoContent result.StatusCode
        eq "Reason Phrase set by server" "No Content" result.ReasonPhrase

      runWithConfig composedApp |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.GZip id asserts

    testCase "Composed OWIN security middleware and app" <| fun _ ->
      let noContent = OwinAppFunc(fun env ->
        env.[OwinConstants.responseStatusCode] <- box 204
        Threading.Tasks.Task.FromResult() :> Threading.Tasks.Task
        )
      
      let basicAuthMidFunc = OwinMidFunc(fun next -> OwinAppFunc(fun env ->
        let requestHeaders : IDictionary<string, string[]> = unbox env.[OwinConstants.requestHeaders]
        // NOTE: currently fails b/c headers are not using StringComparer.OrdinalIgnoreCase.
        let authHeader = requestHeaders.["Authorization"].[0].Split([|' '|])
        let credentials =
          authHeader.[1]
          |> Convert.FromBase64String
          |> Encoding.UTF8.GetString
        match credentials.Split([|':'|]) with
        | [|"foo";"bar"|] ->
          let task = next.Invoke(env)
          task
        | _ ->
          env.[OwinConstants.responseStatusCode] <- box 401
          Threading.Tasks.Task.FromResult() :> Threading.Tasks.Task
        ))

      let composedApp =
        OwinApp.ofAppFunc "/" (basicAuthMidFunc.Invoke(noContent))

      let asserts (result : HttpResponseMessage) =
        eq "Http Status Code" HttpStatusCode.NoContent result.StatusCode
        eq "Reason Phrase set by server" "No Content" result.ReasonPhrase

      let sendAuthHeader (req : HttpRequestMessage) =
        req.Headers.Authorization <- Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String("foo:bar"B))
        req

      runWithConfig composedApp |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.GZip sendAuthHeader asserts

    testCase "Setting a path in Suave should mount an OWIN app at that path and set requestPathBase" <| fun _ ->

      let ok (env : OwinEnvironment) =
        let requestPathBase : string = unbox env.[OwinConstants.requestPathBase]
        eq OwinConstants.requestPathBase requestPathBase "/owin"
        let requestPath : string = unbox env.[OwinConstants.requestPath]
        if requestPath <> "/app" then
          env.[OwinConstants.responseStatusCode] <- box 404
        else () // 200 OK
        async.Return ()

      let postCondition (ctx : HttpContext) =
        eq "request url contains /owin again" "/owin/app" ctx.request.url.AbsolutePath
        async.Return (Some ctx)

      let composedApp =
        pathRegex "/owin(/.+)*" >=> OwinApp.ofApp "/owin" ok >=> postCondition

      let asserts (result : HttpResponseMessage) =
        eq "Http Status Code" HttpStatusCode.OK result.StatusCode
        eq "Reason Phrase set by server" "OK" result.ReasonPhrase

      runWithConfig composedApp |> reqResp HttpMethod.GET "/owin/app" "" None None DecompressionMethods.GZip id asserts

    testCase "Manually mount an OWIN app at that path and set requestPathBase" <| fun _ ->
      let ok (env : OwinEnvironment) =
        let requestPathBase : string = unbox env.[OwinConstants.requestPathBase]
        //eq "owin.RequestPathBase" requestPathBase "/owin"
        let requestPath : string = unbox env.[OwinConstants.requestPath]
        if requestPath <> "/app" then
          env.[OwinConstants.responseStatusCode] <- box 404
        else () // 200 OK
        async.Return ()

      let composedApp =
        pathScan "/owin/%s" (fun path -> OwinApp.ofApp "/" (fun env -> async {
          env.[OwinConstants.requestPathBase] <- box "/owin"
          env.[OwinConstants.requestPath] <- box ("/" + path)
          do! ok env
          env.[OwinConstants.requestPathBase] <- box ""
          env.[OwinConstants.requestPath] <- box ("/owin/" + path)
          })
        )

      let asserts (result : HttpResponseMessage) =
        eq "Http Status Code" HttpStatusCode.OK result.StatusCode
        eq "Reason Phrase set by server" "OK" result.ReasonPhrase

      runWithConfig composedApp |> reqResp HttpMethod.GET "/owin/app" "" None None DecompressionMethods.GZip id asserts
    ]
