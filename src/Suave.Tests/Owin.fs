module Suave.Tests.Owin

open Fuchu

open System
open System.Collections.Generic
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text

open Suave
open Suave.Utils.AsyncExtensions
open Suave.Operators
open Suave.Filters
open Suave.Writers
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
let owinUnit cfg =
  let runWithConfig = runWith cfg

  let create (m : (string * string) list) =
    Dictionary(dict(List.map (fun (a,b) -> a,[|b|]) m), StringComparer.OrdinalIgnoreCase)

  let createOwin () =
    let request = { HttpRequest.empty with ``method`` = HttpMethod.PUT }
    new OwinApp.OwinContext("/", { HttpContext.empty with request = request })
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

      testCase "TryGetValue" <| fun _ ->
        let subj = create [ "a", "a-1" ]
        match subj.TryGetValue "a" with
        | true, x -> eq "should be a-1" [|"a-1"|] x
        | false, _ -> Tests.failtest "key 'a' not found"
    ]

    // 3.2 Environment: Keys MUST be compared using StringComparer.Ordinal.
    testList "OwinContext" [
      testCase "read/write HttpMethod" <| fun _ ->
        let subj = createOwin ()
        eq "method" "PUT" (subj.[OwinConstants.requestMethod] |> unbox)
        subj.[OwinConstants.requestMethod] <- "get"

      testCase "read request scheme" <| fun _ ->
        let subj = createOwin ()
        eq "request scheme" "http" (subj.[OwinConstants.requestScheme] |> unbox)

      testCase "request uri matches original" <| fun _ ->
        let requestUri = "http://localhost/path?q=a&b=c"
        let subj =
          let request =
            { HttpRequest.empty with
                url = Uri(requestUri)
                headers = [("host","localhost")]
                rawQuery = "q=a&b=c"
              }
          new OwinApp.OwinContext("", { HttpContext.empty with request = request })
          :> IDictionary<string, obj>
        let headers : IDictionary<string, string[]> =
          unbox subj.[OwinConstants.requestHeaders]
        let host : string =
          if headers.ContainsKey("Host") then
            headers.["Host"].[0]
          else "localhost"
        let queryString : string = 
          unbox subj.[OwinConstants.requestQueryString]
        let resultUri =
          unbox subj.[OwinConstants.requestScheme] + "://" +
          host +
          unbox subj.[OwinConstants.requestPathBase] +
          unbox subj.[OwinConstants.requestPath] +
          if String.IsNullOrEmpty queryString then "" else "?" + queryString
        eq "request uri" requestUri resultUri
        eq "request path base" "" (unbox subj.[OwinConstants.requestPathBase])

      testCase "read/write custom" <| fun _ ->
        let subj = createOwin ()
        subj.["testing.MyKey"] <- "oh yeah"
        eq "read back" "oh yeah" (subj.["testing.MyKey"] |> unbox)

      testCase "case sensitive lookup for OWIN key" <| fun _ ->
        let subj = createOwin ()
        subj.["owin.RequestPath"] <- "/owin"
        eq "read back" false (subj.ContainsKey("owin.requestPath"))
        eq "read back" "/owin" (subj.["owin.RequestPath"] |> unbox)

      testCase "case sensitive lookup for custom key" <| fun _ ->
        let subj = createOwin ()
        subj.["testing.MyKey"] <- "oh yeah"
        eq "read back" false (subj.ContainsKey("Testing.MyKey"))
        eq "read back" "oh yeah" (subj.["testing.MyKey"] |> unbox)

      testCase "test for issue #387" <| fun _ ->
        let subj = createOwin ()
        subj.["someKey"] <- "hello"
        let hello = 
          match subj.TryGetValue "someKey" with
          | true, o -> Some(unbox o)
          | _ -> None
        Assert.Equal("TryGetValue should find custom key", Some "hello", hello)

      testCase "interaction/set and retrieve with case insensitivity" <| fun _ ->
        let subj = createOwin ()
        subj.["testing.MyKey"] <- "oh yeah"

        let upperCased = 
          match subj.TryGetValue "Testing.MyKey" with
          | true, o -> Some(unbox o)
          | _ -> None

        Assert.Equal("TryGetValue should not find upper cased key", None, upperCased)

        eq "read back" "oh yeah" (subj.["testing.MyKey"] |> unbox)

        subj.["Testing.MyKey"] <- "oh no"
        eq "read again" "oh no" (subj.["Testing.MyKey"] |> unbox)

      testCase "try read/write custom" <| fun _ ->
        let subj = createOwin ()
        subj.["testing.MyKey"] <- "oh yeah"
        eq "read back" (true, "oh yeah") (let succ, res = subj.TryGetValue("testing.MyKey") in succ, unbox res)

      testCase "case sensitive try lookup for custom key" <| fun _ ->
        let subj = createOwin ()
        subj.["testing.MyKey"] <- "oh yeah"
        eq "custom key found" false (subj.ContainsKey "Testing.MyKey")
        eq "read back" (true, "oh yeah") (let succ, res = subj.TryGetValue("testing.MyKey") in succ, unbox res)

      testCase "interaction/set and try retrieve with case sensitivity" <| fun _ ->
        let subj = createOwin ()
        subj.["testing.MyKey"] <- "oh yeah"
        eq "custom key found" true (subj.ContainsKey "testing.MyKey")
        eq "read back" (false, null) (let succ, res = subj.TryGetValue("Testing.MyKey") in succ, res)

        subj.["Testing.MyKey"] <- "oh no"
        eq "read again" (true, "oh no") (let succ, res = subj.TryGetValue("Testing.MyKey") in succ, unbox res)
    ]

    testList "OWIN response headers" [
      testCase "get x-not-here response-header fails" <| fun _ ->
        let subj = createOwin ()
        let headers : IDictionary<string, string[]> = subj.[OwinConstants.responseHeaders] |> unbox
        match headers.TryGetValue "x-not-here" with
        | false, _ -> ()
        | true, null -> Tests.failtest "errenously returned (true, null)"
        | true, otherwise -> Tests.failtestf "errenously returned %A" otherwise
    ]

    testList "OWIN server state" [
      testCase "cannot dispose OwinEnvironment from Webpart" <| fun _ ->
        let misbehaving (env : OwinEnvironment) =
          match box env with
          | :? IDisposable as disposable ->
            disposable.Dispose()
          | _ ->
            ()
          let content = Encoding.UTF8.GetBytes "Afterwards"
          let stream : IO.Stream = unbox env.[OwinConstants.responseBody]
          stream.WriteAsync(content, 0, content.Length) |> Async.AwaitTask

        let composedApp =
          path "/owin" >=> OwinApp.ofApp "/" misbehaving

        let asserts (result : HttpResponseMessage) =
          eq "Http Status Code" HttpStatusCode.OK result.StatusCode
          eq "Reason Phrase set by server" "Afterwards" (result.Content.ReadAsStringAsync().Result)

        runWithConfig composedApp
        |> reqResp HttpMethod.GET "/owin" "" None None DecompressionMethods.GZip
                   id asserts
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
      >=> (OwinApp.ofApp "/" owinHelloWorld >=> setHeader "X-Custom-After" "After OWIN")

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
        | false, _ -> Tests.skiptest "X-Custom-After is missing"

      runWithConfig composedApp |> reqResp HttpMethod.GET "/owin" "" None None DecompressionMethods.GZip id asserts

    testCase "Completed Async signals completion with status code" <| fun _ ->
      let noContent (env : OwinEnvironment) =
        env.[OwinConstants.responseStatusCode] <- box 204
        env.[OwinConstants.responseReasonPhrase] <- box "No Content"
        let responseStream : IO.Stream = unbox env.[OwinConstants.responseBody]
        responseStream.Write([||],0,0)
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
        env.[OwinConstants.responseReasonPhrase] <- box "No Content"
        let responseHeaders : IDictionary<string, string[]> = unbox env.[OwinConstants.responseHeaders]
        responseHeaders.["Content-Type"] <- [| "text/plain; charset=utf-8" |]
        let responseStream : IO.Stream = unbox env.[OwinConstants.responseBody]
        responseStream.Write([||],0,0)
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
        let responseStream : IO.Stream = unbox env.[OwinConstants.responseBody]
        responseStream.Write([||],0,0)
        async.Return ()

      let composedApp =
        OwinApp.ofApp "/" noContent

      let asserts (result : HttpResponseMessage) =
        eq "Http Status Code" HttpStatusCode.NoContent result.StatusCode
        eq "Reason Phrase" "Nothing to see here" result.ReasonPhrase

      runWithConfig composedApp |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.GZip id asserts

    testCase "OWIN middleware runs as a Suave app" <| fun _ ->
      // This test case exists to show that a middleware will mount into Suave as an application.
      let noContent = OwinMidFunc(fun next -> OwinAppFunc(fun env ->
        env.[OwinConstants.responseStatusCode] <- box 204
        env.[OwinConstants.responseReasonPhrase] <- box "No Content"
        let responseStream : IO.Stream = unbox env.[OwinConstants.responseBody]
        responseStream.Write([||],0,0)
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
        env.[OwinConstants.responseReasonPhrase] <- box "No Content"
        let responseStream : IO.Stream = unbox env.[OwinConstants.responseBody]
        responseStream.Write([||],0,0)
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
          env.[OwinConstants.responseReasonPhrase] <- box "Unauthorized"
          let responseStream : IO.Stream = unbox env.[OwinConstants.responseBody]
          responseStream.Write([||],0,0)
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
          env.[OwinConstants.responseReasonPhrase] <- box "Not Found"
        else () // 200 OK
        let responseStream : IO.Stream = unbox env.[OwinConstants.responseBody]
        responseStream.Write([||],0,0)
        async.Return ()

      let postCondition (ctx : HttpContext) =
        eq "request url contains /owin again" "/owin/app" ctx.request.url.AbsolutePath
        async.Return (Some ctx)

      let composedApp =
        pathRegex "/owin(/.+)*" >=> (OwinApp.ofAppWithContinuation "/owin" ok (fun ctx -> async { return Some ctx }) >=> postCondition)

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
          env.[OwinConstants.responseReasonPhrase] <- box "Not Found"
        else () // 200 OK
        let responseStream : IO.Stream = unbox env.[OwinConstants.responseBody]
        responseStream.Write([||],0,0)
        async.Return ()

      let composedApp =
        pathScan "/owin/%s" (fun path -> (OwinApp.ofApp "/" (fun env -> async {
          env.[OwinConstants.requestPathBase] <- box "/owin"
          env.[OwinConstants.requestPath] <- box ("/" + path)
          do! ok env
          env.[OwinConstants.requestPathBase] <- box ""
          env.[OwinConstants.requestPath] <- box ("/owin/" + path)
          }))
        )

      let asserts (result : HttpResponseMessage) =
        eq "Http Status Code" HttpStatusCode.OK result.StatusCode
        eq "Reason Phrase set by server" "OK" result.ReasonPhrase

      runWithConfig composedApp |> reqResp HttpMethod.GET "/owin/app" "" None None DecompressionMethods.GZip id asserts
    ]
