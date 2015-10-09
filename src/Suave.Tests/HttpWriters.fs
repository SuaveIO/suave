module Suave.Tests.HttpWriters

open Fuchu

open System
open System.Linq

open Suave
open Suave.Http
open Suave.Http.Successful
open Suave.Http.Writers
open Suave.Types

open Suave.Tests.TestUtilities
open Suave.Testing

let runWithConfig = runWith defaultConfig

[<Tests>]
let cookies =
  let basic_cookie =
    { name      = "mycookie"
    ; value     = "42"
    ; expires   = None
    ; domain    = None
    ; path      = Some "/"
    ; httpOnly = false
    ; secure    = false }

  testList "Cookies basic tests" [
      testCase "cookie data makes round trip" <| fun _ ->
        Assert.Equal("expecting cookie value"
        , "42"
        , (reqCookies HttpMethod.GET "/" None
          (runWithConfig (Cookie.setCookie basic_cookie >>= OK "test")))
            .GetCookies(Uri("http://127.0.0.1")).[0].Value)

      testCase "cookie name makes round trip" <| fun _ ->
        Assert.Equal("expecting cookie name"
        , "mycookie"
        , (reqCookies HttpMethod.GET "/" None
            (runWithConfig (Cookie.setCookie basic_cookie >>= OK "test")))
            .GetCookies(Uri("http://127.0.0.1")).[0].Name)

      testCase "http_only cookie is http_only" <| fun _ ->
        Assert.Equal("expecting http_only"
        , true
        , (reqCookies HttpMethod.GET "/" None
          (runWithConfig (Cookie.setCookie { basic_cookie with httpOnly = true } >>= OK "test")))
            .GetCookies(Uri("http://127.0.0.1")).[0].HttpOnly)
    ]

[<Tests>]
let headers =
  testList "Headers basic tests" [
    testCase "setHeader adds header if it was not there" <| fun _ ->
        Assert.Equal("expecting header value"
        , "value"
        , (reqHeaders HttpMethod.GET "/" None
          (runWithConfig (Writers.setHeader "X-Custom-Header" "value" >>= OK "test"))).GetValues("X-Custom-Header").Single())

    testCase "setHeader rewrites all instances of header with new single value" <| fun _ ->
        Assert.Equal("expecting header value"
            , "third"
            , (reqHeaders HttpMethod.GET "/" None
              (runWithConfig (
                Writers.setHeader "X-Custom-Header" "first"
                >>= Writers.setHeader "X-Custom-Header" "second"
                >>= Writers.setHeader "X-Custom-Header" "third"
                >>= OK "test"))).GetValues("X-Custom-Header").Single())

    testCase "putHeader adds header and preserve order" <| fun _ ->
        Assert.Equal("expecting header value"
            , [| "first"; "second" |]
            , (reqHeaders HttpMethod.GET "/" None
              (runWithConfig (
                Writers.putHeader "X-Custom-Header" "first"
                >>= Writers.putHeader "X-Custom-Header" "second"
                >>= OK "test"))).GetValues("X-Custom-Header").ToArray())
  ]
