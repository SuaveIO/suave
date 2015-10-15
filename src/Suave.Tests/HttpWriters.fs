module Suave.Tests.HttpWriters

open Fuchu

open System

open Suave
open Suave.Http
open Suave.Http.Successful
open Suave.Http.Writers
open Suave.Types

open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let cookies cfg =
  let runWithConfig = runWith cfg

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
