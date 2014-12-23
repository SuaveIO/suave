module Suave.Tests.HttpWriters

open Fuchu

open System

open Suave
open Suave.Http
open Suave.Http.Successful
open Suave.Http.Writers
open Suave.Types

open Suave.Tests.TestUtilities

[<Tests>]
let cookies =
  let run_with' = run_with default_config

  let basic_cookie =
    { name      = "mycookie"
    ; value     = "42"
    ; expires   = None
    ; domain    = None
    ; path      = Some "/"
    ; http_only = false
    ; secure    = false }

  testList "Cookies basic tests" [
      testCase "cookie data makes round trip" <| fun _ ->
        Assert.Equal("expecting cookie value"
        , "42"
        , (req_cookies HttpMethod.GET "/" None
          (run_with' (Cookie.set_cookie basic_cookie >>= OK "test")))
            .GetCookies(Uri("http://127.0.0.1")).[0].Value)

      testCase "cookie name makes round trip" <| fun _ ->
        Assert.Equal("expecting cookie name"
        , "mycookie"
        , (req_cookies HttpMethod.GET "/" None
            (run_with' (Cookie.set_cookie basic_cookie >>= OK "test")))
            .GetCookies(Uri("http://127.0.0.1")).[0].Name)

      testCase "http_only cookie is http_only" <| fun _ ->
        Assert.Equal("expecting http_only"
        , true
        , (req_cookies HttpMethod.GET "/" None
          (run_with' (Cookie.set_cookie { basic_cookie with http_only = true } >>= OK "test")))
            .GetCookies(Uri("http://127.0.0.1")).[0].HttpOnly)
    ]
