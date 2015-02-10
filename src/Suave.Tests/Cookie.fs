module Suave.Tests.Cookie

open Suave
open Suave.Types
open Suave.Cookie

open Fuchu

open FsCheck

open Tests.TestUtilities

[<Tests>]
let tests =
  testList "parse result cookie" [
    testCase "parse path" <| fun _ ->
      let sample = @"st=oFqpYxbMObHvpEW!QLzedHwSZ1gZnotBs$; Path=/; HttpOnly"
      let subject = Cookie.parseResultCookie sample
      let expected =
        { name      = "st"
          value     = "oFqpYxbMObHvpEW!QLzedHwSZ1gZnotBs$"
          expires   = None
          path      = Some "/"
          domain    = None
          secure    = false
          http_only = true }
      Assert.Equal("cookie should eq", expected, subject)

    testCase "parse secure" <| fun _ ->
      let cookie =
        { name      = ""
          value     = ""
          expires   = None
          path      = None
          domain    = None
          secure    = true
          http_only = false }
      let parsed = Cookie.parseResultCookie (HttpCookie.to_header cookie)
      Assert.Equal("eq", cookie, parsed)

// FsCheck character gen from RFC slightly painful; let's do that when merging Freya
//    testPropertyWithConfig fscheck_config "anything generated" <| fun (cookie : HttpCookie) ->
//      let parsed = Cookie.parse_cookie (HttpCookie.to_header cookie)
//      Assert.Equal("eq", cookie, parsed)

    testCase "set cookie (same name) twice keeps last" <| fun _ ->
      let force = Async.RunSynchronously >> Option.get
      let c1 = HttpCookie.mk' "a" "aa"
      let c2 = HttpCookie.mk' "a" "bb"
      let subject =
        HttpContext.empty
        |> Cookie.set_cookie c1 |> force
        |> Cookie.set_cookie c2 |> force
        
      Assert.Equal("should keep bb-valued cookie",
                   "bb",
                   (subject.response |> HttpResult.cookies).["a"].value)
    ]