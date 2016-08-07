module Suave.Tests.Cookie

open Suave
open Suave.Cookie
open Suave.Logging
open Suave.Testing

open Fuchu

open FsCheck

open Tests.TestUtilities

[<Tests>]
let parseResultCookie (_:SuaveConfig) =
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
          httpOnly = true }
      Assert.Equal("cookie should eq", expected, subject)

    testCase "parse secure" <| fun _ ->
      let cookie =
        { name      = ""
          value     = ""
          expires   = None
          path      = Some "/"
          domain    = None
          secure    = true
          httpOnly = false }
      let parsed = Cookie.parseResultCookie (HttpCookie.toHeader cookie)
      Assert.Equal("eq", cookie, parsed)

// FsCheck character gen from RFC slightly painful; let's do that when merging Freya
//    testPropertyWithConfig fscheck_config "anything generated" <| fun (cookie : HttpCookie) ->
//      let parsed = Cookie.parse_cookie (HttpCookie.to_header cookie)
//      Assert.Equal("eq", cookie, parsed)

    testCase "set cookie (same name) twice keeps last" <| fun _ ->
      let force = Async.RunSynchronously >> Option.get
      let c1 = HttpCookie.createKV "a" "aa"
      let c2 = HttpCookie.createKV "a" "bb"
      let subject =
        HttpContext.empty
        |> Cookie.setCookie c1 |> force
        |> Cookie.setCookie c2 |> force

      Assert.Equal("should keep bb-valued cookie",
                   "bb",
                   subject.response.cookies.["a"].value)
    ]

[<Tests>]
let parseRequestCookies (_ : SuaveConfig) =
    testList "parse request cookies" [
      testCase "parse valid cookies" <| fun _ ->
        let sample = "session=2b14f6a69199243f570031bf94865bb6;abc=123;alphaplusvalues=!#$%&'()*+-./:<=>?@[]^_`{|}~"
        let result = Cookie.parseCookies sample
        let expected = [HttpCookie.createKV "session" "2b14f6a69199243f570031bf94865bb6"
                        HttpCookie.createKV "abc" "123"
                        HttpCookie.createKV "alphaplusvalues" "!#$%&'()*+-./:<=>?@[]^_`{|}~"]
        Assert.Equal("cookies should eq", expected, result)

      testCase "ignore malformed cookies" <| fun _ ->
        let sample = "session=;value;anothervalue= "
        let result = Cookie.parseCookies sample
        Assert.Equal("cookies should be ignored", [], result)
    ]

[<Tests>]
let setCookie (_ : SuaveConfig) =
  testList "set cookie" [
    testCase "set cookie - no warning when < 4k" <| fun _ ->
      let log = InspectableLog()
      let cookie =
        { name      = "test cookie"
          value     = String.replicate 4095 "x"
          expires   = None
          path      = Some "/"
          domain    = None
          secure    = true
          httpOnly  = false }
      let ctx = Cookie.setCookie cookie { HttpContext.empty with runtime = { HttpRuntime.empty with logger = log }}
      Assert.Equal("should be no logs generated", true, List.isEmpty log.logs)
    testCase "set cookie - no warning when = 4k" <| fun _ ->
      let log = InspectableLog()
      let cookie =
        { name      = "test cookie"
          value     = String.replicate 4096 "x"
          expires   = None
          path      = Some "/"
          domain    = None
          secure    = true
          httpOnly  = false }
      let ctx = Cookie.setCookie cookie { HttpContext.empty with runtime = { HttpRuntime.empty with logger = log }}
      Assert.Equal("should be no logs generated", true, List.isEmpty log.logs)
    testCase "set cookie - warning when > 4k" <| fun _ ->
      let log = InspectableLog()
      let cookie =
        { name      = "test cookie"
          value     = String.replicate 4097 "x"
          expires   = None
          path      = Some "/"
          domain    = None
          secure    = true
          httpOnly  = false }
      let ctx = Cookie.setCookie cookie { HttpContext.empty with runtime = { HttpRuntime.empty with logger = log }}
      Assert.Equal("should be 1 log generated", 1, List.length log.logs)
      Assert.Equal("should be a warning", LogLevel.Warn, (List.head log.logs).level)
  ]
