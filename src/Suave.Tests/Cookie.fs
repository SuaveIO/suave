module Suave.Tests.Cookie

open Suave
open Suave.Cookie

open Expecto

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
          httpOnly  = true
          sameSite  = None }
      Expect.equal subject expected "cookie should eq"

    testCase "parse SameSite=Strict" <| fun _ ->
      let sample = @"st=oFqpYxbMObHvpEW!QLzedHwSZ1gZnotBs$; Path=/; HttpOnly; SameSite=Strict"
      let subject = Cookie.parseResultCookie sample
      let expected =
        { name      = "st"
          value     = "oFqpYxbMObHvpEW!QLzedHwSZ1gZnotBs$"
          expires   = None
          path      = Some "/"
          domain    = None
          secure    = false
          httpOnly  = true
          sameSite  = Some Strict }
      Expect.equal subject expected "cookie should eq"

    testCase "parse SameSite=Lax" <| fun _ ->
      let sample = @"st=oFqpYxbMObHvpEW!QLzedHwSZ1gZnotBs$; Path=/; HttpOnly; SameSite=Lax"
      let subject = Cookie.parseResultCookie sample
      let expected =
        { name      = "st"
          value     = "oFqpYxbMObHvpEW!QLzedHwSZ1gZnotBs$"
          expires   = None
          path      = Some "/"
          domain    = None
          secure    = false
          httpOnly  = true
          sameSite  = Some Lax }
      Expect.equal subject expected "cookie should eq"

    testCase "parse secure" <| fun _ ->
      let cookie =
        { name      = ""
          value     = ""
          expires   = None
          path      = Some "/"
          domain    = None
          secure    = true
          httpOnly  = false
          sameSite  = None }
      let parsed = Cookie.parseResultCookie (HttpCookie.toHeader cookie)
      Expect.equal parsed cookie "eq"

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
        Expect.equal result expected "cookies should eq"

      testCase "ignore malformed cookies" <| fun _ ->
        let sample = "session=;value;anothervalue= "
        let result = Cookie.parseCookies sample
        Expect.equal result [] "cookies should be ignored"
    ]


