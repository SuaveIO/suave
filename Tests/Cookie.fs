module Suave.Tests.Cookie

open Suave
open Suave.Types
open Suave.Cookie

open Fuchu

[<Tests>]
let tests =
  testList "parse cookie" [
    testCase "can parse cookie" <| fun _ ->
      let sample = @"st=oFqpYxbMObHvpEW!QLzedHwSZ1gZnotBs$; Path=/; HttpOnly"
      let cookie = Cookie.parse_cookie sample

      Assert.Equal("cookie should eq",
                   { HttpCookie.mk' "st" "oFqpYxbMObHvpEW!QLzedHwSZ1gZnotBs$"
                      with http_only = true },
                   cookie)

    testProperty "can parse any cookie we generate" <| fun (cookie : HttpCookie) ->
      let parsed = Cookie.parse_cookie (HttpCookie.to_header cookie)
      Assert.Equal("eq", cookie, parsed)
    ]