module Suave.Tests.HttpRequestHeaders

open Expecto
open System.Collections.Generic

open Suave

[<Tests>]
let headers (_:SuaveConfig) =
  testList "Request header letter case" [
    testCase "compare header names case-insensitively" <| fun _ ->
      let req = { HttpRequest.empty with headers = List<_>(["x-suave-customheader", "value"]) }
      let actual = req.header "X-Suave-CustomHeader"
      Expect.equal actual (Choice1Of2 "value") "results in Choice1Of2"
    ]
