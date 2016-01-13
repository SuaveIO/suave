module Suave.Tests.HttpRequestHeaders

open Fuchu

open Suave

[<Tests>]
let headers (_:SuaveConfig) =
  testList "Request header letter case" [
    testCase "compare header names case-insensitively" <| fun _ ->
      let req = { HttpRequest.empty with headers = ["x-suave-customheader", "value"] }
      let actual = req.header "X-Suave-CustomHeader"
      Assert.Equal ("results in Choise1Of2", Choice1Of2 "value", actual)
    ]
