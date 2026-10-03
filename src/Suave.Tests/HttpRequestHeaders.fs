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

    testCase "returns the first case-insensitive match" <| fun _ ->
      let req = { HttpRequest.empty with headers = List<_>(["X-Token", "first"; "x-token", "second"]) }
      Expect.equal (req.header "X-TOKEN") (Choice1Of2 "first") "Preserve duplicate ordering"
      Expect.equal (req.header "missing") (Choice2Of2 "Key missing was not present") "Preserve missing-header diagnostics"

    testCase "header matching is ordinal and independent of current culture" <| fun _ ->
      let originalCulture = System.Globalization.CultureInfo.CurrentCulture
      try
        System.Globalization.CultureInfo.CurrentCulture <- System.Globalization.CultureInfo("tr-TR")
        let req = { HttpRequest.empty with headers = List<_>(["x-id", "value"]) }
        Expect.equal (req.header "X-ID") (Choice1Of2 "value") "ASCII case folding must be culture independent"
        Expect.equal (req.header "x-\u00ADid") (Choice2Of2 "Key x-\u00ADid was not present") "Do not treat Unicode formatting characters as ignorable"
      finally
        System.Globalization.CultureInfo.CurrentCulture <- originalCulture
    ]
