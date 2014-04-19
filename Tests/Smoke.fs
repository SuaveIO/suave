module Suave.Tests.Smoke

open Fuchu

[<Tests>]
let smoking =
  testList "smoking hot" [
    testCase "smoke" <| fun _ -> Assert.Equal("smoke test", true, true)
  ]
