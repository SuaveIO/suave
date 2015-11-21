module Suave.Tests.Smoke

open Suave.Types
open Fuchu

[<Tests>]
let smoking (_ : SuaveConfig) =
  testList "smoking hot" [
    testCase "smoke" <| fun _ -> Assert.Equal("smoke test", true, true)
  ]
