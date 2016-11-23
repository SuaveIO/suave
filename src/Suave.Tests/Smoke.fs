module Suave.Tests.Smoke

open Suave
open Expecto

[<Tests>]
let smoking (_ : SuaveConfig) =
  testList "smoking hot" [
    testCase "smoke" <| fun _ -> Expect.equal true true "smoke test"
  ]
