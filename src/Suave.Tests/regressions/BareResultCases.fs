module Suave.Tests.BareResultCases

// Regression: a union case named `Ok` or `Error` reachable after `open Suave`
// shadows F#'s `Result` cases in user code. `open Suave` comes last so that
// anything it brings into scope wins name resolution; if this file compiles,
// bare `Ok`/`Error` still mean `Result.Ok`/`Result.Error`. Suave.Tests sees
// Suave's internals, so internal types must not leak these names either.

open Expecto
open Suave

let private failed : Result<int, string> = Error "x"
let private succeeded : Result<int, string> = Ok 1

let private describe (x : Result<int, string>) =
  match x with
  | Ok v -> string v
  | Error e -> e

[<Tests>]
let tests =
  testList "bare Result cases after open Suave" [
    testCase "Ok and Error construct and match Result" <| fun _ ->
      Expect.equal (describe succeeded) "1" "Ok should be Result.Ok"
      Expect.equal (describe failed) "x" "Error should be Result.Error"
  ]