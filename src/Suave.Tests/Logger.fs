module Suave.Tests.Logger

open Suave
open Suave.Logging
open Suave.Testing
open Expecto
open FsCheck
open Tests.TestUtilities

[<Tests>]
let ``CombiningLogger.log`` (_ : SuaveConfig) =
  testList "CombiningLogger.log" [
    testCase "CombiningLogger.log calls multiple loggers" <| fun _ ->
      let log1 = InspectableLog()
      let log2 = InspectableLog()
      let log3 = InspectableLog()
      let logger = CombiningTarget([| "combining-target" |], [ log1; log2; log3 ]) :> Logger
      logger.log LogLevel.Info (Message.eventX "hello") |> Async.RunSynchronously
      Expect.equal log1.logs.Length 1 "log1 should have 1 logged message"
      Expect.equal log2.logs.Length 1 "log2 should have 1 logged message"
      Expect.equal log3.logs.Length 1 "log3 should have 1 logged message"
  ]