module Suave.Tests.Logger

open Suave
open Suave.Logging
open Suave.Testing
open Fuchu
open FsCheck
open Tests.TestUtilities

[<Tests>]
let ``CombiningLogger.log`` (_ : SuaveConfig) =
  testList "CombiningLogger.log" [
    testCase "CombiningLogger.log calls multiple loggers" <| fun _ ->
      let log1 = InspectableLog()
      let log2 = InspectableLog()
      let log3 = InspectableLog()
      let logger = CombiningTarget [ log1; log2; log3 ] :> Logger
      logger.info (Message.eventX "hello")
      Assert.Equal("log1 should have 1 logged message", 1, log1.logs.Length)
      Assert.Equal("log2 should have 1 logged message", 1, log2.logs.Length)
      Assert.Equal("log3 should have 1 logged message", 1, log3.logs.Length)
  ]