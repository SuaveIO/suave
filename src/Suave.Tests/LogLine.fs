module Suave.Tests.LogLine

open Suave
open Suave.Logging
open Suave.Testing

open Fuchu

open FsCheck

open Tests.TestUtilities

[<Tests>]
let mk (_ : SuaveConfig) =
  testList "mk" [
    testCase "mk works with no exception" <| fun _ ->
      let line = LogLine.mk "/unit-test" LogLevel.Info TraceHeader.empty None "Hello"
      Assert.Equal("Path should be /unit-test", "/unit-test", line.path)
      Assert.Equal("Level should be info", LogLevel.Info, line.level)
      Assert.Equal("Trace Header should be empty", TraceHeader.empty, line.trace)
      Assert.Equal("Exception should be None", None, line.``exception``)
      Assert.Equal("Message should be \"Hello\"", "Hello", line.message)
      Assert.Equal("UTC ticks should not be 0", true, 0L <> line.tsUTCTicks)
    testCase "mk works with an exception" <| fun _ ->
      let line = LogLine.mk "/another-test" LogLevel.Warn { TraceHeader.empty with traceId = 8UL } (Some (exn "oh noes")) "uh oh"
      Assert.Equal("Path should be /another-test", "/another-test", line.path)
      Assert.Equal("Level should be warn", LogLevel.Warn, line.level)
      Assert.Equal("Trace Header should have Id 8", 8UL, line.trace.traceId)
      Assert.Equal("There should be an exception", true, line.``exception``.IsSome)
      Assert.Equal("Exception should be \"oh noes\"", "oh noes", line.``exception``.Value.Message)
      Assert.Equal("Message should be \"uh oh\"", "uh oh", line.message)
      Assert.Equal("UTC ticks should not be 0", true, 0L <> line.tsUTCTicks)
  ]