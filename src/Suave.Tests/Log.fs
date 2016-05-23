module Suave.Tests.Log

open Suave
open Suave.Logging
open Suave.Testing

open Fuchu

open FsCheck

open Tests.TestUtilities

[<Tests>]
let verbose (_ : SuaveConfig) =
  testList "verbose functions" [
    testCase "verbose works" <| fun _ ->
      let log = InspectableLog()
      Log.verbose log "/loggin" { TraceHeader.empty with traceId = 76UL } "verbose test"
      Assert.Equal("There should have been 1 message logged", 1, log.logs.Length)
      Assert.Equal("The message should have been logged as verbose", LogLevel.Verbose, log.logs.Head.level)
      let line = log.logs.Head.value()
      Assert.Equal("The path should have been /loggin", "/loggin", line.path)
      Assert.Equal("The traceId should have been 76", 76UL, line.trace.traceId)
      Assert.Equal("There should not have been an exception", None, line.``exception``)
      Assert.Equal("The message should have been \"verbose test\"", "verbose test", line.message)
    testCase "verbosef works" <| fun _ ->
      let log = InspectableLog()
      Log.verbosef log "/logg" { TraceHeader.empty with traceId = 9UL } (fun f -> f "test %s" "again")
      Assert.Equal("There should have been 1 message logged", 1, log.logs.Length)
      Assert.Equal("The message should have been logged as verbose", LogLevel.Verbose, log.logs.Head.level)
      let line = log.logs.Head.value()
      Assert.Equal("The path should have been /logg", "/logg", line.path)
      Assert.Equal("The traceId should have been 9", 9UL, line.trace.traceId)
      Assert.Equal("There should not have been an exception", None, line.``exception``)
      Assert.Equal("The message should have been \"test again\"", "test again", line.message)
    testCase "verbosee works" <| fun _ ->
      let log = InspectableLog()
      Log.verbosee log "/logz" { TraceHeader.empty with traceId = 12UL } (exn "ugh") "exceptional!"
      Assert.Equal("There should have been 1 message logged", 1, log.logs.Length)
      Assert.Equal("The message should have been logged as verbose", LogLevel.Verbose, log.logs.Head.level)
      let line = log.logs.Head.value()
      Assert.Equal("The path should have been /logz", "/logz", line.path)
      Assert.Equal("The traceId should have been 12", 12UL, line.trace.traceId)
      Assert.Equal("There should have been an exception", true, line.``exception``.IsSome)
      Assert.Equal("The exception message should have been \"ugh\"", "ugh", line.``exception``.Value.Message)
      Assert.Equal("The message should have been \"exceptional!\"", "exceptional!", line.message)
  ]

[<Tests>]
let info (_ : SuaveConfig) =
  testList "info functions" [
    testCase "info works" <| fun _ ->
      let log = InspectableLog()
      Log.info log "/logar" { TraceHeader.empty with traceId = 34UL } "info test"
      Assert.Equal("There should have been 1 message logged", 1, log.logs.Length)
      Assert.Equal("The message should have been logged as info", LogLevel.Info, log.logs.Head.level)
      let line = log.logs.Head.value()
      Assert.Equal("The path should have been /logar", "/logar", line.path)
      Assert.Equal("The traceId should have been 34", 34UL, line.trace.traceId)
      Assert.Equal("There should not have been an exception", None, line.``exception``)
      Assert.Equal("The message should have been \"info test\"", "info test", line.message)
    testCase "infof works" <| fun _ ->
      let log = InspectableLog()
      Log.infof log "/lo" { TraceHeader.empty with traceId = 52UL } (fun f -> f "test %s" "info")
      Assert.Equal("There should have been 1 message logged", 1, log.logs.Length)
      Assert.Equal("The message should have been logged as info", LogLevel.Info, log.logs.Head.level)
      let line = log.logs.Head.value()
      Assert.Equal("The path should have been /lo", "/lo", line.path)
      Assert.Equal("The traceId should have been 52", 52UL, line.trace.traceId)
      Assert.Equal("There should not have been an exception", None, line.``exception``)
      Assert.Equal("The message should have been \"test info\"", "test info", line.message)
    testCase "infoe works" <| fun _ ->
      let log = InspectableLog()
      Log.infoe log "/log-uh" { TraceHeader.empty with traceId = 33UL } (exn "ow") "info ex"
      Assert.Equal("There should have been 1 message logged", 1, log.logs.Length)
      Assert.Equal("The message should have been logged as info", LogLevel.Info, log.logs.Head.level)
      let line = log.logs.Head.value()
      Assert.Equal("The path should have been /log-uh", "/log-uh", line.path)
      Assert.Equal("The traceId should have been 33", 33UL, line.trace.traceId)
      Assert.Equal("There should have been an exception", true, line.``exception``.IsSome)
      Assert.Equal("The exception message should have been \"ow\"", "ow", line.``exception``.Value.Message)
      Assert.Equal("The message should have been \"info ex\"", "info ex", line.message)
  ]

[<Tests>]
let intern (_ : SuaveConfig) =
  testList "intern functions" [
    testCase "intern works" <| fun _ ->
      let log = InspectableLog()
      Log.intern log "/log-it" "intern test"
      Assert.Equal("There should have been 1 message logged", 1, log.logs.Length)
      Assert.Equal("The message should have been logged as verbose", LogLevel.Verbose, log.logs.Head.level)
      let line = log.logs.Head.value()
      Assert.Equal("The path should have been /log-it", "/log-it", line.path)
      Assert.Equal("The trace header should have been empty", TraceHeader.empty, line.trace)
      Assert.Equal("There should not have been an exception", None, line.``exception``)
      Assert.Equal("The message should have been \"intern test\"", "intern test", line.message)
    testCase "interne works" <| fun _ ->
      let log = InspectableLog()
      Log.interne log "/log-log" (exn "oops") "oof"
      Assert.Equal("There should have been 1 message logged", 1, log.logs.Length)
      Assert.Equal("The message should have been logged as verbose", LogLevel.Verbose, log.logs.Head.level)
      let line = log.logs.Head.value()
      Assert.Equal("The path should have been /log-log", "/log-log", line.path)
      Assert.Equal("The trace header should have been empty", TraceHeader.empty, line.trace)
      Assert.Equal("There should have been an exception", true, line.``exception``.IsSome)
      Assert.Equal("The exception message should have been \"oops\"", "oops", line.``exception``.Value.Message)
      Assert.Equal("The message should have been \"oof\"", "oof", line.message)
    testCase "internf works" <| fun _ ->
      let log = InspectableLog()
      Log.internf log "/log-path" (fun f -> f "intern-%s" "test")
      Assert.Equal("There should have been 1 message logged", 1, log.logs.Length)
      Assert.Equal("The message should have been logged as verbose", LogLevel.Verbose, log.logs.Head.level)
      let line = log.logs.Head.value()
      Assert.Equal("The path should have been /log-path", "/log-path", line.path)
      Assert.Equal("The trace header should have been empty", TraceHeader.empty, line.trace)
      Assert.Equal("There should not have been an exception", None, line.``exception``)
      Assert.Equal("The message should have been \"intern-test\"", "intern-test", line.message)
  ]

[<Tests>]
let log (_ : SuaveConfig) =
  testList "log" [
    testCase "log works" <| fun _ ->
      let log = InspectableLog()
      Log.log log "/plain-ol-log" LogLevel.Fatal "loggin' it up!"
      Assert.Equal("There should have been 1 message logged", 1, log.logs.Length)
      Assert.Equal("The message should have been logged as fatal", LogLevel.Fatal, log.logs.Head.level)
      let line = log.logs.Head.value()
      Assert.Equal("The path should have been /plain-ol-log", "/plain-ol-log", line.path)
      Assert.Equal("The trace header should have been empty", TraceHeader.empty, line.trace)
      Assert.Equal("There should not have been an exception", None, line.``exception``)
      Assert.Equal("The message should have been \"loggin' it up!\"", "loggin' it up!", line.message)
  ]
