module Suave.Tests.LogLevel

open Suave
open Suave.Logging
open Suave.Testing

open Fuchu

open FsCheck

open Tests.TestUtilities

[<Tests>]
let toString (_ : SuaveConfig) =
  testList "ToString" [
    testCase "verbose ToString" <| fun _ ->
      Assert.Equal("LogLevel.Verbose ToString incorrect", "verbose", string LogLevel.Verbose)
    testCase "debug ToString"   <| fun _ ->
      Assert.Equal("LogLevel.Debug ToString incorrect",   "debug",   string LogLevel.Debug)
    testCase "info ToString"    <| fun _ ->
      Assert.Equal("LogLevel.Info ToString incorrect",    "info",    string LogLevel.Info)
    testCase "warn ToString"    <| fun _ ->
      Assert.Equal("LogLevel.Warn ToString incorrect",    "warn",    string LogLevel.Warn)
    testCase "error ToString"   <| fun _ ->
      Assert.Equal("LogLevel.Error ToString incorrect",   "error",   string LogLevel.Error)
    testCase "fatal ToString"   <| fun _ ->
      Assert.Equal("LogLevel.Fatal ToString incorrect",   "fatal",   string LogLevel.Fatal)
  ]

[<Tests>]
let fromString (_ : SuaveConfig) =
  testList "FromString" [
    testCase "verbose FromString" <| fun _ ->
      Assert.Equal("LogLevel.Verbose FromString failed", LogLevel.Verbose, LogLevel.FromString "verbose")
    testCase "debug FromString"   <| fun _ ->
      Assert.Equal("LogLevel.Debug FromString failed",   LogLevel.Debug,   LogLevel.FromString "debug")
    testCase "info FromString"    <| fun _ ->
      Assert.Equal("LogLevel.Info FromString failed",    LogLevel.Info,    LogLevel.FromString "info")
    testCase "warn FromString"    <| fun _ ->
      Assert.Equal("LogLevel.Warn FromString failed",    LogLevel.Warn,    LogLevel.FromString "warn")
    testCase "error FromString"   <| fun _ ->
      Assert.Equal("LogLevel.Error FromString failed",   LogLevel.Error,   LogLevel.FromString "error")
    testCase "fatal FromString"   <| fun _ ->
      Assert.Equal("LogLevel.Fatal FromString failed",   LogLevel.Fatal,   LogLevel.FromString "fatal")
    testCase "unrecognized string defaults to info" <| fun _ ->
      Assert.Equal("LogLevel.FromString should default to Info", LogLevel.Info, LogLevel.FromString "garbage")
  ]

[<Tests>]
let toInt (_ : SuaveConfig) =
  testList "ToInt" [
    testCase "verbose ToInt" <| fun _ ->
      Assert.Equal("LogLevel.Verbose ToInt incorrect", 1, LogLevel.Verbose.ToInt())
    testCase "debug ToInt"   <| fun _ ->
      Assert.Equal("LogLevel.Debug ToInt incorrect",   2, LogLevel.Debug.ToInt())
    testCase "info ToInt"    <| fun _ ->
      Assert.Equal("LogLevel.Info ToInt incorrect",    3, LogLevel.Info.ToInt())
    testCase "warn ToInt"    <| fun _ ->
      Assert.Equal("LogLevel.Warn ToInt incorrect",    4, LogLevel.Warn.ToInt())
    testCase "error ToInt"   <| fun _ ->
      Assert.Equal("LogLevel.Error ToInt incorrect",   5, LogLevel.Error.ToInt())
    testCase "fatal ToInt"   <| fun _ ->
      Assert.Equal("LogLevel.Fatal ToInt incorrect",   6, LogLevel.Fatal.ToInt())
  ]

[<Tests>]
let fromInt (_ : SuaveConfig) =
  testList "FromInt" [
    testCase "verbose FromInt" <| fun _ ->
      Assert.Equal("verbose FromInt failed", LogLevel.Verbose, LogLevel.FromInt 1)
    testCase "debug FromInt"   <| fun _ ->
      Assert.Equal("verbose FromInt failed", LogLevel.Debug,   LogLevel.FromInt 2)
    testCase "info FromInt"    <| fun _ ->
      Assert.Equal("verbose FromInt failed", LogLevel.Info,    LogLevel.FromInt 3)
    testCase "warn FromInt"    <| fun _ ->
      Assert.Equal("verbose FromInt failed", LogLevel.Warn,    LogLevel.FromInt 4)
    testCase "error FromInt"   <| fun _ ->
      Assert.Equal("verbose FromInt failed", LogLevel.Error,   LogLevel.FromInt 5)
    testCase "fatal FromInt"   <| fun _ ->
      Assert.Equal("verbose FromInt failed", LogLevel.Fatal,   LogLevel.FromInt 6)
    testCase "FromInt fails with unknown int" <| fun _ ->
      try
        ignore <| LogLevel.FromInt 0
        Assert.Equal("FromInt should have raised for unknown integer", true, false)
      with exn as ex ->
        Assert.Equal("Should have returned an exception message", false, System.String.IsNullOrEmpty ex.Message)
  ]

[<Tests>]
let greaterThan (_ : SuaveConfig) =
  testList "greaterThan" [
    testCase "debug > verbose" <| fun _ ->
      Assert.Equal("debug should be > verbose", true, LogLevel.Debug > LogLevel.Verbose)
    testCase "info > debug"    <| fun _ ->
      Assert.Equal("info should be > debug",    true, LogLevel.Info  > LogLevel.Debug)
    testCase "warn > info"     <| fun _ ->
      Assert.Equal("warn should be > info",     true, LogLevel.Warn  > LogLevel.Info)
    testCase "error > warn"    <| fun _ ->
      Assert.Equal("error should be > warn",    true, LogLevel.Error > LogLevel.Warn)
    testCase "fatal > error"   <| fun _ ->
      Assert.Equal("fatal should be > error",   true, LogLevel.Fatal > LogLevel.Error)
  ]

[<Tests>]
let lessThan (_ : SuaveConfig) =
  testList "lessThan" [
    testCase "verbose < debug" <| fun _ ->
      Assert.Equal("verbose should be < debug", true, LogLevel.Verbose < LogLevel.Debug)
    testCase "debug < info"    <| fun _ ->
      Assert.Equal("debug should be < info",    true, LogLevel.Debug   < LogLevel.Info)
    testCase "info < warn"     <| fun _ ->
      Assert.Equal("info should be < warn",     true, LogLevel.Info    < LogLevel.Warn)
    testCase "warn < error"    <| fun _ ->
      Assert.Equal("warn should be < error",    true, LogLevel.Warn    < LogLevel.Error)
    testCase "error < fatal"   <| fun _ ->
      Assert.Equal("error should be < fatal",   true, LogLevel.Error   < LogLevel.Fatal)
  ]

[<Tests>]
let equals (_ : SuaveConfig) =
  testList "equals" [
    testCase "verbose = verbose" <| fun _ ->
      Assert.Equal("verbose should be = verbose", true, LogLevel.Verbose.Equals(LogLevel.Verbose))
    testCase "verbose != debug" <| fun _ ->
      Assert.Equal("verbose should not be = debug", false, LogLevel.Verbose.Equals(LogLevel.Debug))
    testCase "verbose != some other object" <| fun _ ->
      try
        Assert.Equal("verbose should not be = a string", false, LogLevel.Verbose.Equals("tutti fruity"))
        Assert.Equal("verbose should not have been equal to a string", false, true)
      with exn as ex ->
        Assert.Equal("error message should have been generated", false, System.String.IsNullOrEmpty(ex.Message))
  ]

[<Tests>]
let getHashCode (_ : SuaveConfig) =
  testList "getHashCode" [
    testCase "verbose hash code should use ToInt" <| fun _ ->
      Assert.Equal("verbose hash code != ToInt", LogLevel.Verbose.GetHashCode(), LogLevel.Verbose.ToInt())
    testCase "debug hash code should use ToInt"   <| fun _ ->
      Assert.Equal("debug hash code != ToInt",   LogLevel.Debug.GetHashCode(),   LogLevel.Debug.ToInt())
    testCase "info hash code should use ToInt"    <| fun _ ->
      Assert.Equal("info hash code != ToInt",    LogLevel.Info.GetHashCode(),    LogLevel.Info.ToInt())
    testCase "warn hash code should use ToInt"    <| fun _ ->
      Assert.Equal("warn hash code != ToInt",    LogLevel.Warn.GetHashCode(),    LogLevel.Warn.ToInt())
    testCase "error hash code should use ToInt"    <| fun _ ->
      Assert.Equal("error hash code != ToInt",   LogLevel.Error.GetHashCode(),   LogLevel.Error.ToInt())
    testCase "fatal hash code should use ToInt"    <| fun _ ->
      Assert.Equal("fatal hash code != ToInt",   LogLevel.Fatal.GetHashCode(),   LogLevel.Fatal.ToInt())
  ]

// IComparable, <=, and => are exercised indirectly by the above functions
