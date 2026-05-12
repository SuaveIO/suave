module Suave.Tests.LogLevel

open Suave
open Suave.Logging
open Suave.Testing
open Expecto
open FsCheck

[<Tests>]
let toString (_ : SuaveConfig) =
  testList "ToString" [
    testCase "verbose ToString" <| fun _ ->
      Expect.equal (string LogLevel.Verbose) "verbose" "LogLevel.Verbose ToString incorrect"
    testCase "debug ToString"   <| fun _ ->
      Expect.equal (string LogLevel.Debug) "debug" "LogLevel.Debug ToString incorrect"
    testCase "info ToString"    <| fun _ ->
      Expect.equal (string LogLevel.Info) "info" "LogLevel.Info ToString incorrect"
    testCase "warn ToString"    <| fun _ ->
      Expect.equal (string LogLevel.Warn) "warn" "LogLevel.Warn ToString incorrect"
    testCase "error ToString"   <| fun _ ->
      Expect.equal (string LogLevel.Error) "error" "LogLevel.Error ToString incorrect"
    testCase "fatal ToString"   <| fun _ ->
      Expect.equal (string LogLevel.Fatal) "fatal" "LogLevel.Fatal ToString incorrect"
  ]

[<Tests>]
let fromString (_ : SuaveConfig) =
  testList "FromString" [
    testCase "verbose FromString" <| fun _ ->
      Expect.equal (LogLevel.ofString "verbose") LogLevel.Verbose "LogLevel.Verbose FromString failed"
    testCase "debug FromString"   <| fun _ ->
      Expect.equal (LogLevel.ofString "debug") LogLevel.Debug "LogLevel.Debug FromString failed"
    testCase "info FromString"    <| fun _ ->
      Expect.equal (LogLevel.ofString "info") LogLevel.Info "LogLevel.Info FromString failed"
    testCase "warn FromString"    <| fun _ ->
      Expect.equal (LogLevel.ofString "warn") LogLevel.Warn "LogLevel.Warn FromString failed"
    testCase "error FromString"   <| fun _ ->
      Expect.equal (LogLevel.ofString "error") LogLevel.Error "LogLevel.Error FromString failed"
    testCase "fatal FromString"   <| fun _ ->
      Expect.equal (LogLevel.ofString "fatal") LogLevel.Fatal "LogLevel.Fatal FromString failed"
    testCase "unrecognized string defaults to info" <| fun _ ->
      Expect.equal (LogLevel.ofString "garbage") LogLevel.Info "LogLevel.FromString should default to Info"
  ]

[<Tests>]
let toInt (_ : SuaveConfig) =
  testList "ToInt" [
    testCase "verbose ToInt" <| fun _ ->
      Expect.equal (LogLevel.Verbose.toInt()) 1 "LogLevel.Verbose ToInt incorrect"
    testCase "debug ToInt"   <| fun _ ->
      Expect.equal (LogLevel.Debug.toInt()) 2 "LogLevel.Debug ToInt incorrect"
    testCase "info ToInt"    <| fun _ ->
      Expect.equal (LogLevel.Info.toInt()) 3 "LogLevel.Info ToInt incorrect"
    testCase "warn ToInt"    <| fun _ ->
      Expect.equal (LogLevel.Warn.toInt()) 4 "LogLevel.Warn ToInt incorrect"
    testCase "error ToInt"   <| fun _ ->
      Expect.equal (LogLevel.Error.toInt()) 5 "LogLevel.Error ToInt incorrect"
    testCase "fatal ToInt"   <| fun _ ->
      Expect.equal (LogLevel.Fatal.toInt()) 6 "LogLevel.Fatal ToInt incorrect"
  ]

[<Tests>]
let fromInt (_ : SuaveConfig) =
  testList "FromInt" [
    testCase "verbose FromInt" <| fun _ ->
      Expect.equal (LogLevel.ofInt 1) LogLevel.Verbose "verbose FromInt failed"
    testCase "debug FromInt"   <| fun _ ->
      Expect.equal (LogLevel.ofInt 2) LogLevel.Debug "verbose FromInt failed"
    testCase "info FromInt"    <| fun _ ->
      Expect.equal (LogLevel.ofInt 3) LogLevel.Info "verbose FromInt failed"
    testCase "warn FromInt"    <| fun _ ->
      Expect.equal (LogLevel.ofInt 4) LogLevel.Warn "verbose FromInt failed"
    testCase "error FromInt"   <| fun _ ->
      Expect.equal (LogLevel.ofInt 5) LogLevel.Error "verbose FromInt failed"
    testCase "fatal FromInt"   <| fun _ ->
      Expect.equal (LogLevel.ofInt 6) LogLevel.Fatal "verbose FromInt failed"
    testCase "FromInt fails with unknown int" <| fun _ ->
      try
        ignore <| LogLevel.ofInt 0
        Expect.equal false true "FromInt should have raised for unknown integer"
      with exn as ex ->
        Expect.equal (System.String.IsNullOrEmpty ex.Message) false "Should have returned an exception message"
  ]

[<Tests>]
let greaterThan (_ : SuaveConfig) =
  testList "greaterThan" [
    testCase "debug > verbose" <| fun _ ->
      Expect.isTrue (LogLevel.Debug > LogLevel.Verbose) "debug should be > verbose"
    testCase "info > debug"    <| fun _ ->
      Expect.isTrue (LogLevel.Info  > LogLevel.Debug)  "info should be > debug"
    testCase "warn > info"     <| fun _ ->
      Expect.isTrue (LogLevel.Warn  > LogLevel.Info) "warn should be > info"
    testCase "error > warn"    <| fun _ ->
      Expect.isTrue (LogLevel.Error > LogLevel.Warn) "error should be > warn"
    testCase "fatal > error"   <| fun _ ->
      Expect.isTrue (LogLevel.Fatal > LogLevel.Error) "fatal should be > error"
  ]

[<Tests>]
let lessThan (_ : SuaveConfig) =
  testList "lessThan" [
    testCase "verbose < debug" <| fun _ ->
      Expect.isTrue (LogLevel.Verbose < LogLevel.Debug) "verbose should be < debug"
    testCase "debug < info"    <| fun _ ->
      Expect.isTrue (LogLevel.Debug   < LogLevel.Info) "debug should be < info"
    testCase "info < warn"     <| fun _ ->
      Expect.isTrue (LogLevel.Info    < LogLevel.Warn) "info should be < warn"
    testCase "warn < error"    <| fun _ ->
      Expect.isTrue (LogLevel.Warn    < LogLevel.Error) "warn should be < error"
    testCase "error < fatal"   <| fun _ ->
      Expect.isTrue (LogLevel.Error   < LogLevel.Fatal) "error should be < fatal"
  ]

[<Tests>]
let equals (_ : SuaveConfig) =
  testList "equals" [
    testCase "verbose = verbose" <| fun _ ->
      Expect.isTrue (LogLevel.Verbose.Equals(LogLevel.Verbose)) "verbose should be = verbose"
    testCase "verbose != debug" <| fun _ ->
      Expect.isFalse (LogLevel.Verbose.Equals(LogLevel.Debug)) "verbose should not be = debug"
    testCase "verbose != some other object" <| fun _ ->
      try
        Expect.isFalse (LogLevel.Verbose.Equals("tutti fruity"))
                       "Verbose should not be = a string"
        Tests.failtest "Verbose should not have been equal to a string"
      with exn as ex ->
        Expect.isFalse (System.String.IsNullOrEmpty(ex.Message))
                       "Error message should have been generated"
  ]

[<Tests>]
let getHashCode (_ : SuaveConfig) =
  testList "getHashCode" [
    testCase "verbose hash code should use ToInt" <| fun _ ->
      Expect.equal (LogLevel.Verbose.toInt()) (LogLevel.Verbose.GetHashCode()) "verbose hash code != toInt"
    testCase "debug hash code should use ToInt"   <| fun _ ->
      Expect.equal (LogLevel.Debug.toInt()) (LogLevel.Debug.GetHashCode()) "debug hash code != toInt"
    testCase "info hash code should use ToInt"    <| fun _ ->
      Expect.equal (LogLevel.Info.toInt()) (LogLevel.Info.GetHashCode()) "info hash code != toInt"
    testCase "warn hash code should use ToInt"    <| fun _ ->
      Expect.equal (LogLevel.Warn.toInt()) (LogLevel.Warn.GetHashCode()) "warn hash code != toInt"
    testCase "error hash code should use ToInt"    <| fun _ ->
      Expect.equal (LogLevel.Error.toInt()) (LogLevel.Error.GetHashCode()) "error hash code != toInt"
    testCase "fatal hash code should use ToInt"    <| fun _ ->
      Expect.equal (LogLevel.Fatal.toInt()) (LogLevel.Fatal.GetHashCode()) "fatal hash code != toInt"
  ]

// IComparable, <=, and => are exercised indirectly by the above functions
