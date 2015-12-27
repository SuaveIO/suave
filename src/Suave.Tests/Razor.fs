module Suave.Tests.Razor

open Fuchu

open System
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text

open Suave
open Suave.Operators
open Suave.Razor

open Suave.Tests.TestUtilities
open Suave.Testing

type Foo = { bar : string }

[<Tests>]
let razorTest (cfg :SuaveConfig) =
  let runWithConfig = runWith cfg

  testList "razor test" [
    testCase "simple test" <| fun _ ->
      Assert.Equal("process razor file", "Hello Foo",
        runWithConfig (razor<Foo> "razor.cshtml" { bar = "Foo" }) |> req HttpMethod.GET "/" None)
    testCase "simple test without extension" <| fun _ ->
      Assert.Equal("process razor file", "Hello test",
        runWithConfig (razor<Foo> "razor" { bar = "test" }) |> req HttpMethod.GET "/" None) ]