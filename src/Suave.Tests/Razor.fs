module Suave.Tests.Razor

open Fuchu

open System
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text

open Suave
open Suave.Types
open Suave.Http
open Suave.Razor

open Suave.Tests.TestUtilities
open Suave.Testing

type Foo = { bar : string }

[<Tests>]
let razorTest =
  let runWithConfig = runWith defaultConfig

  testList "razor test" [
    testCase "simple test" <| fun _ ->
      Assert.Equal("process razor file", "Hello Foo", 
        runWithConfig (razor<Foo> "razor.cshtml" { bar = "Foo" }) |> req HttpMethod.GET "/" None) ]