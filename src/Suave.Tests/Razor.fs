module Suave.Tests.Razor

open Expecto

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
        runWithConfig (razor<Foo> "razor" { bar = "test" }) |> req HttpMethod.GET "/" None)
    testCase "simple test for fallback" <| fun _ ->
      Assert.Equal("process razor file", "Hello test",
        runWithConfig (razor<Foo> "fallback" { bar = "test" }) |> req HttpMethod.GET "/" None)
    testCase "test if accept-language views work" <| fun _ ->
      Assert.Equal("process razor file", "Localized test",
        runWithConfig (razor<Foo> "razor" { bar = "test" })
        |> reqResp HttpMethod.GET "/" "" None None System.Net.DecompressionMethods.None
                   (fun r ->
                      r.Headers.Add("Accept-Language", "de-DE")
                      r
                   ) contentString)
    testCase "test if accept-language views work, even when casing is incorrect" <| fun _ ->
      Assert.Equal("process razor file", "Localized test",
        runWithConfig (razor<Foo> "razor" { bar = "test" })
        |> reqResp HttpMethod.GET "/" "" None None System.Net.DecompressionMethods.None
                   (fun r ->
                      r.Headers.Add("Accept-Language", "de-de")
                      r
                   ) contentString) ]