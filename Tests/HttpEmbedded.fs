module HttpEmbedded

open Fuchu

open Suave
open Suave.Types
open Suave.Http
open Suave.Http.Successful
open Suave.Http.Embedded

open Suave.Tests.TestUtilities

[<Tests>]
let embedded_resources =
  let run_with' = run_with default_config

  testList "test Embedded.browse" [
      testCase "200 OK returns embedded file" <| fun _ ->
        Assert.Equal("expecting 'Hello World!'", "Hello World!", run_with' browse' |> req HttpMethod.GET "/embedded-resource.txt" None)
    ]