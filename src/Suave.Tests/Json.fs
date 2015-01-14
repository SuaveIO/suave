module Suave.Tests.Json

open Fuchu

open System
open System.Net
open System.Net.Http
open System.Text

open Suave
open Suave.Types
open Suave.Http
open Suave.Json

open Suave.Tests.TestUtilities
open Suave.Testing

let run_with' = run_with default_config

type Foo = { foo : string; }

type Bar = { bar : string; }

[<Tests>]
let tests =
  let async_mapping_func (a:Foo) = 
    async {
      return { bar = a.foo }
    }

  testList "Json tests"
    [
      testCase "map_json test" (fun () ->
        let post_data = new ByteArrayContent(Encoding.UTF8.GetBytes("""{"foo":"foo"}"""))
        let response_data =
          (run_with' (map_json (fun (a:Foo) -> { bar = a.foo })))
          |> req HttpMethod.POST "/" (Some post_data)

        Assert.Equal(
          "Should map JSON from one class to another",
          """{"bar":"foo"}""", response_data))

      testCase "map_json_async test" (fun () ->
        let bad_post_data = new ByteArrayContent(Encoding.UTF8.GetBytes("""{"foo":"foo"}"""))
        let response_data =
          (run_with' (map_json_async async_mapping_func))
          |> req HttpMethod.POST "/" (Some bad_post_data)

        Assert.Equal(
          "Should map JSON from one class to another",
          """{"bar":"foo"}""", response_data))

      testCase "map_json_async bad request test" (fun () ->
        let post_data = new ByteArrayContent(Encoding.UTF8.GetBytes("""{"foo":foo"}"""))
        let actual_status_code =
          (run_with' (map_json_async async_mapping_func))
          |> req_status_code HttpMethod.POST "/" (Some post_data)

        Assert.Equal(
          "Badly formatted JSON should return 400",
          HttpStatusCode.BadRequest, actual_status_code))
    ]