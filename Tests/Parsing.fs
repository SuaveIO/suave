module Suave.Tests.Parsing

open Fuchu

open System
open System.IO
open System.Net.Http

open Suave.Types.Methods
open Suave.Http
open Suave.Http.Successful
open Suave.Tests.TestUtilities

[<Tests>]
let compression =
  let run_with' = run_with default_config

  let post_data = File.ReadAllBytes(Path.Combine(current_path,"request.txt"))
  let http_content = new ByteArrayContent(post_data)

  testList "parsing a large multipart form" [
      testCase "200 OK returns 'Pass'" <| fun _ ->
        Assert.Equal("expecting 'Havana'", "Pass", run_with' (OK "Pass") |> req_gzip GET "/" (Some http_content))

    ]
