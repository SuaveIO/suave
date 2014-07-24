module Suave.Tests.Parsing

open Fuchu

open System
open System.IO
open System.Net.Http
open System.Text

open Suave
open Suave.Types.Methods
open Suave.Types
open Suave.Http
open Suave.Http.Successful
open Suave.Tests.TestUtilities

[<Tests>]
let parsing_multipart =
  let run_with' = run_with default_config

  let post_data1 = File.ReadAllBytes(Path.Combine(current_path,"request.txt"))
  let post_data2 = File.ReadAllText(Path.Combine(current_path,"request-1.txt"))

  let test_url_encoded_form = 
    request(fun r -> 
      match (form r) ^^ "stripped-text" with
      | Some str -> OK str
      | None -> OK "FAILED")

  testList "http parser tests" [
      testCase "parsing a large multipart form" <| fun _ ->
        Assert.Equal("expecting 'Pass'", "Pass", run_with' (OK "Pass") |> req_gzip POST "/" (Some <| new ByteArrayContent(post_data1))) 

      testCase "parsing a large urlencoded form data" <| fun _ ->
        Assert.Equal("expecting 'Pass'", "hallo wereld", 
          run_with' test_url_encoded_form |> req_gzip POST "/" (Some <| new StringContent(post_data2, Encoding.UTF8, "application/x-www-form-urlencoded")))

  ]
