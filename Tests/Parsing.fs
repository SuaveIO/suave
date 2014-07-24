module Suave.Tests.Parsing

open Fuchu

open System
open System.IO
open System.Net.Http
open System.Net.Http.Headers
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

  let test_multipart_form = 
    request(fun r -> 
      match get_first r.multipart_fields "From" with
      | Some str -> OK str
      | None -> OK "FAILED")

  let byte_array_content = new ByteArrayContent(post_data1)
  byte_array_content.Headers.TryAddWithoutValidation("Content-Type","multipart/form-data; boundary=99233d57-854a-4b17-905b-ae37970e8a39") |> ignore

  testList "http parser tests" [
      testCase "parsing a large multipart form" <| fun _ ->
        Assert.Equal("", "Bob <bob@wishfulcoding.mailgun.org>", run_with' test_multipart_form |> req POST "/" (Some <| byte_array_content))

      testCase "parsing a large urlencoded form data" <| fun _ ->
        Assert.Equal("", "hallo wereld", 
          run_with' test_url_encoded_form |> req_gzip POST "/" (Some <| new StringContent(post_data2, Encoding.UTF8, "application/x-www-form-urlencoded")))

  ]
