module Suave.Tests.Parsing

open Fuchu

open System
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text

open Suave
open Suave.Types
open Suave.Types
open Suave.Http
open Suave.Http.Successful
open Suave.Utils
open Suave.Web

open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let parsing_multipart =
  let runWith' = runWith defaultConfig

  let post_data1 = read_bytes "request.txt"
  let post_data2 = read_text "request-1.txt"
  let post_data3 = read_text "request-2.txt"

  let test_url_encoded_form fieldName =
    request (fun r ->
      match r.queryParam fieldName  with
      | Some str -> OK str
      | None -> OK "field-does-not-exists")

  let test_multipart_form =
    request (fun r ->
      match getFirst r.multiPartFields "From" with
      | Some str -> OK str
      | None -> OK "field-does-not-exists")

  let byte_array_content = new ByteArrayContent(post_data1)
  byte_array_content.Headers.TryAddWithoutValidation("Content-Type","multipart/form-data; boundary=99233d57-854a-4b17-905b-ae37970e8a39") |> ignore

  testList "http parser tests" [
      testCase "parsing a large multipart form" <| fun _ ->
        Assert.Equal("", "Bob <bob@wishfulcoding.mailgun.org>", runWith' test_multipart_form |> req HttpMethod.POST "/" (Some <| byte_array_content))

      testCase "parsing a large urlencoded form data" <| fun _ ->
        Assert.Equal("", "hallo wereld", 
          runWith' (test_url_encoded_form "stripped-text") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(post_data2, Encoding.UTF8, "application/x-www-form-urlencoded")))

      testCase "parsing a large urlencoded form data" <| fun _ ->
        Assert.Equal("", "Pepijn de Vos <pepijndevos@gmail.com>", 
          runWith' (test_url_encoded_form "from") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(post_data3, Encoding.UTF8, "application/x-www-form-urlencoded")))

      testCase "parsing a large urlencoded form data" <| fun _ ->
        Assert.Equal("", "no attachment 2", 
          runWith' (test_url_encoded_form "subject") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(post_data3, Encoding.UTF8, "application/x-www-form-urlencoded")))

      testCase "parsing a large urlencoded form data" <| fun _ ->
        Assert.Equal("", "identifier 123abc", 
          runWith' (test_url_encoded_form "body-plain") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(post_data3, Encoding.UTF8, "application/x-www-form-urlencoded")))

      testCase "parsing a large urlencoded form data" <| fun _ ->
        Assert.Equal("", "field-does-not-exists", 
          runWith' (test_url_encoded_form "body-html") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(post_data3, Encoding.UTF8, "application/x-www-form-urlencoded")))
  ]
