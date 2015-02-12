module Suave.Tests.Model

open System.IO
open System.Text
open System.Net.Http
open System.Net.Http.Headers

open Fuchu

open Suave
open Suave.Types
open Suave.Model
open Suave.Types
open Suave.Http
open Suave.Http.Successful
open Suave.Http.RequestErrors
open Suave.Web

open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let tests =
  let runWith' = runWith defaultConfig

  let post_data3 = readText "request-2.txt"
  
  let test_url_encoded_form field_name : WebPart =
    Binding.bind_req (Binding.form field_name Choice1Of2) OK BAD_REQUEST

  testList "Suave.Model" [
    testCase "header" <| fun _ ->
      let expected = "application/vnd.lolcatz; version=1.0"
      let request  = { HttpRequest.empty with headers = [ "Content-Type", expected ] }

      let subject  = Binding.header "Content-Type" Choice1Of2 request
      Assert.Equal("should have bound Content-Type",
                   Choice1Of2 expected, subject)

      let subject  = Binding.header "Content-Encoding" Choice1Of2 request
      match subject with
      | Choice1Of2 _ ->
        Tests.failtest "didn't expect to bind non-existent header"
      | _ -> ()

    testCase "form" <| fun _ ->
      Assert.Equal(
        "should have read data",
        "identifier 123abc",
        runWith' (test_url_encoded_form "body-plain")
        |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(post_data3, Encoding.UTF8, "application/x-www-form-urlencoded")))

    testCase "200 OK returns 'a'" <| fun _ ->
      Assert.Equal(
        "expecting nilsson response",
        "nilsson",
        runWith' (Binding.bind_req (Binding.query "apa" Choice1Of2) OK BAD_REQUEST)
        |> reqQuery HttpMethod.GET "/" "apa=nilsson")
    ]