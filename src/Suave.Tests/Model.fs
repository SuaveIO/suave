module Suave.Tests.Model

open System.IO
open System.Text
open System.Net.Http
open System.Net.Http.Headers

open Expecto

open Suave
open Suave.Model
open Suave.Successful
open Suave.RequestErrors

open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let modelTests cfg =
  let runWithConfig = runWith cfg

  let postData3 = readText "request-2.txt"
  
  let testUrlEncodedForm fieldName : WebPart =
    Binding.bindReq (Binding.form fieldName Choice1Of2) OK BAD_REQUEST

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
        runWithConfig (testUrlEncodedForm "body-plain")
        |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(postData3, Encoding.UTF8, "application/x-www-form-urlencoded")))

    testCase "200 OK returns 'a'" <| fun _ ->
      Assert.Equal(
        "expecting nilsson response",
        "nilsson",
        runWithConfig (Binding.bindReq (Binding.query "apa" Choice1Of2) OK BAD_REQUEST)
        |> reqQuery HttpMethod.GET "/" "apa=nilsson")
    ]