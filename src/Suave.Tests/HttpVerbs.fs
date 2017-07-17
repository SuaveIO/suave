module Suave.Tests.HttpVerbs

open System
open System.Text
open System.Net.Http
open Suave
open Suave.Successful
open Suave.Utils
open Suave.Tests.TestUtilities
open Suave.Testing
open Expecto

[<Tests>]
let gets cfg =
  let runWithConfig = runWith cfg
  testList "getting basic responses" [
    testCase "200 OK returns 'a'" <| fun _ ->
      let actual = runWithConfig (OK "a") |> req HttpMethod.GET "/" None
      Expect.equal actual "a" "expecting non-empty response"

    testCase "200 OK returning url" <| fun _ ->
      Assert.Equal("expecting correct url from binding",
                   SuaveConfig.firstBindingUri cfg "/" "" |> string,
                   runWithConfig (request (fun r -> OK (r.url.ToString())))
                   |> req HttpMethod.GET "/" None)

    testPropertyWithConfig fsCheckConfig "200 OK returns equivalent" <| fun respStr ->
      (runWithConfig (OK respStr) |> req HttpMethod.GET "/hello" None) = respStr

    testCase "204 No Content empty body" <| fun _ ->
      Assert.Equal("empty string should always be returned by 204 No Content",
                   "", (runWithConfig NO_CONTENT |> req HttpMethod.GET "/" None))

    testCase "204 No Content must not sends content-length header" <| fun _ ->
      let headers = reqContentHeaders HttpMethod.GET "/" None (runWithConfig NO_CONTENT)
      Assert.Equal("204 No Content must not sends content-length header",
                   false,
                   headers.Contains("Content-Length"))

    testCase "302 FOUND sends content-length header" <| fun _ ->
      let headers = reqContentHeaders HttpMethod.GET "/" None (runWithConfig (Redirection.FOUND "/url"))
      Assert.Equal("302 FOUND sends content-length header",
                   true,
                   headers.Contains("Content-Length"))
    ]

let longData = String.replicate 1815 "A"

[<Tests>]
let posts cfg =
  let runWithConfig = runWith cfg

  let get = function Choice1Of2 x -> x | _ -> failwith "couldn't get it"

  let webId =
    request (fun x -> OK (x.rawForm |> Encoding.UTF8.GetString))

  let getFormValue name =
    request (fun x -> let q = x.formData name in OK (get q))

  let getMultiPartValue name =
    request (fun x -> let q = x.fieldData name in OK (get q))

  let getFileContent _ =
    request (fun x -> let q = List.head x.files in OK (IO.File.ReadAllText q.tempFilePath))

  let getFileName _ =
    request (fun x -> let q = List.head x.files in OK q.fileName)

  let assertion = "eyJhbGciOiJSUzI1NiJ9.eyJwdWJsaWMta2V5Ijp7ImFsZ29yaXRobSI6IkR"+
                  "TIiwieSI6Ijc1MDMyNGRmYzQwNGI0OGQ3ZDg0MDdlOTI0NWMxNGVkZmVlZTY"+
                  "xOWY4ZmUxYWQxM2U5M2Y2ZmVlNjcxM2U5NjYxMjdlZTExNTZiYjIzZTBlMDJ"+
                  "jODFhYWQwMGJhMGIzNzQxZjEzZDgzNTdkYjNkOTU0ZDMzNmFjZDU2YWIwN2N"+
                  "kMTQ4N2ZiNDlkYWFmM2RhY2JlODFhMDg5NjY5NzQyNTQwMTUwODM4N2E2M2Q"+
                  "4YTJlODQ5YzdiMDhiZTFhMWY0NzdiNDY0ZDQ1NDljZmQ0YTc4YWE4MDM2MzR"+
                  "hZGNhMmVlZDRmOWQzMmY5NTQ0OThhYWIyYjdkNTA2ZTAwZjI3ZjQiLCJwIjo"+
                  "iZmY2MDA0ODNkYjZhYmZjNWI0NWVhYjc4NTk0YjM1MzNkNTUwZDlmMWJmMmE"+
                  "5OTJhN2E4ZGFhNmRjMzRmODA0NWFkNGU2ZTBjNDI5ZDMzNGVlZWFhZWZkN2U"+
                  "yM2Q0ODEwYmUwMGU0Y2MxNDkyY2JhMzI1YmE4MWZmMmQ1YTViMzA1YThkMTd"+
                  "lYjNiZjRhMDZhMzQ5ZDM5MmUwMGQzMjk3NDRhNTE3OTM4MDM0NGU4MmExOGM"+
                  "0NzkzMzQzOGY4OTFlMjJhZWVmODEyZDY5YzhmNzVlMzI2Y2I3MGVhMDAwYzN"+
                  "mNzc2ZGZkYmQ2MDQ2MzhjMmVmNzE3ZmMyNmQwMmUxNyIsInEiOiJlMjFlMDR"+
                  "mOTExZDFlZDc5OTEwMDhlY2FhYjNiZjc3NTk4NDMwOWMzIiwiZyI6ImM1MmE"+
                  "0YTBmZjNiN2U2MWZkZjE4NjdjZTg0MTM4MzY5YTYxNTRmNGFmYTkyOTY2ZTN"+
                  "jODI3ZTI1Y2ZhNmNmNTA4YjkwZTVkZTQxOWUxMzM3ZTA3YTJlOWUyYTNjZDV"+
                  "kZWE3MDRkMTc1ZjhlYmY2YWYzOTdkNjllMTEwYjk2YWZiMTdjN2EwMzI1OTM"+
                  "yOWU0ODI5YjBkMDNiYmM3ODk2YjE1YjRhZGU1M2UxMzA4NThjYzM0ZDk2MjY"+
                  "5YWE4OTA0MWY0MDkxMzZjNzI0MmEzODg5NWM5ZDViY2NhZDRmMzg5YWYxZDd"+
                  "hNGJkMTM5OGJkMDcyZGZmYTg5NjIzMzM5N2EifSwicHJpbmNpcGFsIjp7ImV"+
                  "tYWlsIjoibWljaGFlbEBtYXZubi5jby51ayJ9LCJpYXQiOjEzODk3MzAwNTI"+
                  "4MDEsImV4cCI6MTM4OTgxNjQ1MjgwMSwiaXNzIjoibG9naW4ucGVyc29uYS5"+
                  "vcmcifQ.JDdYkcznYXIoOgqiTOHdMuRSc9aT-1MoU5AxfJFLObUs_jZeuEkq"+
                  "Mtl7Ypdn7wDkdWNANnlR8OXpCe1Wqguaeyhz63XJilZP2u4T5_AHKmhyJ7d1"+
                  "ZPIwWjY4gKaZYAQY4m5KAQzJnWOPtdW3unEPYoPwVI1WzXSouLW-KlADV_eO"+
                  "P37W5Bgp81oj3zNWRrjiNCoQ6ZwgMmpODgj8e7fdbllbn73NBw6S8nIV4jzU"+
                  "n8P4d8ge6bnSenKApfa71N44E31HDRp8jvcXkBdVMllgjccowI9eKSBKdmWn"+
                  "YZ_Xzp12opzujNlPLmXFcO2a6xH1GB_I2sGy0xtWc37M03DVgg~eyJhbGciO"+
                  "iJEUzEyOCJ9.eyJleHAiOjEzODk3MzMzMzI2MTUsImF1ZCI6Imh0dHA6Ly9z"+
                  "bXMubG9jYWw6NzU3NSJ9.xbMyR2R7N9ZeLzqLWYw5hisaomZrtJlNdMvVdx0"+
                  "EaXxMkY7ocCpcpA"

  let unicodeString1 =  "Testing «ταБЬℓσ»: 1<2 & 4+1>3, now 20% off!;"
  let unicodeString2 =  "文档内容"

  testList "posting basic data" [
    testCase "POST data round trips with no content-type" <| fun _ ->
      use data = new StringContent("bob")
      let actual = runWithConfig webId |> req HttpMethod.POST "/" (Some data)
      Expect.equal actual "bob" "expecting data to be returned"

    testCase "POST form data makes round trip" <| fun _ ->
      use data = new FormUrlEncodedContent(dict [ "name", "bob"])
      let actual = runWithConfig (getFormValue "name") |> req HttpMethod.POST "/" (Some data)
      Expect.equal actual "bob" "expecting form data to be returned"

    testCase "POST long data" <| fun _ ->
      use data = new FormUrlEncodedContent(dict [ "long", longData])
      let actual = runWithConfig (getFormValue "long") |> req HttpMethod.POST "/" (Some data)
      Expect.equal actual longData "expecting form data to be returned"

    testCase "POST persona assertion" <| fun _ ->
      use data = new FormUrlEncodedContent(dict [ "assertion", assertion])
      let actual = runWithConfig (getFormValue "assertion") |> req HttpMethod.POST "/" (Some data)
      Expect.equal actual assertion "expecting form data to be returned"

    testCase "POST unicode data" <| fun _ ->
      use data = new FormUrlEncodedContent(dict [ "name", unicodeString1 ])
      let actual = runWithConfig (getFormValue "name") |> req HttpMethod.POST "/" (Some data)
      Expect.equal actual unicodeString1 "expecting form data to be returned"

    testCase "POST unicode multipart" <| fun _ ->
      let multipart = new MultipartFormDataContent()
      let data = new StringContent(unicodeString1)
      multipart.Add(data, "name")
      let actual = runWithConfig (getMultiPartValue "name") |> req HttpMethod.POST "/" (Some multipart)
      Expect.equal actual unicodeString1 "expecting form data to be returned"

    testCase "POST unicode multipart (Chinese)" <| fun _ ->
      let multipart = new MultipartFormDataContent()
      let data = new StringContent(unicodeString2)
      multipart.Add(data, "name")
      let actual = runWithConfig (getMultiPartValue "name") |> req HttpMethod.POST "/" (Some multipart)
      Expect.equal actual unicodeString2 "expecting form data to be returned"

    testCase "POST multipart file with unicode file-name" <| fun _ ->
      let multipart = new MultipartFormDataContent()
      let fileContent = "there is no cake"
      let data = new ByteArrayContent(UTF8.bytes fileContent)
      data.Headers.ContentType <- Headers.MediaTypeHeaderValue.Parse "text/html"
      multipart.Add(data,"attached-file","文档内容.txt")
      let actual = runWithConfig (getFileContent ()) |> req HttpMethod.POST "/" (Some multipart)
      Expect.equal actual fileContent "expecting File to be saved and recovered"

    testCase "POST unicode file-name is parsed correctly" <| fun _ ->
      let multipart = new MultipartFormDataContent()
      let fileContent = "there is no cake"
      let filename = "文档内容.txt"
      let data = new ByteArrayContent(UTF8.bytes fileContent)
      data.Headers.ContentType <- Headers.MediaTypeHeaderValue.Parse "text/html"
      multipart.Add(data,"attached-file",filename)
      let actual = runWithConfig (getFileName ()) |> req HttpMethod.POST "/" (Some multipart)
      Expect.equal actual filename "expecting to return correct unicode file name"
  ]

[<Tests>]
let testMaxContentLength cfg =
  let runWithConfig = runWith { cfg with maxContentLength = 100 }

  testList "test maxContentLength" [

    testCase "POST larger than maxContentLength" <| fun _ ->
      use data = new StringContent(longData)
      let actual = runWithConfig (OK "response") |> req HttpMethod.POST "/" (Some data)
      Expect.equal actual "Payload too large" "expecting data to be returned"

    ]

[<Tests>]
let testDELETE cfg =
  let runWithConfig = runWith cfg

  let webId =
    request (fun x -> OK (x.rawForm |> Encoding.UTF8.GetString))

  testList "testing DELETE with body" [
    testCase "body is parsed" <| fun _ ->
      use data = new StringContent("bob")
      let actual = runWithConfig webId |> req HttpMethod.DELETE "/" (Some data)
      Expect.equal actual "bob" "expecting data to be returned" ]
