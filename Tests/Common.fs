module Suave.Tests.Common

open System
open System.Text
open System.Net.Http

open Suave
open Suave.Types
open Suave.Web
open Suave.Http

open Suave.Tests.TestUtilities

open Fuchu

[<Tests>]
let ``canonicalization attacks`` =
  testList "canonicalization attacks" [
    testCase "should throw" <| fun _ ->
      Assert.Raise("'../../passwd' is not a valid path",
        typeof<Exception>,
        fun _ -> Files.local_file "../../passwd" current_path |> ignore)

//    testCase "should throw" <| fun _ ->
//      Assert.Raise("'..\..\passwd' is not a valid path", typeof<Exception>, fun _ -> local_file "..\..\passwd" current_path |> ignore)
  ]

open RequestFactory

open Successful

[<Tests>]
let gets =
  let run_with' = run_with default_config
  testList "getting basic responses" [
      testCase "200 OK returns 'a'" <| fun _ ->
        Assert.Equal("expecting non-empty response", "a", run_with' (OK "a") |> req GET "/" None)

      testProperty "200 OK returns equivalent" <| fun resp_str ->
        (run_with' (OK resp_str) |> req GET "/hello" None) = resp_str

      testCase "204 No Content empty body" <| fun _ ->
        Assert.Equal("empty string should always be returned by 204 No Content",
                     "", (run_with' NO_CONTENT |> req GET "/" None))

      testCase "302 FOUND sends content-length header" <| fun _ ->
        Assert.Equal("302 FOUND sends content-length header",
                     true, (req_headers GET "/" None (run_with' (Redirection.FOUND "/url"))).Contains("Content-Length"))
    ]

[<Tests>]
let posts =
//  let loggers = Loggers.sane_defaults_for Log.LogLevel.Debug
  let run_with' = run_with default_config

  let webId =
    request (fun x -> OK (x.raw_form |> Text.Encoding.UTF8.GetString))

  let getFormValue name =
    request (fun x -> OK (x.form.[name]))

  let assertion = "eyJhbGciOiJSUzI1NiJ9.eyJwdWJsaWMta2V5Ijp7ImFsZ29yaXRobSI6IkRTIiwieSI6Ijc1MDMyNGRmYzQwNGI0OGQ3ZDg0MDdlOTI0NWMxNGVkZmVlZTYxOWY4ZmUxYWQxM2U5M2Y2ZmVlNjcxM2U5NjYxMjdlZTExNTZiYjIzZTBlMDJjODFhYWQwMGJhMGIzNzQxZjEzZDgzNTdkYjNkOTU0ZDMzNmFjZDU2YWIwN2NkMTQ4N2ZiNDlkYWFmM2RhY2JlODFhMDg5NjY5NzQyNTQwMTUwODM4N2E2M2Q4YTJlODQ5YzdiMDhiZTFhMWY0NzdiNDY0ZDQ1NDljZmQ0YTc4YWE4MDM2MzRhZGNhMmVlZDRmOWQzMmY5NTQ0OThhYWIyYjdkNTA2ZTAwZjI3ZjQiLCJwIjoiZmY2MDA0ODNkYjZhYmZjNWI0NWVhYjc4NTk0YjM1MzNkNTUwZDlmMWJmMmE5OTJhN2E4ZGFhNmRjMzRmODA0NWFkNGU2ZTBjNDI5ZDMzNGVlZWFhZWZkN2UyM2Q0ODEwYmUwMGU0Y2MxNDkyY2JhMzI1YmE4MWZmMmQ1YTViMzA1YThkMTdlYjNiZjRhMDZhMzQ5ZDM5MmUwMGQzMjk3NDRhNTE3OTM4MDM0NGU4MmExOGM0NzkzMzQzOGY4OTFlMjJhZWVmODEyZDY5YzhmNzVlMzI2Y2I3MGVhMDAwYzNmNzc2ZGZkYmQ2MDQ2MzhjMmVmNzE3ZmMyNmQwMmUxNyIsInEiOiJlMjFlMDRmOTExZDFlZDc5OTEwMDhlY2FhYjNiZjc3NTk4NDMwOWMzIiwiZyI6ImM1MmE0YTBmZjNiN2U2MWZkZjE4NjdjZTg0MTM4MzY5YTYxNTRmNGFmYTkyOTY2ZTNjODI3ZTI1Y2ZhNmNmNTA4YjkwZTVkZTQxOWUxMzM3ZTA3YTJlOWUyYTNjZDVkZWE3MDRkMTc1ZjhlYmY2YWYzOTdkNjllMTEwYjk2YWZiMTdjN2EwMzI1OTMyOWU0ODI5YjBkMDNiYmM3ODk2YjE1YjRhZGU1M2UxMzA4NThjYzM0ZDk2MjY5YWE4OTA0MWY0MDkxMzZjNzI0MmEzODg5NWM5ZDViY2NhZDRmMzg5YWYxZDdhNGJkMTM5OGJkMDcyZGZmYTg5NjIzMzM5N2EifSwicHJpbmNpcGFsIjp7ImVtYWlsIjoibWljaGFlbEBtYXZubi5jby51ayJ9LCJpYXQiOjEzODk3MzAwNTI4MDEsImV4cCI6MTM4OTgxNjQ1MjgwMSwiaXNzIjoibG9naW4ucGVyc29uYS5vcmcifQ.JDdYkcznYXIoOgqiTOHdMuRSc9aT-1MoU5AxfJFLObUs_jZeuEkqMtl7Ypdn7wDkdWNANnlR8OXpCe1Wqguaeyhz63XJilZP2u4T5_AHKmhyJ7d1ZPIwWjY4gKaZYAQY4m5KAQzJnWOPtdW3unEPYoPwVI1WzXSouLW-KlADV_eOP37W5Bgp81oj3zNWRrjiNCoQ6ZwgMmpODgj8e7fdbllbn73NBw6S8nIV4jzUn8P4d8ge6bnSenKApfa71N44E31HDRp8jvcXkBdVMllgjccowI9eKSBKdmWnYZ_Xzp12opzujNlPLmXFcO2a6xH1GB_I2sGy0xtWc37M03DVgg~eyJhbGciOiJEUzEyOCJ9.eyJleHAiOjEzODk3MzMzMzI2MTUsImF1ZCI6Imh0dHA6Ly9zbXMubG9jYWw6NzU3NSJ9.xbMyR2R7N9ZeLzqLWYw5hisaomZrtJlNdMvVdx0EaXxMkY7ocCpcpA"
  let longData = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"

  testList "posting basic data" [
      testCase "POST data round trips with no content-type" <| fun _ ->
        use data = new StringContent("bob")
        Assert.Equal("expecting data to be returned", "bob", run_with' webId |> req POST "/" (Some data))
      testCase "POST form data makes round trip" <| fun _ ->
        use data = new FormUrlEncodedContent(dict [ "name", "bob"])
        Assert.Equal("expecting form data to be returned", "bob", run_with' (getFormValue "name") |> req POST "/" (Some data))
      testCase "POST long data" <| fun _ ->
        use data = new FormUrlEncodedContent(dict [ "long", longData])
        Assert.Equal("expecting form data to be returned", longData, run_with' (getFormValue "long") |> req POST "/" (Some data))
      testCase "POST persona assertion" <| fun _ ->
        use data = new FormUrlEncodedContent(dict [ "assertion", assertion])
        Assert.Equal("expecting form data to be returned", assertion, run_with' (getFormValue "assertion") |> req POST "/" (Some data))
    ]

open Writers

[<Tests>]
let cookies =
  let run_with' = run_with default_config

  let basic_cookie =
    { name      = "mycookie"
    ; value     = "42"
    ; expires   = None
    ; domain    = None
    ; path      = Some "/"
    ; http_only = false
    ; secure    = false
    ; version   = None }

  testList "Cookies basic tests" [
      testCase "cookie data makes round trip" <| fun _ ->
        Assert.Equal("expecting cookie value"
        , "42"
        , (req_cookies GET "/" None
          (run_with' (set_cookie basic_cookie >> succeed >>= OK "test")))
            .GetCookies(Uri("http://127.0.0.1")).[0].Value)

      testCase "cookie name makes round trip" <| fun _ ->
        Assert.Equal("expecting cookie name"
        , "mycookie"
        , (req_cookies GET "/" None
          (run_with' (set_cookie basic_cookie >> succeed >>= OK "test")))
            .GetCookies(Uri("http://127.0.0.1")).[0].Name)

      testCase "http_only cookie is http_only" <| fun _ ->
        Assert.Equal("expecting http_only"
        , true
        , (req_cookies GET "/" None
          (run_with' (set_cookie { basic_cookie with http_only = true } >> succeed >>= OK "test")))
            .GetCookies(Uri("http://127.0.0.1")).[0].HttpOnly)
    ]

open System.IO
open System.Text

[<Tests>]
let compression =
  let run_with' = run_with default_config

  let test_file_size = (new FileInfo(Path.Combine(current_path,"test-text-file.txt"))).Length

  testList "getting basic gzip/deflate responses" [
      testCase "200 OK returns 'Havana' with gzip " <| fun _ ->
        Assert.Equal("expecting 'Havana'", "Havana", run_with' (OK "Havana") |> req_gzip GET "/" None)

      testCase "200 OK returns 'Havana' with deflate " <| fun _ ->
        Assert.Equal("expecting 'Havana'", "Havana", run_with' (OK "Havana") |> req_deflate GET "/" None)

      testCase "verifiying we get the same size uncompressed" <| fun _ ->
        Assert.Equal("length should match"
        , test_file_size
        , (run_with' (Files.browse_file "test-text-file.txt") |> req_bytes GET "/" None).Length |> int64)

      testCase "gzip static file" <| fun _ ->
        Assert.Equal("length should match"
        , test_file_size
        , (run_with' (Files.browse_file "test-text-file.txt") |> req_gzip_bytes GET "/" None).Length |> int64 )

      // this one is not running
      (*
      testCase "deflate static file" <| fun _ ->
        Assert.Equal("length should match"
        , test_file_size
        , (run_with' (browse_file "test-text-file.txt") |> req_deflate_bytes GET "/" None).Length |> int64 )
        *)
    ]
