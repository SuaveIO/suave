module Suave.Tests

#nowarn "25"

module Resp =
  open System.Net.Http
  let content (r : HttpResponseMessage) = r.Content
  
open System
open System.Threading
open System.Net.Http

open Suave.Types
open Suave.Web
open Suave.Http

open Fuchu
open Resp

type Method =
  | GET
  | POST
  | DELETE
  | PUT
  | HEAD
  | CONNECT
  | PATCH
  | TRACE
  | OPTIONS

module RequestFactory =
  type SuaveTestCtx =
    { cts          : CancellationTokenSource
    ; suave_config : SuaveConfig }

  let dispose_context (ctx : SuaveTestCtx) =
    ctx.cts.Cancel()
    ctx.cts.Dispose()

  let verbose_logging f =
    Log.enable_trace <- true
    try
      f ()
    finally
      Log.enable_trace <- false

  let run_with_factory factory config web_parts : SuaveTestCtx =
    let binding = config.bindings.Head
    let base_uri = binding.ToString()
    let cts = new CancellationTokenSource()
    cts.Token.Register(fun () -> Log.trace(fun () -> "tests:run_with - cancelled")) |> ignore
    let config' = { config with ct = cts.Token; buffer_size = 128; max_ops = 10 }

    let listening, server = factory config web_parts
    Async.Start(server, cts.Token)
    Log.trace(fun () -> "tests:run_with_factory -> listening")
    listening |> Async.RunSynchronously // wait for the server to start listening
    Log.trace(fun () -> "tests:run_with_factory <- listening")

    { cts = cts
    ; suave_config = config' }

  let run_with = run_with_factory web_server_async

  let req_resp (methd : Method) (resource : string) data ctx =
    let to_http_method = function
      | Method.GET -> HttpMethod.Get
      | Method.POST -> HttpMethod.Post
      | Method.DELETE -> HttpMethod.Delete
      | Method.PUT-> HttpMethod.Put
      | Method.HEAD -> HttpMethod.Head
      | Method.TRACE -> HttpMethod.Trace
      | Method.OPTIONS -> HttpMethod.Options
      | _ -> failwithf "unsupported method %A by HttpClient" methd

    let server = ctx.suave_config.bindings.Head.ToString()
    let uri_builder = UriBuilder server
    uri_builder.Path <- resource
    use handler = new Net.Http.HttpClientHandler(AllowAutoRedirect = false)
    use client = new Net.Http.HttpClient(handler)
    let r = new HttpRequestMessage(to_http_method methd, uri_builder.Uri)
    r.Headers.ConnectionClose <- Nullable(true)
    Log.tracef(fun fmt -> fmt "tests:req %A %O -> execute" methd uri_builder.Uri)
    let get = 
      match data with
      | Some data ->
        client.PostAsync(uri_builder.Uri, data, ctx.cts.Token)
      | None ->
        client.SendAsync(r, HttpCompletionOption.ResponseContentRead, ctx.cts.Token)
    let completed = get.Wait(5000)
    if not completed && System.Diagnostics.Debugger.IsAttached then System.Diagnostics.Debugger.Break()
    else Assert.Equal("should finish request in 5000ms", true, completed)

    Log.tracef(fun fmt -> fmt "tests:req %A %O <- execute" methd uri_builder.Uri)

    dispose_context ctx
    get.Result

  let req methd resource data ctx =
    let res = req_resp methd resource data ctx
    res.Content.ReadAsStringAsync().Result

[<Tests>]
let smoking =
  testList "smoking hot" [
    testCase "smoke" <| fun _ -> Assert.Equal("smoke test", true, true)
  ]

[<Tests>]
let utilities =
  testList "trying some utility functions" [
    testCase "loopback ipv4" <| fun _ ->
      Assert.Equal("127.0.0.1 is a local address", true, is_local_address "127.0.0.1")

    testCase "loopback ipv6" <| fun _ ->
      Assert.Equal("::0 is a local address", true, is_local_address "::1")
  ]

open RequestFactory

[<Tests>]
let gets =
  let run_with' = run_with default_config

  testList "getting basic responses"
    [
      testCase "200 OK returns 'a'" <| fun _ ->
        Assert.Equal("expecting non-empty response", "a", run_with' (OK "a") |> req GET "/" None)

      testProperty "200 OK returns equivalent" <| fun resp_str ->
        (run_with' (OK resp_str) |> req GET "/hello" None) = resp_str

      testCase "204 No Content empty body" <| fun _ ->
        Assert.Equal("empty string should always be returned by 204 No Content",
                     "", (run_with' NO_CONTENT |> req GET "/" None))
    ]

[<Tests>]
let posts =
    let run_with' = run_with default_config

    let webId =
      choose [
        warbler (fun x -> OK (x.raw_form |> Text.Encoding.UTF8.GetString))
      ]

    let getFormValue name =
      choose [
        warbler (fun x -> OK (x.form.[name]))
      ] 

    let assertion = "eyJhbGciOiJSUzI1NiJ9.eyJwdWJsaWMta2V5Ijp7ImFsZ29yaXRobSI6IkRTIiwieSI6Ijc1MDMyNGRmYzQwNGI0OGQ3ZDg0MDdlOTI0NWMxNGVkZmVlZTYxOWY4ZmUxYWQxM2U5M2Y2ZmVlNjcxM2U5NjYxMjdlZTExNTZiYjIzZTBlMDJjODFhYWQwMGJhMGIzNzQxZjEzZDgzNTdkYjNkOTU0ZDMzNmFjZDU2YWIwN2NkMTQ4N2ZiNDlkYWFmM2RhY2JlODFhMDg5NjY5NzQyNTQwMTUwODM4N2E2M2Q4YTJlODQ5YzdiMDhiZTFhMWY0NzdiNDY0ZDQ1NDljZmQ0YTc4YWE4MDM2MzRhZGNhMmVlZDRmOWQzMmY5NTQ0OThhYWIyYjdkNTA2ZTAwZjI3ZjQiLCJwIjoiZmY2MDA0ODNkYjZhYmZjNWI0NWVhYjc4NTk0YjM1MzNkNTUwZDlmMWJmMmE5OTJhN2E4ZGFhNmRjMzRmODA0NWFkNGU2ZTBjNDI5ZDMzNGVlZWFhZWZkN2UyM2Q0ODEwYmUwMGU0Y2MxNDkyY2JhMzI1YmE4MWZmMmQ1YTViMzA1YThkMTdlYjNiZjRhMDZhMzQ5ZDM5MmUwMGQzMjk3NDRhNTE3OTM4MDM0NGU4MmExOGM0NzkzMzQzOGY4OTFlMjJhZWVmODEyZDY5YzhmNzVlMzI2Y2I3MGVhMDAwYzNmNzc2ZGZkYmQ2MDQ2MzhjMmVmNzE3ZmMyNmQwMmUxNyIsInEiOiJlMjFlMDRmOTExZDFlZDc5OTEwMDhlY2FhYjNiZjc3NTk4NDMwOWMzIiwiZyI6ImM1MmE0YTBmZjNiN2U2MWZkZjE4NjdjZTg0MTM4MzY5YTYxNTRmNGFmYTkyOTY2ZTNjODI3ZTI1Y2ZhNmNmNTA4YjkwZTVkZTQxOWUxMzM3ZTA3YTJlOWUyYTNjZDVkZWE3MDRkMTc1ZjhlYmY2YWYzOTdkNjllMTEwYjk2YWZiMTdjN2EwMzI1OTMyOWU0ODI5YjBkMDNiYmM3ODk2YjE1YjRhZGU1M2UxMzA4NThjYzM0ZDk2MjY5YWE4OTA0MWY0MDkxMzZjNzI0MmEzODg5NWM5ZDViY2NhZDRmMzg5YWYxZDdhNGJkMTM5OGJkMDcyZGZmYTg5NjIzMzM5N2EifSwicHJpbmNpcGFsIjp7ImVtYWlsIjoibWljaGFlbEBtYXZubi5jby51ayJ9LCJpYXQiOjEzODk3MzAwNTI4MDEsImV4cCI6MTM4OTgxNjQ1MjgwMSwiaXNzIjoibG9naW4ucGVyc29uYS5vcmcifQ.JDdYkcznYXIoOgqiTOHdMuRSc9aT-1MoU5AxfJFLObUs_jZeuEkqMtl7Ypdn7wDkdWNANnlR8OXpCe1Wqguaeyhz63XJilZP2u4T5_AHKmhyJ7d1ZPIwWjY4gKaZYAQY4m5KAQzJnWOPtdW3unEPYoPwVI1WzXSouLW-KlADV_eOP37W5Bgp81oj3zNWRrjiNCoQ6ZwgMmpODgj8e7fdbllbn73NBw6S8nIV4jzUn8P4d8ge6bnSenKApfa71N44E31HDRp8jvcXkBdVMllgjccowI9eKSBKdmWnYZ_Xzp12opzujNlPLmXFcO2a6xH1GB_I2sGy0xtWc37M03DVgg~eyJhbGciOiJEUzEyOCJ9.eyJleHAiOjEzODk3MzMzMzI2MTUsImF1ZCI6Imh0dHA6Ly9zbXMubG9jYWw6NzU3NSJ9.xbMyR2R7N9ZeLzqLWYw5hisaomZrtJlNdMvVdx0EaXxMkY7ocCpcpA"
    let longData = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"

    testList "posting basic data"
      [
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

open OpenSSL.X509
open OpenSSL.Core

[<Tests>]
let proxy =
  let bind :: _ = default_config.bindings
  let to_target r = Some(bind.ip, bind.port)

  let run_target = run_with default_config

  let run_in_context item f_finally f_body =
    try
      f_body item
    finally
      f_finally item

  // let sslCert = X509Certificate.FromPKCS12(BIO.File("suave.p12","r"), "easy")
  // let proxy_config = { default_config with bindings = [ HttpBinding.Create(Protocol.HTTPS(sslCert), "127.0.0.1", 8084) ] }
  let proxy_config = { default_config with bindings = [ HttpBinding.Create(Protocol.HTTP, "127.0.0.1", 8084) ] }
  let proxy = run_with_factory Proxy.proxy_server_async proxy_config

  testList "creating proxy" [
    testProperty "GET / returns 200 OK with passed string" <| fun str ->
      run_in_context (run_target (OK str)) dispose_context <| fun _ ->
        Assert.Equal("target's WebPart should return its value", str,
//          verbose_logging(fun () -> proxy to_target |> req GET "/"))
          proxy to_target |> req GET "/" None)

    testCase "GET /redirect returns 'redirect'" <| fun _ ->
      run_in_context (run_target (url "/secret" >>= redirect "https://sts.example.se")) dispose_context <| fun _ ->
        let res = proxy to_target |> req_resp GET "/secret" None
        Assert.Equal("should proxy redirect", Net.HttpStatusCode.Found, res.StatusCode)
        Assert.Equal("should give Location-header together with redirect",
          Uri("https://sts.example.se"), res.Headers.Location)

    testCase "Should proxy 500 Internal Server Error too" <| fun _ ->
      run_in_context (run_target (INTERNAL_ERROR "Oh noes")) dispose_context <| fun _ ->
        Assert.Equal("should have correct status code",
          Net.HttpStatusCode.InternalServerError,
          (proxy to_target |> req_resp GET "/" None).StatusCode)
        Assert.Equal("should have correct content",
          "Oh noes",
          (proxy to_target |> req_resp GET "/" None).Content.ReadAsStringAsync().Result)

    testCase "Proxy decides to return directly" <| fun _ ->
      run_in_context (run_target (OK "upstream reply")) dispose_context <| fun _ ->
//          let subject = proxy (choose [ 
        Assert.Equal("", true, true)
    ]

[<EntryPoint>]
let main args =
  let r = defaultMainThisAssembly args
  Console.ReadLine() |> ignore
  r
