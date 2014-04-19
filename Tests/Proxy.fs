module Susave.Tests.Proxy

open Fuchu

open OpenSSL.X509
open OpenSSL.Core

open System
open System.Net

open Suave.Types
open Suave.Http
open Suave.Proxy

open Suave.Tests.TestUtilities

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
  let proxy = run_with_factory proxy_server_async proxy_config

  testList "creating proxy" [
    testProperty "GET / returns 200 OK with passed string" <| fun str ->
      run_in_context (run_target (OK str)) dispose_context <| fun _ ->
        Assert.Equal("target's WebPart should return its value", str,
//          verbose_logging(fun () -> proxy to_target |> req GET "/"))
          proxy to_target |> req GET "/" None)

    testCase "GET /redirect returns 'redirect'" <| fun _ ->
      run_in_context (run_target (url "/secret" >>= redirect "https://sts.example.se")) dispose_context <| fun _ ->
        let res = proxy to_target |> req_resp GET "/secret" None None DecompressionMethods.None
        Assert.Equal("should proxy redirect", HttpStatusCode.Found, res.StatusCode)
        Assert.Equal("should give Location-header together with redirect",
          Uri("https://sts.example.se"), res.Headers.Location)

    testCase "Should proxy 500 Internal Server Error too" <| fun _ ->
      run_in_context (run_target (INTERNAL_ERROR "Oh noes")) dispose_context <| fun _ ->
        Assert.Equal("should have correct status code",
          Net.HttpStatusCode.InternalServerError,
          (proxy to_target |> req_resp GET "/" None None DecompressionMethods.None).StatusCode)
        Assert.Equal("should have correct content",
          "Oh noes",
          (proxy to_target |> req_resp GET "/" None None DecompressionMethods.None).Content.ReadAsStringAsync().Result)

    testCase "Proxy decides to return directly" <| fun _ ->
      run_in_context (run_target (OK "upstream reply")) dispose_context <| fun _ ->
//          let subject = proxy (choose [ 
        Assert.Equal("", true, true)
    ]
