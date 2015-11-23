module Susave.Tests.Proxy

#nowarn "25"

open Fuchu

open System
open System.Net

open Suave
open Suave.Types
open Suave.Http.Successful
open Suave.Http.Redirection
open Suave.Http.ServerErrors
open Suave.Http.Applicatives
open Suave.Http
open Suave.Proxy

open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let proxy cfg =
  let bind :: _ = cfg.bindings
  let toTarget r = Some (bind.socketBinding.ip, bind.socketBinding.port)

  let runTarget = runWith cfg

  let runInContext item f_finally f_body =
    try
      f_body item
    finally
      f_finally item

  // let sslCert = X509Certificate.FromPKCS12(BIO.File("suave.p12","r"), "easy")
  // let proxy_config = { default_config with bindings = [ HttpBinding.Create(Protocol.HTTPS(sslCert), "127.0.0.1", 8084) ] }
  let proxyConfig =
    { cfg with
        bindings = [ HttpBinding.mk HTTP IPAddress.Loopback 8084us ] }
  let proxy = runWithFactory createReverseProxyServerAsync proxyConfig

  testList "creating proxy" [
    testPropertyWithConfig fsCheckConfig "GET / returns 200 OK with passed string" <| fun str ->
      runInContext (runTarget (Successful.OK str)) disposeContext <| fun _ ->
        Assert.Equal("target's WebPart should return its value", str,
          proxy toTarget |> req HttpMethod.GET "/" None)

    testCase "GET /redirect returns 'redirect'" <| fun _ ->
      runInContext (runTarget (path "/secret" >>= redirect "https://sts.example.se")) disposeContext <| fun _ ->
        let headers, stat =
          proxy toTarget |> reqResp HttpMethod.GET "/secret" "" None None DecompressionMethods.None id
            (fun r -> r.Headers, r.StatusCode)
        Assert.Equal("should proxy redirect", HttpStatusCode.Found, stat)
        Assert.Equal("should give Location-header together with redirect",
          Uri("https://sts.example.se"), headers.Location)

    testCase "Should proxy 500 Internal Server Error too" <| fun _ ->
      runInContext (runTarget (INTERNAL_ERROR "Oh noes")) disposeContext <| fun _ ->
        Assert.Equal("should have correct status code",
          HttpStatusCode.InternalServerError,
          proxy toTarget |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None id statusCode)
        Assert.Equal("should have correct content",
          "Oh noes",
          proxy toTarget |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None id contentString)

    testCase "Proxy decides to return directly" <| fun _ ->
      runInContext (runTarget (OK "upstream reply")) disposeContext <| fun _ ->
//          let subject = proxy (choose [ 
        Assert.Equal("", true, true)
    ]
