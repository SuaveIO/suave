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
    let config' = { config with ct = cts.Token }

    let listening, server = factory config web_parts
    Async.Start(server, cts.Token)
    Log.trace(fun () -> "tests:run_with_factory -> listening")
    listening |> Async.RunSynchronously // wait for the server to start listening
    Log.trace(fun () -> "tests:run_with_factory <- listening")

    { cts = cts
    ; suave_config = config' }

  let run_with = run_with_factory web_server_async

  let req_resp (methd : Method) (resource : string) ctx =
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
    Log.tracef(fun fmt -> fmt "tests:req GET %O -> execute" uri_builder.Uri)

    let get = client.SendAsync(r, HttpCompletionOption.ResponseContentRead, ctx.cts.Token)
    let completed = get.Wait(5000)
    if not completed && System.Diagnostics.Debugger.IsAttached then System.Diagnostics.Debugger.Break()
    else Assert.Equal("should finish request in 5000ms", true, completed)

    Log.tracef(fun fmt -> fmt "tests:req GET %O <- execute" uri_builder.Uri)

    dispose_context ctx
    get.Result

  let req (methd : Method) (resource : string) ctx : string =
    let res = req_resp methd resource ctx
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
        Assert.Equal("expecting non-empty response", "a", run_with' (OK "a") |> req GET "/")

      testProperty "200 OK returns equivalent" <| fun resp_str ->
        (run_with' (OK resp_str) |> req GET "/hello") = resp_str

      testCase "204 No Content empty body" <| fun _ ->
        Assert.Equal("empty string should always be returned by 204 No Content",
                     "", (run_with' NO_CONTENT |> req GET "/"))
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
          proxy to_target |> req GET "/")

    testCase "GET /redirect returns 'redirect'" <| fun _ ->
      run_in_context (run_target (url "/secret" >>= redirect "https://sts.example.se")) dispose_context <| fun _ ->
        let res = proxy to_target |> req_resp GET "/secret"
        Assert.Equal("should proxy redirect", Net.HttpStatusCode.Found, res.StatusCode)
        Assert.Equal("should give Location-header together with redirect",
          Uri("https://sts.example.se"), res.Headers.Location)

    testCase "Should proxy 500 Internal Server Error too" <| fun _ ->
      run_in_context (run_target (INTERNAL_ERROR "Oh noes")) dispose_context <| fun _ ->
        Assert.Equal("should have correct status code",
          Net.HttpStatusCode.InternalServerError,
          (proxy to_target |> req_resp GET "/").StatusCode)
        Assert.Equal("should have correct content",
          "Oh noes",
          (proxy to_target |> req_resp GET "/").Content.ReadAsStringAsync().Result)
    ]

[<EntryPoint>]
let main args =
  let r = defaultMainThisAssembly args
  r
