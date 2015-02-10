(** For testing suave applications easily

Example:

  open Suave
  open Suave.Web
  open Suave.Types
  open Suave.Testing

  open Fuchu

  let run_with' = run_with default_config
  
  testCase "parsing a large multipart form" <| fun _ ->

    let res =
      run_with' test_multipart_form
      |> req HttpMethod.POST "/" (Some <| byte_array_content)

    Assert.Equal("", "Bob <bob@wishfulcoding.mailgun.org>", )

*)
module Suave.Testing

open System
open System.Diagnostics
open System.Threading
open System.Net
open System.Net.Http
open System.Net.Http.Headers

open Fuchu

open Suave
open Suave.Types
open Suave.Web

[<AutoOpen>]
module ResponseData =
  let response_headers (response : HttpResponseMessage) =
    response.Headers

  let content_headers (response : HttpResponseMessage) =
    response.Content.Headers

  let status_code (response : HttpResponseMessage) =
    response.StatusCode

  let content_string (response : HttpResponseMessage) =
    response.Content.ReadAsStringAsync().Result

  let content_byte_array (response : HttpResponseMessage) =
    response.Content.ReadAsByteArrayAsync().Result

module Utilities =
    
  /// Utility function for mapping from Suave.Types.HttpMethod to
  /// System.Net.Http.HttpMethod.
  let to_http_method = function
    | HttpMethod.GET -> HttpMethod.Get
    | HttpMethod.POST -> HttpMethod.Post
    | HttpMethod.DELETE -> HttpMethod.Delete
    | HttpMethod.PUT-> HttpMethod.Put
    | HttpMethod.HEAD -> HttpMethod.Head
    | HttpMethod.TRACE -> HttpMethod.Trace
    | HttpMethod.OPTIONS -> HttpMethod.Options
    | HttpMethod.PATCH -> failwithf "PATCH not a supported method in HttpClient"
    | HttpMethod.CONNECT -> failwithf "CONNECT not a supported method in the unit tests"
    | HttpMethod.OTHER x -> failwithf "%A not a supported method" x

open Utilities

/// This test context is a holder for the runtime values of the web
/// server of suave, as well as the cancellation token that is
/// threaded throughout the web server and will shut down all
/// concurrently running async operations.
///
/// When you are done with it, you should call `dispose_context` to
/// cancel the token and dispose the server's runtime artifacts
/// (like the listening socket etc).
type SuaveTestCtx =
  { cts          : CancellationTokenSource
    suave_config : SuaveConfig }

/// Cancels the cancellation token source and disposes the server's
/// resources.
let dispose_context (ctx : SuaveTestCtx) =
  ctx.cts.Cancel()
  ctx.cts.Dispose()

/// Create a new test context from a factory that starts the web
/// server, such as `web_server_async` from `Suave.Web`. Also pass
/// in a `SuaveConfig` value and the web parts you'd like to test.
///
/// The factory needs to start two async's, one which this function
/// can block on (listening) and another (server) which is the actual
/// async value of the running server. The listening async value will
/// be awaited inside this function but the server async value will
/// be run on the thread pool.
let runWithFactory factory config webParts : SuaveTestCtx =
  let binding = config.bindings.Head
  let baseUri = binding.ToString()
  let cts = new CancellationTokenSource()
  let config2 = { config with cancellationToken = cts.Token; bufferSize = 128; maxOps = 10 }

  let listening, server = factory config webParts
  Async.Start(server, cts.Token)
  listening |> Async.RunSynchronously |> ignore // wait for the server to start listening

  { cts = cts
    suave_config = config2 }

/// Similar to run_with_factory, but uses the default suave factory.
let runWith = runWithFactory createWebServerAsync

/// Ensures the context is disposed after 'f ctx' is called.
let with_context f ctx =
  try
    f ctx
  finally dispose_context ctx

/// Create a new HttpRequestMessage towards the endpoint
let mk_request methd resource query data (endpoint : Uri) =
  let uri_builder   = UriBuilder endpoint
  uri_builder.Path  <- resource
  uri_builder.Query <- query

  let request = new HttpRequestMessage(to_http_method methd, uri_builder.Uri)
  request.Headers.ConnectionClose <- Nullable(true)
  data |> Option.iter (fun data -> request.Content <- data)
  request

/// Create a new disposable HttpClientHandler
let mk_handler decomp_method cookies =
  let handler = new Net.Http.HttpClientHandler(AllowAutoRedirect = false)
  handler.AutomaticDecompression <- decomp_method
  cookies |> Option.iter (fun cookies -> handler.CookieContainer <- cookies)
  handler

let mk_client handler =
  new HttpClient(handler)

/// Send the request with the client - returning the result of the request
let send (client : HttpClient) (timeout : TimeSpan) (ctx : SuaveTestCtx) (request : HttpRequestMessage) =
  Log.intern ctx.suave_config.logger "Suave.Tests"
             (sprintf "%s %O"  request.Method.Method request.RequestUri)
  let send = client.SendAsync(request, HttpCompletionOption.ResponseContentRead, ctx.cts.Token)

  let completed = send.Wait (int timeout.TotalMilliseconds, ctx.cts.Token)
  if not completed && Debugger.IsAttached then Debugger.Break()
  else Assert.Equal("should finish request in 5000ms", true, completed)

  send.Result

let endpoint_uri (suave_config : SuaveConfig) =
  Uri(suave_config.bindings.Head.ToString())

/// This is the main function for the testing library; it lets you assert
/// on the request/response values while ensuring deterministic
/// disposal of suave.
///
/// Currently, it:
///
///  - doesn't automatically follow 301 FOUND redirects (nor 302, 307) to
///    ensure you can assert on redirects.
///  - only requests to the very first binding your web server has in use
///  - only sets a HttpContent if you have given a value to the `data`
///    parameter.
///  - waits 5000 ms for a reply, then breaks into the debugger if you're
///    attached, otherwise asserts a failure of the timeout
///  - calls `f_result` with the HttpResponseMessage
///
let req_resp
  (methd : HttpMethod)
  (resource : string)
  (query : string)
  data
  (cookies : CookieContainer option)
  (decomp_method : DecompressionMethods)
  (f_request : HttpRequestMessage -> HttpRequestMessage)
  f_result =

  let default_timeout = TimeSpan.FromSeconds 5.

  with_context <| fun ctx ->
    use handler = mk_handler decomp_method cookies
    use client = mk_client handler
    use request = mk_request methd resource query data (endpoint_uri ctx.suave_config) |> f_request
    use result = request |> send client default_timeout ctx
    f_result result

let req methd resource data =
  req_resp methd resource "" data None DecompressionMethods.None id content_string

let req_query methd resource query =
  req_resp methd resource query None None DecompressionMethods.None id content_string

let req_bytes methd resource data =
  req_resp methd resource "" data None DecompressionMethods.None id content_byte_array

let req_gzip methd resource data =
  req_resp methd resource "" data None DecompressionMethods.GZip id content_string

let req_deflate methd resource data =
  req_resp methd resource "" data None DecompressionMethods.Deflate id content_string

let req_gzip_bytes methd resource data =
  req_resp methd resource "" data None DecompressionMethods.GZip id content_byte_array

let req_deflate_bytes methd resource data =
  req_resp methd resource "" data None DecompressionMethods.Deflate id content_byte_array

let req_headers methd resource data =
  req_resp methd resource "" data None DecompressionMethods.None id response_headers

let req_content_headers methd resource data =
  req_resp methd resource "" data None DecompressionMethods.None id content_headers

/// Test a request by looking at the cookies alone.
let req_cookies methd resource data ctx =
  let cookies = new CookieContainer()
  req_resp
    methd resource "" data
    (Some cookies)
    DecompressionMethods.None
    id
    content_string
    ctx
  |> ignore // places stuff in the cookie container
  cookies

/// Returns the cookie collection for the default binding.
let req_cookies' methd resource data ctx =
  req_cookies methd resource data ctx
  |> fun cookies ->
    cookies.GetCookies(endpoint_uri ctx.suave_config)