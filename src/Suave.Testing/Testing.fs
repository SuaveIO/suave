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
      run_with' (OK "hi")
      |> req HttpMethod.GET "/" None

    Assert.Equal("", "hi", res)

*)
module Suave.Testing

open System
open System.Threading

open HttpClient

open Fuchu

open Suave
open Suave.Types
open Suave.Web

[<AutoOpen>]
module ResponseData =
  let response_headers (request : Request) =
    request
    |> getResponse
    |> fun resp -> resp.Headers

  let status_code (resp : Response) =
    resp.StatusCode

  let content_string (resp : Response) =
    resp.EntityBody |> Option.get

  let content_byte_array (resp : Response) =
    res

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
    | HttpMethod.CONNECT -> HttpMethod.Connect
    | HttpMethod.PATCH -> failwithf "PATCH not a supported method in HttpClient"
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
let run_with_factory factory config web_parts : SuaveTestCtx =
  let binding = config.bindings.Head
  let base_uri = binding.ToString()
  let cts = new CancellationTokenSource()
  let config' = { config with ct = cts.Token; buffer_size = 128; max_ops = 10 }

  let listening, server = factory config web_parts
  Async.Start(server, cts.Token)
  listening |> Async.RunSynchronously |> ignore // wait for the server to start listening

  { cts = cts
    suave_config = config' }

/// Similar to run_with_factory, but uses the default suave factory.
let run_with = run_with_factory web_server_async

/// Ensures the context is disposed after 'f ctx' is called.
let with_context f ctx =
  try
    f ctx
  finally dispose_context ctx

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
  (cookies : NameValue list)
  (decompressionMethod : DecompressionScheme)
  f_result =

  with_context <| fun ctx ->
    let server = ctx.suave_config.bindings.Head.ToString()
    let uri_builder   = UriBuilder server
    uri_builder.Path  <- resource
    uri_builder.Query <- query

    let get =
      createRequest (to_http_method methd) (uri_builder.Uri.ToString())
      |> withAutoDecompression decompressionMethod
      |> fun r ->
        cookies |> List.fold (fun s c -> s |> withCookie c) r
      |> withKeepAlive false
      |> fun r ->
        match data with
        | None -> r
        | Some body -> r |> withBody body
      |> getResponseAsync
      |> fun ras -> Async.StartAsTask(ras, cancellationToken = ctx.cts.Token)

    let completed = get.Wait(5000)
    if not completed && System.Diagnostics.Debugger.IsAttached then System.Diagnostics.Debugger.Break()
    else Assert.Equal("should finish request in 5000ms", true, completed)

    f_result (get.Result)

let req methd resource data =
  req_resp methd resource "" data [] DecompressionScheme.None content_string

let req_query methd resource query =
  req_resp methd resource query None [] DecompressionScheme.None content_string

let req_bytes methd resource data =
  req_resp methd resource "" data [] DecompressionScheme.None content_byte_array

let req_gzip methd resource data =
  req_resp methd resource "" data [] DecompressionScheme.GZip content_string

let req_deflate methd resource data =
  req_resp methd resource "" data [] DecompressionScheme.Deflate content_string

let req_gzip_bytes methd resource data =
  req_resp methd resource "" data [] DecompressionScheme.GZip content_byte_array

let req_deflate_bytes methd resource data =
  req_resp methd resource "" data [] DecompressionScheme.Deflate content_byte_array

let req_headers methd resource data =
  req_resp methd resource "" data [] DecompressionScheme.None response_headers

let req_content_headers methd resource data =
  req_resp methd resource "" data [] DecompressionScheme.None content_headers

/// Test a request by looking at the cookies alone.
let req_cookies methd resource data ctx =
  let cookies = new CookieContainer()
  req_resp
    methd resource "" data
    (Some cookies)
    Compression.Plain
    id ctx
  |> ignore // places stuff in the cookie container
  cookies
