module Suave.Testing

(** For testing suave applications easily

Example:

  open Suave
  open Suave.Web
  open Suave.Types
  open Suave.Testing

  open Expecto

  let runWithConfig = runWith defaultConfig

  testCase "parsing a large multipart form" <| fun _ ->

    let res =
      runWithConfig testMultipartForm
      |> req HttpMethod.POST "/" (Some byteArrayContent)

    Expect.equal  "Bob <bob@wishfulcoding.mailgun.org>" ""

*)

open System
open System.Diagnostics
open System.Threading
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open Expecto
open Suave
open Suave.Logging
open Suave.Logging.Message
open Suave.Http

[<AutoOpen>]
module ResponseData =
  let responseHeaders (response : HttpResponseMessage) =
    response.Headers

  let contentHeaders (response : HttpResponseMessage) =
    response.Content.Headers

  let statusCode (response : HttpResponseMessage) =
    response.StatusCode

  let contentString (response : HttpResponseMessage) =
    response.Content.ReadAsStringAsync().Result

  let contentByteArray (response : HttpResponseMessage) =
    response.Content.ReadAsByteArrayAsync().Result

module Utilities =

  /// Utility function for mapping from Suave.Types.HttpMethod to
  /// System.Net.Http.HttpMethod.
  let toHttpMethod = function
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
  { cts         : CancellationTokenSource
    suaveConfig : SuaveConfig }

/// Cancels the cancellation token source and disposes the server's
/// resources.
let disposeContext (ctx : SuaveTestCtx) =
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
    suaveConfig = config2 }

/// Similar to run_with_factory, but uses the default suave factory.
let runWith config webParts = runWithFactory startWebServerAsync config webParts

/// Ensures the context is disposed after 'f ctx' is called.
let withContext f ctx =
  try
    f ctx
  finally disposeContext ctx

/// Create a new HttpRequestMessage towards the endpoint
let createRequest methd resource query data (endpoint : Uri) =
  let uriBuilder   = UriBuilder endpoint
  uriBuilder.Path  <- resource
  uriBuilder.Query <- query

  let request = new HttpRequestMessage(toHttpMethod methd, uriBuilder.Uri)
  request.Headers.ConnectionClose <- Nullable(true)
  data |> Option.iter (fun data -> request.Content <- data)
  request

/// Create a new disposable HttpClientHandler
let createHandler decomp_method cookies =
  let handler = new Net.Http.HttpClientHandler(AllowAutoRedirect = false)
  handler.AutomaticDecompression <- decomp_method
  cookies |> Option.iter (fun cookies -> handler.CookieContainer <- cookies)
  handler

let createClient handler =
  new HttpClient(handler)

/// Send the request with the client - returning the result of the request
let send (client : HttpClient) (timeout : TimeSpan) (ctx : SuaveTestCtx) (request : HttpRequestMessage) =
  ctx.suaveConfig.logger.verbose (
    eventX "Send"
    >> setFieldValue "method" request.Method.Method
    >> setFieldValue "uri" request.RequestUri)

  let send = client.SendAsync(request, HttpCompletionOption.ResponseContentRead, ctx.cts.Token)

  let completed = send.Wait (int timeout.TotalMilliseconds, ctx.cts.Token)
  if not completed && Debugger.IsAttached then Debugger.Break()
  else Expect.isTrue completed (sprintf "should finish request in %fms" timeout.TotalMilliseconds)

  send.Result

let endpointUri (suaveConfig : SuaveConfig) =
  Uri(suaveConfig.bindings.Head.ToString())

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
let reqResp
  (methd : HttpMethod)
  (resource : string)
  (query : string)
  data
  (cookies : CookieContainer option)
  (decompMethod : DecompressionMethods)
  (fRequest : HttpRequestMessage -> HttpRequestMessage)
  fResult =

  let defaultTimeout = TimeSpan.FromSeconds 10.

  withContext <| fun ctx ->
    use handler = createHandler decompMethod cookies
    use client = createClient handler
    use request = createRequest methd resource query data (endpointUri ctx.suaveConfig) |> fRequest
    use result = request |> send client defaultTimeout ctx
    fResult result

let req methd resource data =
  reqResp methd resource "" data None DecompressionMethods.None id contentString

let reqQuery methd resource query =
  reqResp methd resource query None None DecompressionMethods.None id contentString

let reqBytes methd resource data =
  reqResp methd resource "" data None DecompressionMethods.None id contentByteArray

let reqGZip methd resource data =
  reqResp methd resource "" data None DecompressionMethods.GZip id contentString

let reqDeflate methd resource data =
  reqResp methd resource "" data None DecompressionMethods.Deflate id contentString

let reqGZipBytes methd resource data =
  reqResp methd resource "" data None DecompressionMethods.GZip id contentByteArray

let reqDeflateBytes methd resource data =
  reqResp methd resource "" data None DecompressionMethods.Deflate id contentByteArray

let reqHeaders methd resource data =
  reqResp methd resource "" data None DecompressionMethods.None id responseHeaders

let reqContentHeaders methd resource data =
  reqResp methd resource "" data None DecompressionMethods.None id contentHeaders

let reqStatusCode methd resource data =
  reqResp methd resource "" data None DecompressionMethods.None id statusCode

/// Test a request by looking at the cookies alone.
let reqCookies methd resource data ctx =
  let cookies = new CookieContainer()
  reqResp
    methd resource "" data
    (Some cookies)
    DecompressionMethods.None
    id
    contentString
    ctx
  |> ignore // places stuff in the cookie container
  cookies

/// Returns the cookie collection for the default binding.
let reqCookies' methd resource data ctx =
  reqCookies methd resource data ctx
  |> fun cookies ->
    cookies.GetCookies(endpointUri ctx.suaveConfig)
