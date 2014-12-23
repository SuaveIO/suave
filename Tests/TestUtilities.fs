module Suave.Tests.TestUtilities

#nowarn "25"

open System
open System.IO
open System.Threading
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Reflection

open Suave
open Suave.Types
open Suave.Web
open Suave.Http
open Suave.Log

open FsCheck

open Fuchu

type Arbs =
  static member String () = Arb.Default.String () |> Arb.filter (fun str -> str <> null)
let fscheck_config = { Config.Default with Arbitrary = [ typeof<Arbs> ] }

let current_path =
  Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

let read_text relative_path =
  File.ReadAllText(Path.Combine(current_path, relative_path))

let read_bytes relative_path =
  File.ReadAllBytes(Path.Combine(current_path, relative_path))

let default_config = { default_config with logger = Loggers.sane_defaults_for Log.LogLevel.Warn }

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

[<AutoOpen>]
module RequestFactory =
  type SuaveTestCtx =
    { cts          : CancellationTokenSource
    ; suave_config : SuaveConfig }

  let dispose_context (ctx : SuaveTestCtx) =
    ctx.cts.Cancel()
    ctx.cts.Dispose()

  let run_with_factory factory config web_parts : SuaveTestCtx =
    let binding = config.bindings.Head
    let base_uri = binding.ToString()
    let cts = new CancellationTokenSource()
    let config' = { config with ct = cts.Token; buffer_size = 128; max_ops = 10 }

    let listening, server = factory config web_parts
    Async.Start(server, cts.Token)
    listening |> Async.RunSynchronously |> ignore // wait for the server to start listening

    { cts = cts
    ; suave_config = config' }

  let run_with = run_with_factory web_server_async

  let to_http_method = function
    | GET -> HttpMethod.Get
    | POST -> HttpMethod.Post
    | DELETE -> HttpMethod.Delete
    | PUT-> HttpMethod.Put
    | HEAD -> HttpMethod.Head
    | TRACE -> HttpMethod.Trace
    | OPTIONS -> HttpMethod.Options
    | methd -> failwithf "unsupported method %A by HttpClient" methd

  /// ensures the context is disposed after 'f ctx' is called
  let with_context f ctx =
    try
      f ctx
    finally dispose_context ctx

  let req_resp
    (methd : HttpMethod)
    (resource : string)
    (query : string)
    data
    (cookies : Net.CookieContainer option)
    (decompressionMethod : Net.DecompressionMethods)
    f_result =

    with_context <| fun ctx ->
      let server = ctx.suave_config.bindings.Head.ToString()
      let uri_builder   = UriBuilder server
      uri_builder.Path  <- resource
      uri_builder.Query <- query

      use handler = new Net.Http.HttpClientHandler(AllowAutoRedirect = false)
      handler.AutomaticDecompression <- decompressionMethod
      cookies |> Option.iter (fun cookies -> handler.CookieContainer <- cookies)

      use client = new Net.Http.HttpClient(handler)

      let r = new HttpRequestMessage(to_http_method methd, uri_builder.Uri)
      r.Headers.ConnectionClose <- Nullable(true)

      let get =
        match data with
        | Some data ->
          client.PostAsync(uri_builder.Uri, data, ctx.cts.Token)
        | None ->
          client.SendAsync(r, HttpCompletionOption.ResponseContentRead, ctx.cts.Token)

      let completed = get.Wait(5000)
      if not completed && System.Diagnostics.Debugger.IsAttached then System.Diagnostics.Debugger.Break()
      else Assert.Equal("should finish request in 5000ms", true, completed)

      use r = get.Result
      f_result r

  let req methd resource data =
    req_resp methd resource "" data None DecompressionMethods.None content_string

  let req_query methd resource query =
    req_resp methd resource query None None DecompressionMethods.None content_string

  let req_bytes methd resource data =
    req_resp methd resource "" data None DecompressionMethods.None content_byte_array

  let req_gzip methd resource data =
    req_resp methd resource "" data None DecompressionMethods.GZip content_string

  let req_deflate methd resource data =
    req_resp methd resource "" data None DecompressionMethods.Deflate content_string

  let req_gzip_bytes methd resource data =
    req_resp methd resource "" data None DecompressionMethods.GZip content_byte_array

  let req_deflate_bytes methd resource data =
    req_resp methd resource "" data None DecompressionMethods.Deflate content_byte_array

  let req_headers methd resource data =
    req_resp methd resource "" data None DecompressionMethods.None response_headers

  let req_content_headers methd resource data =
    req_resp methd resource "" data None DecompressionMethods.None content_headers

  let req_cookies methd resource data ctx =
    let cookies = new CookieContainer()
    req_resp
      methd resource "" data
      (Some cookies)
      DecompressionMethods.None
      id ctx
    |> ignore // places stuff in the cookie container
    cookies
