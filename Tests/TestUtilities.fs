module Suave.Tests.TestUtilities

#nowarn "25"

open System
open System.Threading
open System.Net.Http
open System.Net.Http.Headers

open Suave
open Suave.Types
open Suave.Web
open Suave.Http
open Suave.Log

open Fuchu

let current_path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
let default_config = { default_config with logger = Loggers.sane_defaults_for Log.LogLevel.Warn }

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

  let req_resp (methd : HttpMethod) (resource : string) data (cookies : Net.CookieContainer option) (decompressionMethod : Net.DecompressionMethods) ctx =
    let to_http_method = function
      | GET -> HttpMethod.Get
      | POST -> HttpMethod.Post
      | DELETE -> HttpMethod.Delete
      | PUT-> HttpMethod.Put
      | HEAD -> HttpMethod.Head
      | TRACE -> HttpMethod.Trace
      | OPTIONS -> HttpMethod.Options
      | _ -> failwithf "unsupported method %A by HttpClient" methd

    let server = ctx.suave_config.bindings.Head.ToString()
    let uri_builder = UriBuilder server
    uri_builder.Path <- resource
    use handler = new Net.Http.HttpClientHandler(AllowAutoRedirect = false)
    handler.AutomaticDecompression <- decompressionMethod
    match cookies with | Some cnt -> handler.CookieContainer <- cnt | _ -> ()
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

    dispose_context ctx
    get.Result

  let req methd resource data ctx =
    let res = req_resp methd resource data None Net.DecompressionMethods.None ctx
    res.Content.ReadAsStringAsync().Result

  let req_bytes methd resource data ctx =
    let res = req_resp methd resource data None Net.DecompressionMethods.None ctx
    res.Content.ReadAsByteArrayAsync().Result

  let req_gzip methd resource data ctx =
    let res = req_resp methd resource data None Net.DecompressionMethods.GZip ctx
    res.Content.ReadAsStringAsync().Result

  let req_deflate methd resource data ctx =
    let res = req_resp methd resource data None Net.DecompressionMethods.Deflate ctx
    res.Content.ReadAsStringAsync().Result

  let req_gzip_bytes methd resource data ctx =
    let res = req_resp methd resource data None Net.DecompressionMethods.GZip ctx
    res.Content.ReadAsByteArrayAsync().Result

  let req_deflate_bytes methd resource data ctx =
    let res = req_resp methd resource data None Net.DecompressionMethods.Deflate ctx
    res.Content.ReadAsByteArrayAsync().Result

  let req_headers methd resource data ctx =
    let res = req_resp methd resource data None Net.DecompressionMethods.None ctx
    res.Content.Headers

  let req_cookies methd resource data ctx =
    let cookies = new Net.CookieContainer()
    let res = req_resp methd resource data (Some cookies) Net.DecompressionMethods.None ctx
    cookies
