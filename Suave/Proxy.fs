/// A module for proxying requests to another computer/ip/port
module Suave.Proxy

open System
open System.Net
open System.Collections.Generic

open Suave.Utils.Bytes

open Suave.Types
open Suave.Types.Codes
open Suave.Web
open Suave.Tcp

open Suave.Http
open Suave.Http.Response
open Suave.Web.ParsingAndControl

open Socket

/// Copies the headers from 'headers1' to 'headers2'
let private to_header_list (headers : WebHeaderCollection) =
  
  headers.AllKeys
  |> Seq.map (fun key -> key, headers.[key])
  |> List.ofSeq

/// Send the web response from HttpWebResponse to the HttpRequest 'p'
let private send_web_response (data : HttpWebResponse) ({ request = { trace = t }; response = resp } as ctx : HttpContext) =
  let headers = to_header_list data.Headers 
  // TODO: if downstream sends a Content-Length header copy from one stream
  // to the other asynchronously
  "-> read_fully" |> Log.verbose ctx.runtime.logger "proxy:send_web_response:GetResponseStream" t
  let bytes = data.GetResponseStream() |> read_fully
  "<- read_fully" |> Log.verbose ctx.runtime.logger "proxy:send_web_response:GetResponseStream" t
  response (HttpCode.TryParse(int data.StatusCode) |> Option.get) bytes { ctx with response = { resp with headers = resp.headers @ headers } }

/// Forward the HttpRequest 'p' to the 'ip':'port'
let forward (ip : IPAddress) (port : uint16) (ctx : HttpContext) =
  let p = ctx.request
  let buildWebHeadersCollection (h : NameValueList) =
    let r = new WebHeaderCollection()
    for e in h do
      let key = fst e
      if not (WebHeaderCollection.IsRestricted key) then
        r.Add(key, snd e)
    r
  let url = new UriBuilder("http", ip.ToString(), int port, p.url, p.raw_query)
  let q = WebRequest.Create(url.Uri) :?> HttpWebRequest

  q.AllowAutoRedirect         <- false
  q.AllowReadStreamBuffering  <- false
  q.AllowWriteStreamBuffering <- false
  q.Method  <- p.``method``
  q.Headers <- buildWebHeadersCollection p.headers
  q.Proxy   <- null

  //copy restricted headers
  let header s = get_first p.headers s
  header "accept"            |> Option.iter (fun v -> q.Accept <- v)
  header "date"              |> Option.iter (fun v -> q.Date <- DateTime.Parse v)
  header "expect"            |> Option.iter (fun v -> q.Expect <- v)
  header "host"              |> Option.iter (fun v -> q.Host <- v)
  header "range"             |> Option.iter (fun v -> q.AddRange(Int64.Parse v))
  header "referer"           |> Option.iter (fun v -> q.Referer <- v)
  header "content-type"      |> Option.iter (fun v -> q.ContentType <- v)
  header "content-length"    |> Option.iter (fun v -> q.ContentLength <- Int64.Parse(v))
  header "if-modified-since" |> Option.iter (fun v -> q.IfModifiedSince <- DateTime.Parse v)
  header "transfer-encoding" |> Option.iter (fun v -> q.TransferEncoding <- v)
  header "user-agent"        |> Option.iter (fun v -> q.UserAgent <- v)

  q.Headers.Add("X-Forwarded-For", p.ipaddr.ToString())

  fun cn -> socket {
    if p.``method`` = "POST" || p.``method`` = "PUT" then
      let content_length = Convert.ToInt32(p.headers %% "content-length")
      do! transfer_len_x cn (q.GetRequestStream()) content_length
    try
      let! data = lift_async <| q.AsyncGetResponse()
      let! res = lift_async <| send_web_response ((data : WebResponse) :?> HttpWebResponse) ctx
      match res with
      | Some new_ctx ->
        do! response_f new_ctx cn
      | None -> ()
    with
    | :? WebException as ex when ex.Response <> null ->
      let! res = lift_async <| send_web_response (ex.Response :?> HttpWebResponse) ctx
      match res with
      | Some new_ctx ->
        do! response_f new_ctx cn
      | _ -> ()
    | :? WebException as ex when ex.Response = null ->
      let! res = lift_async <|response HTTP_502 (UTF8.bytes "suave proxy: Could not connect to upstream") ctx
      match res with
      | Some new_ctx ->
        do! response_f new_ctx cn
      | _ -> ()
  } |> succeed

/// Proxy the HttpRequest 'r' with the proxy found with 'proxy_resolver'
let proxy proxy_resolver (r : HttpContext) =
  match proxy_resolver r.request with
  | Some (ip, port) -> forward ip port r
  | None            -> failwith "invalid request."

open System.IO

/// Run a proxy server with the given configuration and given upstream/target
/// resolver.
let proxy_server_async (config : SuaveConfig) resolver =
  let home_dir = ParsingAndControl.resolve_directory config.home_folder
  let compression_folder = Path.Combine(ParsingAndControl.resolve_directory config.compressed_files_folder, "_temporary_compressed_files")
  let mk_runtime proto =
    ParsingAndControl.mk_http_runtime
      proto config.error_handler config.mime_types_map
      home_dir compression_folder config.logger config.session_provider

  let all =
    config.bindings
    |> List.map (fun { scheme = proto; ip = ip; port = port } ->
        tcp_ip_server
          (ip, port, config.buffer_size, config.max_ops)
          config.logger
          (ParsingAndControl.request_loop true
            (mk_runtime proto)
            (SocketPart (proxy resolver))))
  let listening = all |> Seq.map fst |> Async.Parallel |> Async.Ignore
  let server    = all |> Seq.map snd |> Async.Parallel |> Async.Ignore
  listening, server

let proxy_server config resolver =
  Async.RunSynchronously(proxy_server_async config resolver |> snd,
    cancellationToken = config.ct)
