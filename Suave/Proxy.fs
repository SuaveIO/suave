/// A module for proxying requests to another computer/ip/port
module Suave.Proxy

open System
open System.Net
open System.Collections.Generic

open Suave.Types
open Suave.Http
open Suave.Web
open Suave.Tcp
open Socket

/// Copies the headers from 'headers1' to 'headers2'
let private copy_response_headers (headers1 : WebHeaderCollection) (headers2 : List<string*string>) =
  for e in headers1 do
    headers2.Add(e, headers1.[e])

/// Send the web response from HttpWebResponse to the HttpRequest 'p'
let private send_web_response (data : HttpWebResponse)
                              ({ request = { response = resp; trace = t }
                               ; runtime = { logger = logger }} as ctx : HttpContext) = async {
  copy_response_headers data.Headers resp.Headers
  // TODO: if downstream sends a Content-Length header copy from one stream
  // to the other asynchronously
  "-> read_fully" |> Log.verbose logger "proxy:send_web_response:GetResponseStream" t
  let bytes = data.GetResponseStream() |> read_fully
  "<- read_fully" |> Log.verbose logger "proxy:send_web_response:GetResponseStream" t
  do! response (int data.StatusCode) (data.StatusDescription) bytes ctx
}

/// Forward the HttpRequest 'p' to the 'ip':'port'
let forward (ip : IPAddress) (port : uint16) (ctx : HttpContext) =
  let p = ctx.request
  let buildWebHeadersCollection (h : Dictionary<string,string>) =
    let r = new WebHeaderCollection()
    for e in h do
      if not (WebHeaderCollection.IsRestricted e.Key) then
        r.Add(e.Key, e.Value)
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
  let header = look_up p.headers
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

  q.Headers.Add("X-Forwarded-For", ctx.connection.ipaddr.ToString())

  async {
    if p.``method`` = "POST" || p.``method`` = "PUT" then
      let content_length = Convert.ToInt32(p.headers.["content-length"])
      do! transfer_len_x ctx.connection (q.GetRequestStream()) content_length
    try
      let! data = q.AsyncGetResponse()
      do! send_web_response ((data : WebResponse) :?> HttpWebResponse) ctx
    with
    | :? WebException as ex when ex.Response <> null ->
      do! send_web_response (ex.Response :?> HttpWebResponse) ctx
    | :? WebException as ex when ex.Response = null ->
      do! response 502 "Bad Gateway" (bytes_utf8 "suave proxy: Could not connect to upstream") ctx
  } |> succeed

/// Proxy the HttpRequest 'r' with the proxy found with 'proxy_resolver'
let proxy proxy_resolver (r : HttpRequest) =
  match proxy_resolver r with
  | Some (ip, port) -> forward ip port
  | None            -> never

open System.IO

/// Run a proxy server with the given configuration and given upstream/target
/// resolver.
let proxy_server_async (config : SuaveConfig) resolver =
  let home_dir = ParsingAndControl.resolve_directory config.home_folder
  let compression_folder = Path.Combine(ParsingAndControl.resolve_directory config.compressed_files_folder, "_temporary_compressed_files")
  let mk_runtime proto =
    ParsingAndControl.mk_http_runtime
      proto config.web_part_timeout config.error_handler config.mime_types_map
      home_dir compression_folder config.logger

  let all =
    config.bindings
    |> List.map (fun { scheme = proto; ip = ip; port = port } ->
        tcp_ip_server
          (ip, port, config.buffer_size, config.max_ops)
          config.logger
          (ParsingAndControl.request_loop (ParsingAndControl.process_request true)
            (mk_runtime proto)
            (request (proxy resolver))))
  let listening = all |> Seq.map fst |> Async.Parallel |> Async.Ignore
  let server    = all |> Seq.map snd |> Async.Parallel |> Async.Ignore
  listening, server

let proxy_server config resolver =
  Async.RunSynchronously(proxy_server_async config resolver |> snd,
    cancellationToken = config.ct)
