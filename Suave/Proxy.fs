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
let private send_web_response (data : HttpWebResponse) (p : HttpRequest)  = async {
  copy_response_headers data.Headers (p.response.Headers)
  //TODO: if downstream sends a Content-Length header copy from one stream to the other asynchronously
  Log.trace (fun () -> "proxy:send_web_response:GetResponseStream -> read_fully")
  let bytes = data.GetResponseStream() |> read_fully
  Log.trace (fun () -> "proxy:send_web_response:GetResponseStream <- read_fully")
  do! response (int data.StatusCode) (data.StatusDescription) bytes p
}

/// Forward the HttpRequest 'p' to the 'ip':'port'
let forward (ip : IPAddress) (port : uint16) (p : HttpRequest) =
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
  match p.headers ? accept with Some(v) -> q.Accept <- v | None -> ()
  //match p.Headers ? connection with Some(v) -> q.Connection <- v | None -> ()
  match p.headers ? date with Some(v) -> q.Date <- DateTime.Parse(v) | None -> ()
  match p.headers ? expect with Some(v) -> q.Expect <- v | None -> ()
  match p.headers ? host with Some(v) -> q.Host <- v | None -> ()
  match p.headers ? range with Some(v) -> q.AddRange(Int64.Parse(v)) | None -> ()
  match p.headers ? referer with Some(v) -> q.Referer <- v | None -> ()
  match look_up p.headers "content-type" with Some(v) -> q.ContentType <- v | None -> ()
  match look_up p.headers "content-length" with Some(v) -> q.ContentLength <- Int64.Parse(v) | None -> ()
  match look_up p.headers "if-modified-since" with Some(v) -> q.IfModifiedSince <- DateTime.Parse(v) | None -> ()
  match look_up p.headers "transfer-encoding" with Some(v) -> q.TransferEncoding <- v | None -> ()
  match look_up p.headers "user-agent" with Some(v) -> q.UserAgent <- v | None -> ()
  q.Headers.Add("X-Forwarded-For", p.remote_address .ToString())
  async {
    if p.``method`` = "POST" || p.``method`` = "PUT" then
      let content_length = Convert.ToInt32(p.headers.["content-length"])
      do! transfer_len_x p.connection (q.GetRequestStream()) content_length
    try
      let! data = q.AsyncGetResponse()
      do! send_web_response ((data : WebResponse) :?> HttpWebResponse) p
    with
    | :? WebException as ex when ex.Response <> null ->
      do! send_web_response (ex.Response :?> HttpWebResponse) p
    | :? WebException as ex when ex.Response = null ->
      do! response 502 "Bad Gateway" (bytes_utf8 "suave proxy: Could not connect to upstream") p
  } |> succeed

/// Proxy the HttpRequest 'r' with the proxy found with 'proxy_resolver'
let proxy proxy_resolver (r : HttpRequest) =
  match proxy_resolver r with
  | Some (ip, port) -> forward ip port
  | None            -> never

/// Run a proxy server with the given configuration and given upstream/target
/// resolver.
let proxy_server_async config resolver =
  let all =
    config.bindings
    |> List.map (fun { scheme = proto; ip = ip; port = port } ->
        tcp_ip_server (ip, port, config.buffer_size, config.max_ops) (request_loop (process_request true) proto (warbler (fun http -> proxy resolver http)) config.web_part_timeout config.error_handler))
  let listening = all |> Seq.map fst |> Async.Parallel |> Async.Ignore
  let server    = all |> Seq.map snd |> Async.Parallel |> Async.Ignore
  listening, server

let proxy_server config resolver =
  Async.RunSynchronously(proxy_server_async config resolver |> snd,
    cancellationToken = config.ct)
