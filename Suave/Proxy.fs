/// A module for proxying requests to another computer/ip/port
module Suave.Proxy

open System
open System.Net
open System.Collections.Generic

open Suave.Types
open Suave.Http
open Suave.Web
open Suave.Tcp

/// Copies the headers from 'headers1' to 'headers2'
let private copy_response_headers (headers1 : WebHeaderCollection) (headers2 : List<string*string>) =
  for e in headers1 do
    headers2.Add(e, headers1.[e])

/// Send the web response from HttpWebResponse to the HttpRequest 'p'
let private send_web_response (data : HttpWebResponse) (p : HttpRequest)  = async {
  copy_response_headers data.Headers (p.Response.Headers)
  //TODO: if downstream sends a Content-Length header copy from one stream to the other asynchronously
  let bytes = data.GetResponseStream() |> read_fully
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
  let url = new UriBuilder("http", ip.ToString(), int port, p.Url, p.RawQuery)
  let q = WebRequest.Create(url.Uri) :?> HttpWebRequest
  q.AllowAutoRedirect         <- false
  q.AllowReadStreamBuffering  <- false
  q.AllowWriteStreamBuffering <- false
  // q.UserAgent <- p.UserAgent
  q.Method  <- p.Method
  q.Headers <- buildWebHeadersCollection p.Headers
  q.Proxy   <- null
  //copy restricted headers
  match p.Headers ? accept with Some(v) -> q.Accept <- v | None -> ()
  //match p.Headers ? connection with Some(v) -> q.Connection <- v | None -> ()
  match p.Headers ? date with Some(v) -> q.Date <- DateTime.Parse(v) | None -> ()
  match p.Headers ? expect with Some(v) -> q.Expect <- v | None -> ()
  match p.Headers ? host with Some(v) -> q.Host <- v | None -> ()
  match p.Headers ? range with Some(v) -> q.AddRange(Int64.Parse(v)) | None -> ()
  match p.Headers ? referer with Some(v) -> q.Referer <- v | None -> ()
  match look_up p.Headers "content-type" with Some(v) -> q.ContentType <- v | None -> ()
  match look_up p.Headers "content-length" with Some(v) -> q.ContentLength <- Int64.Parse(v) | None -> ()
  match look_up p.Headers "if-modified-since" with Some(v) -> q.IfModifiedSince <- DateTime.Parse(v) | None -> ()
  match look_up p.Headers "transfer-encoding" with Some(v) -> q.TransferEncoding <- v | None -> ()
  match look_up p.Headers "user-agent" with Some(v) -> q.UserAgent <- v | None -> ()
  q.Headers.Add("X-Forwarded-For", p.RemoteAddress)
  async {
    if p.Method = "POST" then
      let content_length = Convert.ToInt32(p.Headers.["content-length"])
      let req = q.GetRequestStream()
      do! p.Stream.CopyToAsync req
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

/// Run a proxy server with the given configuration and given proxy resolver.
let proxy_server_async config resolver =
  let all =
    config.bindings
    |> List.map (fun { scheme = proto; ip = ip; port = port } ->
        tcp_ip_server (ip,port) (request_loop (warbler (fun http -> proxy resolver http)) proto (process_request true) config.error_handler config.timeout))
  let listening = all |> Seq.map fst |> Async.Parallel |> Async.Ignore
  let server    = all |> Seq.map snd |> Async.Parallel |> Async.Ignore
  listening, server

let proxy_server config resolver =
  Async.RunSynchronously(async {
    let listening, server = proxy_server_async config resolver
    do! listening
    do! server },
    cancellationToken = config.ct)
