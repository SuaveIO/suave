/// A module for proxying requests to another computer/ip/port
module Suave.Proxy

open System
open System.IO
open System.Net
open System.Collections.Generic

open Suave.Utils
open Suave.Utils.Bytes

open Suave.Sockets
open Suave.Types
open Suave.Web
open Suave.Tcp

open Suave.Http
open Suave.Http.Response
open Suave.Web.ParsingAndControl


/// Copies the headers from 'headers1' to 'headers2'
let private toHeaderList (headers : WebHeaderCollection) =
  
  headers.AllKeys
  |> Seq.map (fun key -> key, headers.[key])
  |> List.ofSeq

/// Send the web response from HttpWebResponse to the HttpRequest 'p'
let private sendWebResponse (data : HttpWebResponse) ({ request = { trace = t }; response = resp } as ctx : HttpContext) =
  let headers = toHeaderList data.Headers 
  // TODO: if downstream sends a Content-Length header copy from one stream
  // to the other asynchronously
  "-> read_fully" |> Log.verbose ctx.runtime.logger "Suave.Proxy.send_web_response:GetResponseStream" t
  let bytes = data.GetResponseStream() |> readFully
  "<- read_fully" |> Log.verbose ctx.runtime.logger "Suave.Proxy.send_web_response:GetResponseStream" t
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
  let url = new UriBuilder("http", ip.ToString(), int port, p.url.AbsolutePath, p.rawQuery)
  let q = WebRequest.Create(url.Uri) :?> HttpWebRequest

  q.AllowAutoRedirect         <- false
  q.AllowReadStreamBuffering  <- false
  q.AllowWriteStreamBuffering <- false
  q.Method  <- string p.``method``
  q.Headers <- buildWebHeadersCollection p.headers
  q.Proxy   <- null

  //copy restricted headers
  let header s = getFirst p.headers s
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

  fun ctx -> socket {
    if p.``method`` = HttpMethod.POST || p.``method`` = HttpMethod.PUT then
      match p.headers %% "content-length" with
      | Some contentLength ->
        do! transferStreamBounded ctx.connection (q.GetRequestStream()) (int32 contentLength)
      | None -> ()
    try
      let! data = liftAsync <| q.AsyncGetResponse()
      let! res = liftAsync <| sendWebResponse ((data : WebResponse) :?> HttpWebResponse) ctx
      match res with
      | Some newCtx ->
        do! response_f newCtx
        return Some newCtx
      | None -> return None
    with
    | :? WebException as ex when ex.Response <> null ->
      let! res = liftAsync <| sendWebResponse (ex.Response :?> HttpWebResponse) ctx
      match res with
      | Some new_ctx ->
        do! response_f new_ctx
        return Some new_ctx
      | _ -> return None
    | :? WebException as ex when ex.Response = null ->
      let! res = liftAsync <|response HTTP_502 (UTF8.bytes "suave proxy: Could not connect to upstream") ctx
      match res with
      | Some new_ctx ->
        do! response_f new_ctx
        return Some new_ctx
      | _ -> return None
  } |> succeed

/// Proxy the HttpRequest 'r' with the proxy found with 'proxy_resolver'
let private proxy proxyResolver (r : HttpContext) =
  match proxyResolver r.request with
  | Some (ip, port) -> forward ip port r
  | None            -> failwith "invalid request."

/// Run a proxy server with the given configuration and given upstream/target
/// resolver.
let createReverseProxyServerAsync (config : SuaveConfig) resolver =
  let homeFolder, compressionFolder =
    ParsingAndControl.resolveDirectory config.homeFolder,
    Path.Combine(ParsingAndControl.resolveDirectory config.compressedFilesFolder, "_temporary_compressed_files")
  let all =
    [ for binding in config.bindings do 
        let reqLoop = ParsingAndControl.requestLoop (SuaveConfig.toRuntime config homeFolder compressionFolder false binding )  (SocketPart (proxy resolver))
        let server = startTcpIpServerAsync (config.bufferSize, config.maxOps) config.logger reqLoop binding.socketBinding
        yield server ]
      
  let listening = all |> List.map fst |> Async.Parallel |> Async.Ignore
  let server    = all |> List.map snd |> Async.Parallel |> Async.Ignore
  listening, server

let startReverseProxyServer config resolver =
  Async.RunSynchronously(createReverseProxyServerAsync config resolver |> snd,
    cancellationToken = config.cancellationToken)

/// Obsolete
[<System.Obsolete("Use createReverseProxyServerAsync")>]
let proxy_server_async config resolver = createReverseProxyServerAsync config resolver

/// Obsolete
[<System.Obsolete("Use startReverseProxyServer")>]
let proxy_server config resolver = startReverseProxyServer config resolver
