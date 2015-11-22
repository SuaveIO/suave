/// A module for proxying requests to another computer/ip/port
module Suave.Proxy

open System
open System.IO
open System.Net
open System.Collections.Generic

open Suave.Utils
open Suave.Utils.Bytes

open Suave.Sockets
open Suave.Sockets.Control
open Suave.Http
open Suave.Web
open Suave.Tcp

open Suave.Http.Operators
open Suave.Http.Response
open Suave.Web.ParsingAndControl

/// Copies the headers from 'headers1' to 'headers2'
let private toHeaderList (headers : WebHeaderCollection) =
  headers.AllKeys
  |> Seq.map (fun key -> key, headers.[key])
  |> List.ofSeq

/// Send the web response from HttpWebResponse to the HttpRequest 'p'
let private sendWebResponse (data : HttpWebResponse) ({ request = { trace = t }; response = resp } as ctx : HttpContext) = async {
  let headers = toHeaderList data.Headers 
  "-> readFully" |> Log.verbose ctx.runtime.logger "Suave.Proxy.sendWebResponse:GetResponseStream" ctx.request.trace
  use ms = new MemoryStream()
  do! data.GetResponseStream().CopyToAsync ms
  let bytes = ms.ToArray()
  "<- readFully" |> Log.verbose ctx.runtime.logger "Suave.Proxy.sendWebResponse:GetResponseStream" ctx.request.trace

  let ctxNext =
    { ctx with response = { resp with headers = resp.headers @ headers } }

  let composed = 
    response (HttpCode.TryParse(int data.StatusCode) |> Option.get) bytes

  return! composed ctxNext
  }

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
  q.Method                    <- string p.``method``
  q.Headers                   <- buildWebHeadersCollection p.headers
  q.Proxy                     <- null

  //copy restricted headers
  let header s = getFirst p.headers s
  header "accept"            |> Choice.iter (fun v -> q.Accept <- v)
  header "date"              |> Choice.iter (fun v -> q.Date <- DateTime.Parse v)
  header "expect"            |> Choice.iter (fun v -> q.Expect <- v)
  header "host"              |> Choice.iter (fun v -> q.Host <- v)
  header "range"             |> Choice.iter (fun v -> q.AddRange(Int64.Parse v))
  header "referer"           |> Choice.iter (fun v -> q.Referer <- v)
  header "content-type"      |> Choice.iter (fun v -> q.ContentType <- v)
  header "content-length"    |> Choice.iter (fun v -> q.ContentLength <- Int64.Parse(v))
  header "if-modified-since" |> Choice.iter (fun v -> q.IfModifiedSince <- DateTime.Parse v)
  header "transfer-encoding" |> Choice.iter (fun v -> q.TransferEncoding <- v)
  header "user-agent"        |> Choice.iter (fun v -> q.UserAgent <- v)

  q.Headers.Add("X-Real-IP", ctx.clientIpTrustProxy.ToString())

  fun ctx -> socket {
    if p.``method`` = HttpMethod.POST || p.``method`` = HttpMethod.PUT then
      match p.headers %% "content-length" with
      | Choice1Of2 contentLength ->
        do! transferStreamBounded ctx.connection (q.GetRequestStream()) (int32 contentLength)
      | _ -> ()
    try
      let! data = SocketOp.ofAsync <| q.AsyncGetResponse()
      let! res = SocketOp.ofAsync <| sendWebResponse ((data : WebResponse) :?> HttpWebResponse) ctx
      match res with
      | Some newCtx ->
        do! response_f newCtx
        return Some newCtx
      | None -> return None
    with
    | :? WebException as ex when ex.Response <> null ->
      let! res = SocketOp.ofAsync <| sendWebResponse (ex.Response :?> HttpWebResponse) ctx
      match res with
      | Some newCtx ->
        do! response_f newCtx
        return Some newCtx
      | _ -> return None
    | :? WebException as ex when ex.Response = null ->
      let! res = SocketOp.ofAsync <|response HTTP_502 (UTF8.bytes "suave proxy: Could not connect to upstream") ctx
      match res with
      | Some newCtx ->
        do! response_f newCtx
        return Some newCtx
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
        let runtime = SuaveConfig.toRuntime config homeFolder compressionFolder false binding
        let reqLoop = ParsingAndControl.requestLoop runtime (SocketPart (proxy resolver))
        let tcpServer = config.tcpServerFactory.create(config.logger, config.maxOps, config.bufferSize, binding.socketBinding)
        let server = startTcpIpServerAsync reqLoop binding.socketBinding tcpServer
        yield server ]
      
  let listening = all |> List.map fst |> Async.Parallel |> Async.Ignore
  let server    = all |> List.map snd |> Async.Parallel |> Async.Ignore
  listening, server

let startReverseProxyServer config resolver =
  Async.RunSynchronously(createReverseProxyServerAsync config resolver |> snd,
    cancellationToken = config.cancellationToken)