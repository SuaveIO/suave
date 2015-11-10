/// A module for proxying requests to another computer/ip/port
module Suave.Proxy

open System
open System.Globalization
open System.IO
open System.Net
open System.Collections.Generic

open Suave.Utils
open Suave.Utils.Bytes

open Suave.Sockets
open Suave.Sockets.Control
open Suave.Types
open Suave.Web
open Suave.Tcp

open Suave.Http
open Suave.Http.Response
open Suave.Web.ParsingAndControl

let dateTimePatterns = [| "ddd, d MMM yyyy H:m:s 'GMT'";      // RFC 1123 (r, except it allows both 1 and 01 for date and time) 
                          "ddd, d MMM yyyy H:m:s";            // RFC 1123, no zone - assume GMT 
                          "d MMM yyyy H:m:s 'GMT'";           // RFC 1123, no day-of-week 
                          "d MMM yyyy H:m:s";                 // RFC 1123, no day-of-week, no zone 
                          "ddd, d MMM yy H:m:s 'GMT'";        // RFC 1123, short year 
                          "ddd, d MMM yy H:m:s";              // RFC 1123, short year, no zone 
                          "d MMM yy H:m:s 'GMT'";             // RFC 1123, no day-of-week, short year 
                          "d MMM yy H:m:s";                   // RFC 1123, no day-of-week, short year, no zone 
                          "dddd, d'-'MMM'-'yy H:m:s 'GMT'";   // RFC 850 
                          "dddd, d'-'MMM'-'yy H:m:s";         // RFC 850 no zone 
                          "ddd MMM d H:m:s yyyy";             // ANSI C's asctime() format 
                          "ddd, d MMM yyyy H:m:s zzz";        // RFC 5322 
                          "ddd, d MMM yyyy H:m:s";            // RFC 5322 no zone 
                          "d MMM yyyy H:m:s zzz";             // RFC 5322 no day-of-week 
                          "d MMM yyyy H:m:s"                  // RFC 5322 no day-of-week, no zone 
                       |]

/// See: https://msdn.microsoft.com/en-us/library/system.net.webheadercollection(v=vs.110).aspx
let restrictedHeaders = [| "Accept";
                           "Connection"; 
                           "Content-Length";  
                           "Content-Type"; 
                           "Date"; 
                           "Expect"; 
                           "Host"; 
                           "If-Modified-Since"; 
                           "Range"; 
                           "Referrer"; 
                           "Transfer-Encoding"; 
                           "User-Agent"; 
                           "Proxy-Connection" 
                        |] 

[<Literal>]
let headerIdsDate = "Date"
[<Literal>]
let headerIdsExpect = "Expect"
[<Literal>]
let headerIdsUserAgent = "User-Agent"

type private HttpWebRequest with
    member this.GetRequestStream () =
        let mutable value = null
        let iar = this.BeginGetRequestStream(new AsyncCallback(fun r -> let req = r.AsyncState :?> HttpWebRequest
                                                                        value <- req.EndGetRequestStream(r)
                                                                        ()), this)
        if not iar.IsCompleted then do 
            iar.AsyncWaitHandle.WaitOne()
            iar.AsyncWaitHandle.Dispose()
        value

    member this.Date 
        with get():DateTime = 
            let date = this.Headers.[headerIdsDate]
            let result, value = DateTime.TryParseExact(date, 
                                                       dateTimePatterns, DateTimeFormatInfo.InvariantInfo, 
                                                       DateTimeStyles.AllowWhiteSpaces ||| DateTimeStyles.AssumeUniversal) 
            if result then value else Unchecked.defaultof<DateTime>

        and  set (value:DateTime) =
            // Format according to RFC1123; 'r' uses invariant info (DateTimeFormatInfo.InvariantInfo) 
            let date = value.ToUniversalTime().ToString("r", CultureInfo.InvariantCulture) 
            this.Headers.[headerIdsDate] <- date

    member this.Expect 
        with get():string = this.Headers.[headerIdsExpect]

        and  set (value:string) =
            let expect = value
            if String.IsNullOrWhiteSpace(value) then
                this.Headers.Remove(headerIdsExpect)
            else
                this.Headers.[headerIdsExpect] <- expect

    member this.UserAgent 
      with get() =  this.Headers.[headerIdsUserAgent]
      and  set value =  this.Headers.[headerIdsUserAgent] <- value

type private System.Net.WebHeaderCollection with
    static member IsRestricted (key:string):bool =   
        match restrictedHeaders |> Array.tryFind(fun elem -> String.Compare(key, elem, StringComparison.OrdinalIgnoreCase) = 0) with
        | Some(_) -> true 
        | None -> false

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
  "-> readFully" |> Log.verbose ctx.runtime.logger "Suave.Proxy.sendWebResponse:GetResponseStream" ctx.request.trace
  let bytes = data.GetResponseStream() |> readFully
  "<- readFully" |> Log.verbose ctx.runtime.logger "Suave.Proxy.sendWebResponse:GetResponseStream" ctx.request.trace
  response (HttpCode.TryParse(int data.StatusCode) |> Option.get) bytes { ctx with response = { resp with headers = resp.headers @ headers } }
  
/// Forward the HttpRequest 'p' to the 'ip':'port'
let forward (ip : IPAddress) (port : uint16) (ctx : HttpContext) =
  let p = ctx.request
  let buildWebHeadersCollection (h : NameValueList) =
    let r = new WebHeaderCollection()
    for e in h do
      let key = fst e
      if not (WebHeaderCollection.IsRestricted key) then
        r.[key] <- (snd e)
    r
  let url = new UriBuilder("http", ip.ToString(), int port, p.url.AbsolutePath, p.rawQuery)
  let q = WebRequest.Create(url.Uri) :?> HttpWebRequest

#if !LIMITED_WEB_CLIENT
  q.AllowAutoRedirect         <- false
  q.AllowWriteStreamBuffering <- false
#endif
  q.AllowReadStreamBuffering  <- false
  q.Method                    <- string p.``method``
  q.Headers                   <- buildWebHeadersCollection p.headers
  q.Proxy                     <- null

  //copy restricted headers
  let header s = getFirst p.headers s
  header "accept"            |> Choice.iter (fun v -> q.Accept <- v)
  header "date"              |> Choice.iter (fun v -> q.Date <- DateTime.Parse v)
  header "expect"            |> Choice.iter (fun v -> q.Expect <- v)
#if !LIMITED_WEB_CLIENT
  header "host"              |> Choice.iter (fun v -> q.Host <- v)
  header "range"             |> Choice.iter (fun v -> q.AddRange(Int64.Parse v))
  header "referer"           |> Choice.iter (fun v -> q.Referer <- v)
#endif
  header "content-type"      |> Choice.iter (fun v -> q.ContentType <- v)
#if !LIMITED_WEB_CLIENT
  header "content-length"    |> Choice.iter (fun v -> q.ContentLength <- Int64.Parse(v))
  header "if-modified-since" |> Choice.iter (fun v -> q.IfModifiedSince <- DateTime.Parse v)
  header "transfer-encoding" |> Choice.iter (fun v -> q.TransferEncoding <- v)
#endif
  header "user-agent"        |> Choice.iter (fun v -> q.UserAgent <- v)

  q.Headers.["X-Real-IP"] <- (ctx.clientIpTrustProxy.ToString())

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
        let tcpServer = config.tcpServerFactory.create(config.logger, config.maxOps, config.bufferSize, binding)
        let server = startTcpIpServerAsync reqLoop binding.socketBinding tcpServer
        yield server ]
      
  let listening = all |> List.map fst |> Async.Parallel |> Async.Ignore
  let server    = all |> List.map snd |> Async.Parallel |> Async.Ignore
  listening, server

let startReverseProxyServer config resolver =
  Async.RunSynchronously(createReverseProxyServerAsync config resolver |> snd,
    cancellationToken = config.cancellationToken)