module Suave.Web

/// Parsing and control flow handling for web requests
module internal ParsingAndControl =
  open System
  open System.IO
  open System.Text
  open System.Diagnostics
  open System.Net
  open System.Net.Sockets
  open System.Threading
  open System.Threading.Tasks
  open System.Security.Permissions
  open System.Security.Principal
  open System.Collections.Generic

  open Suave.Http
  open Suave.Sockets
  open Suave.Types
  open Suave.Globals
  open Suave.Compression
  open Suave.Sockets.Connection
  open Suave.Tcp
  
  open Suave.Utils
  open Suave.Utils.Bytes
  open Suave.Utils.Parsing
  open Suave.Logging


  /// Free up a list of buffers
  let internal free context connection =
    List.iter (fun x -> connection.bufferManager.FreeBuffer (x.buffer, context)) connection.segments

  let skipBuffers (pairs : BufferSegment list) (number : int) :  BufferSegment list =
    let rec loop xxs acc = 
      match xxs with
      | [] -> []
      | x :: tail ->
        if x.length + acc >= number then 
          let segment = BufferSegment.mk x.buffer (x.offset  + (number - acc)) (x.length - number + acc)
          segment :: tail
        else loop tail (acc + x.length)
    loop pairs 0

  let split index connection select markerLength : Async<int * Connection> =
    let rec loop connection acc count =  async {
      match connection.segments with
      | [] -> return count, connection
      | pair :: tail ->
        if acc + pair.length < index then
          do! select (ArraySegment(pair.buffer.Array, pair.offset, pair.length)) pair.length
          connection.bufferManager.FreeBuffer( pair.buffer, "Suave.Web.split")
          return! loop {connection with segments = tail } (acc + pair.length) (count + acc + pair.length)
        elif acc + pair.length >= index then
          let bytesRead = index - acc
          do! select (ArraySegment(pair.buffer.Array, pair.offset, bytesRead)) bytesRead
          let remaining = pair.length - bytesRead
          if remaining = markerLength then
            connection.bufferManager.FreeBuffer( pair.buffer, "Suave.Web.split")
            return count + bytesRead, { connection with segments = tail }
          else
            if remaining - markerLength >= 0 then
              let segment = BufferSegment.mk pair.buffer
                                             (pair.offset  + bytesRead  + markerLength)
                                             (remaining - markerLength)
              return count + bytesRead, { connection with segments = segment :: tail}
            else
              let newTail = skipBuffers tail (markerLength - remaining)
              return count + bytesRead, { connection with segments = newTail }
        else return failwith "Suave.Web.split: invalid case"
      }
    loop connection 0 0

  type ScanResult = NeedMore | Found of int

  /// Iterates over a BufferSegment list looking for a marker, data before the marker 
  /// is sent to the function select and the corresponding buffers are released
  let scanMarker marker select connection = socket {

    match kmpZ marker connection.segments with
    | Some x -> 
      let! res, connection = liftAsync <| split x connection select marker.Length
      return Found res, connection
    | None   ->
      let rec loop (xs : BufferSegment list) (acc,n) =
        if n >= marker.Length then
          acc,xs
        else
          match xs with
          | x :: tail -> loop tail (acc @ [x],n + x.length)
          | []  -> acc,[]
      let rev = List.rev connection.segments
      let ret,free = loop rev ([],0)
      for b in free do
        assert (b.length >= 0)
        do! liftAsync <| select (ArraySegment(b.buffer.Array, b.offset, b.length)) b.length
        do connection.bufferManager.FreeBuffer( b.buffer,"Suave.Web.scanMarker" )
      return NeedMore,{ connection with segments = ret }
    }


  let readData (connection : Connection) buff = socket {
    let! b = receive connection buff 
    if b > 0 then
      return { buffer = buff; offset = buff.Offset; length = b }
    else
      return! abort (Error.SocketError SocketError.Shutdown)
    }

  let readMoreData connection = async {
    let buff = connection.bufferManager.PopBuffer("Suave.Web.read_till_pattern.loop")
    let! result = readData connection buff
    match result with
    | Choice1Of2 data ->
      return { connection with segments = connection.segments @ [data] } |> Choice1Of2
    | Choice2Of2 error ->
      for b in connection.segments do
        do connection.bufferManager.FreeBuffer( b.buffer, "Suave.Web.read_till_pattern.loop")
      do connection.bufferManager.FreeBuffer( buff, "Suave.Web.read_till_pattern.loop")
      return Choice2Of2 error
    }

  /// Read the passed stream into buff until the EOL (CRLF) has been reached
  /// and returns an array containing excess data read past the marker
  let readUntilPattern (connection : Connection) scanData =

    let rec loop (connection : Connection)  = socket {
      let! res, connection = scanData connection
      match res with
      | Found a ->
        return (a, connection)
      | NeedMore ->
        let! connection = readMoreData connection
        return! loop connection
    }
    loop connection

  let readUntilEOL (connection : Connection) select =
    readUntilPattern connection (scanMarker EOL select)

  /// Read the stream until the marker appears.
  let readUntil (marker : byte []) (select : ArraySegment<_> -> int -> Async<unit>) (connection : Connection) =
    readUntilPattern connection (scanMarker marker select)

  /// Read a line from the stream, calling ASCII.toString on the bytes before the EOL marker
  let readLine (connection : Connection) = socket {
    let offset = ref 0
    let buf = connection.lineBuffer
    let! count, connection = readUntilEOL connection (fun a count -> async { Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count; offset := !offset + count })
    let result = ASCII.toStringAtOffset buf.Array buf.Offset count
    return result, connection
  }

  /// Read all headers from the stream, returning a dictionary of the headers found
  let readHeaders connection =
    let rec loop (connection : Connection) headers = socket {
      let offset = ref 0
      let buf = connection.lineBuffer
      let! count, connection =
        readUntilEOL
          connection
          (fun a count -> async { Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count; offset := !offset + count })
      if count <> 0 then
        let line = ASCII.toStringAtOffset buf.Array buf.Offset count
        let indexOfColon = line.IndexOf(':')
        let header = (line.Substring(0, indexOfColon).ToLower(), line.Substring(indexOfColon+1).TrimStart())
        return! loop connection (header :: headers)
      else return (headers, connection )
    }
    loop connection []

  let inline arraySegmentFromBufferSegment b =
    ArraySegment(b.buffer.Array, b.offset, b.length)

  /// Read the post data from the stream, given the number of bytes that makes up the post data.
  let readPostData (connection : Connection) (bytes : int) select  : SocketOp<Connection> =

    let rec loop n (connection : Connection) : SocketOp<Connection> =
      socket {
        match connection.segments with
        | segment :: tail ->
          if segment.length > n then
            do! liftAsync <| select (arraySegmentFromBufferSegment { segment with offset = n }) n
            return { connection with segments = { buffer = segment.buffer; offset = segment.offset + n; length = segment.length - n } :: tail }
          else
            do! liftAsync <| select (arraySegmentFromBufferSegment segment) segment.length
            do connection.bufferManager.FreeBuffer(segment.buffer,"Suave.Web.readPostData:loop")
            return! loop (n - segment.length) { connection with segments = tail }
        | [] ->
          if n = 0 then
            return connection
          else
            let! connection = readMoreData connection
            return! loop n connection
      }
    loop bytes connection

  /// Parses multipart data from the stream, feeding it into the HttpRequest's property Files.
  let parseMultipart boundary (context : HttpContext) : SocketOp<HttpContext> =

    let rec loop boundary (ctx : HttpContext) = socket {

      let! firstLine, connection = readLine ctx.connection
      if not(firstLine.Equals("--")) then
        let! partHeaders, connection = readHeaders connection
        let contentDisposition = partHeaders %% "content-disposition"
        let fieldname = 
          (headerParams contentDisposition).TryLookup "name"
          |> Option.get
          |> (fun x -> x.Trim('"'))
        let contentType = partHeaders %% "content-type"
        match contentType with
        | Some(x) when x.StartsWith("multipart/mixed") ->
          let subboundary = "--" + x.Substring(x.IndexOf('=') + 1).TrimStart()
          return! loop subboundary { ctx with connection = connection }
        | Some(x) ->
          let tempFileName = Path.GetTempFileName()
          use tempFile = new FileStream(tempFileName, FileMode.Truncate)
          let! a, connection = readUntil (ASCII.bytes(eol + boundary)) (fun x y -> async { do! tempFile.AsyncWrite(x.Array, x.Offset, y) } ) connection
          let fileLength = tempFile.Length
          tempFile.Close()
          if fileLength > 0L then
            let filename =
              (headerParams contentDisposition).["filename"].Trim('"')
            let upload = 
                { fieldName     = fieldname
                  fileName      = filename
                  mimeType      = (contentType |> Option.get) 
                  tempFilePath = tempFileName }
            return! loop boundary { ctx with request = { ctx.request with files = upload :: ctx.request.files};  connection = connection }
          else
            File.Delete tempFileName
            return! loop boundary { ctx with connection = connection }
        | None ->
          use mem = new MemoryStream()
          let! a, connection = readUntil (ASCII.bytes(eol + boundary)) (fun x y -> async { do! mem.AsyncWrite(x.Array, x.Offset, y) } ) connection
          let byts = mem.ToArray()
          return! loop boundary { ctx with request = { ctx.request with multiPartFields = (fieldname,ASCII.toStringAtOffset byts 0 byts.Length)::(ctx.request.multiPartFields) }; connection = connection}
      else 
        return { ctx with connection = connection }
      }
    loop boundary context

  let parseTraceHeaders (headers : NameValueList) =
    let tryParseUint64 x = 
        match UInt64.TryParse x with 
        | true, value -> Some value
        | false, _    -> None
    let parent = "x-b3-spanid"  |> getFirst headers |> Option.bind tryParseUint64
    let trace  = "x-b3-traceid" |> getFirst headers |> Option.bind tryParseUint64
    TraceHeader.mk trace parent

  /// Reads raw POST data
  let getRawPostData connection contentLength =
    socket {
      let offset = ref 0
      let rawForm = Array.zeroCreate contentLength
      let! connection = readPostData connection contentLength (fun a count -> async { Array.blit a.Array a.Offset rawForm !offset count; offset := !offset + count })
      return rawForm , connection
    }

  let parsePostData (ctx : HttpContext) = socket{
    let request = ctx.request
    let contentEncoding = request.header "content-type"

    match request.header "content-length" with 
    | Some contentLengthString ->
      let contentLength = Convert.ToInt32(contentLengthString)

      match contentEncoding with
      | Some ce when ce.StartsWith("application/x-www-form-urlencoded") ->
        let! rawForm, connection = getRawPostData ctx.connection contentLength
        return Some { ctx with request = { request with rawForm = rawForm}; connection = connection }
      | Some ce when ce.StartsWith("multipart/form-data") ->
        let boundary = "--" + ce.Substring(ce.IndexOf('=')+1).TrimStart()
        let! ctx = parseMultipart boundary ctx
        return Some ctx
      | Some _ | None ->
        let! rawForm, connection = getRawPostData ctx.connection contentLength
        return Some { ctx with request = { request with rawForm = rawForm}; connection = connection }
    | None ->  return Some ctx
    }

  /// Process the request, reading as it goes from the incoming 'stream', yielding a HttpRequest
  /// when done
  let processRequest (ctx : HttpContext) = socket {

    let runtime = ctx.runtime
    let! firstLine, connection' = readLine ctx.connection

    let rawMethod, path, rawQuery, httpVersion = parseUrl firstLine
    let meth = HttpMethod.parse rawMethod

    let! headers, connection'' = readHeaders connection'

    // TODO: if client Host header not present, respond with 400 Bad Request as
    // per http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
    let host =
      headers %% "host" |> Option.get
      |> function
      | s when System.Text.RegularExpressions.Regex.IsMatch(s, ":\d+$") ->
        s.Substring(0, s.LastIndexOf(':'))
      | s -> s

    let request =
      { httpVersion      = httpVersion
        url              = runtime.matchedBinding.uri path rawQuery
        host             = ClientOnly host
        ``method``       = meth  
        headers          = headers
        rawForm          = [| |]
        rawQuery         = rawQuery
        files            = []
        multiPartFields  = []
        trace            = parseTraceHeaders headers
        isSecure         = runtime.matchedBinding.scheme.secure
        ipaddr           = connection''.ipaddr }

    if runtime.parsePostData then
      return! parsePostData { ctx with request = request; connection = connection'' }
    else
      return Some { ctx with request = request; connection = connection'' }
  }


  /// Load a readable plain-text stream, based on the protocol in use. If plain HTTP
  /// is being used, the stream is returned as it, otherwise a new SslStream is created
  /// to decipher the stream, without client certificates.
  let inline loadConnection (logger : Logger) proto (connection : Connection) = socket{
    match proto with
    | HTTP       ->
      return connection
    | HTTPS sslProvider -> 
      return! sslProvider.Wrap connection
    }

  open System.Net.Sockets

  let internal writeContentType connection (headers : (string*string) list) = socket {
    if not(List.exists(fun (x : string,_) -> x.ToLower().Equals("content-type")) headers )then
      do! asyncWriteLn connection "Content-Type: text/html"
  }

  let internal writeHeaders connection (headers : (string*string) seq) = socket {
    for x,y in headers do
      if not (List.exists (fun y -> x.ToLower().Equals(y)) ["server";"date";"content-length"]) then
        do! asyncWriteLn connection (String.Concat [| x; ": "; y |])
    }

  let writePreamble (context: HttpContext) = socket{
    let r = context.response
    let connection = context.connection

    do! asyncWriteLn connection (String.concat " " [ "HTTP/1.1"; r.status.code.ToString(); r.status.reason ])
    do! asyncWriteLn connection Internals.serverHeader
    do! asyncWriteLn connection (String.Concat( [| "Date: "; Globals.utcNow().ToString("R") |]))

    do! writeHeaders connection r.headers
    do! writeContentType connection r.headers
    }

  let writeContent context = function
    | Bytes b -> socket {
      let connection = context.connection
      let! (content : byte []) = Compression.transform b context connection
      // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
      do! asyncWriteLn connection (String.Concat [| "Content-Length: "; content.Length.ToString() |])
      do! asyncWriteLn connection ""
      if content.Length > 0 then
        do! send connection (new ArraySegment<_>(content, 0, content.Length))
      }
    | SocketTask f -> socket{ 
      return! f context.connection
      }
    | NullContent -> socket.Return()

  /// response_f writes the HTTP headers regardles of the setting of context.writePreamble
  /// it is currently only used in Proxy.fs
  let response_f (context: HttpContext) = socket {
    do! writePreamble context
    do! writeContent context context.response.content
    }

  let executeTask ctx r errorHandler = async {
    try
      let! q  = r
      return q
    with ex ->
      return! errorHandler ex "request failed" ctx
  }

  /// Check if the web part can perform its work on the current request. If it
  /// can't it will return None and the run method will return.
  let internal run ctx (webPart : WebPart) = 
    socket {
      let! result = liftAsync <| executeTask ctx (webPart ctx) ctx.runtime.errorHandler
      match result with 
      | Some newCtx ->
        if newCtx.response.writePreamble then
          do! writePreamble newCtx
        do! writeContent newCtx newCtx.response.content
        return Some newCtx
      | None -> return None
  }

  type HttpConsumer =
    | WebPart of WebPart
    | SocketPart of (HttpContext -> Async<(HttpContext -> SocketOp<HttpContext option>) option >)

  let operate consumer ctx = socket {
    match consumer with
    | WebPart webPart ->
      return! run ctx webPart
    | SocketPart writer ->
      let! intermediate = liftAsync <| writer ctx
      match intermediate with
      | Some task ->
        return! task ctx
      | None -> return Some ctx
    }

  let cleanResponse (ctx : HttpContext) =
    { ctx with response = HttpResult.empty }

  let httpLoop (ctxOuter:HttpContext) (consumer : HttpConsumer) =

    let runtime = ctxOuter.runtime
    let connection = ctxOuter.connection
    let rec loop (ctx : HttpContext) = socket {

      let verbose  = Log.verbose runtime.logger "Suave.Web.request_loop.loop" TraceHeader.empty
      let verbosef = Log.verbosef runtime.logger "Suave.Web.request_loop.loop" TraceHeader.empty

      verbose "-> processor"
      let! result = processRequest ctx
      verbose "<- processor"

      match result with
      | None -> verbose "'result = None', exiting"
      | Some ctx ->
        let! result = operate consumer ctx
        match result with
        | Some ctx ->
          match ctx.request.header "connection" with
          | Some (x : string) when x.ToLower().Equals("keep-alive") ->
            verbose "'Connection: keep-alive' recurse"
            return! loop (cleanResponse ctx)
          | Some _ ->
            free "Suave.Web.http_loop.loop (case Some _)" connection
            verbose "'Connection: close', exiting"
            return ()
          | None ->
            if ctx.request.httpVersion.Equals("HTTP/1.1") then
              verbose "'Connection: keep-alive' recurse (!)"
              return! loop (cleanResponse ctx)
            else
              free "Suave.Web.http_loop.loop (case None, else branch)" connection
              verbose "'Connection: close', exiting"
              return ()
        | None -> return ()
    }
    loop ctxOuter

  /// The request loop initialises a request with a processor to handle the
  /// incoming stream and possibly pass the request to the web parts, a protocol,
  /// a web part, an error handler and a Connection to use for read-write
  /// communication -- getting the initial request stream.
  let requestLoop
    (runtime    : HttpRuntime)
    (consumer   : HttpConsumer)
    (connection : Connection) =

    socket {
      let! connection = loadConnection runtime.logger runtime.matchedBinding.scheme connection
      do! httpLoop { HttpContext.empty with runtime = runtime; connection = connection } consumer
      return ()
    }


  /// Starts a new web worker, given the configuration and a web part to serve.
  let startWebWorkerAsync (bufferSize, maxOps) (webpart : WebPart) (runtime : HttpRuntime) =
    startTcpIpServerAsync
        (bufferSize, maxOps) 
        runtime.logger 
        (requestLoop runtime (WebPart webpart)) 
        runtime.matchedBinding.socketBinding

  let resolveDirectory homeDirectory =
    match homeDirectory with
    | None   -> Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
    | Some s -> s

////////////////////////////////////////////////////

open System
open System.IO
open System.Net
open Suave.Types
open Suave.Http
open Suave.Utils
open Suave.Logging

/// The default error handler returns a 500 Internal Error in response to
/// thrown exceptions.
let defaultErrorHandler (ex : Exception) msg (ctx : HttpContext) =
  let request = ctx.request
  msg |> Log.infoe ctx.runtime.logger "Suave.Web.default_error_handler" ctx.request.trace ex
  if IPAddress.IsLoopback ctx.request.ipaddr then
    Response.response HTTP_500 (UTF8.bytes (sprintf "<h1>%s</h1><br/>%A" ex.Message ex)) ctx
  else 
    Response.response HTTP_500 (UTF8.bytes HTTP_500.message) ctx

/// Starts the web server asynchronously.
///
/// Returns the webserver as a tuple of 1) an async computation the yields unit when
/// the web server is ready to serve quests, and 2) an async computation that yields
/// when the web server is being shut down and is being terminated. The async values
/// returned are not 'hot' in the sense that they have started running, so you must manually
/// start the 'server' (second item in tuple), as this starts the TcpListener.
/// Have a look at the example and the unit tests for more documentation.
/// In other words: don't block on 'listening' unless you have started the server.
/// The return value from 'listening' (first item in tuple) gives you some metrics on
/// how quickly suave started.
let startWebServerAsync (config : SuaveConfig) (webpart : WebPart) =
  let homeFolder, compressionFolder =
    ParsingAndControl.resolveDirectory config.homeFolder,
    Path.Combine(ParsingAndControl.resolveDirectory config.compressedFilesFolder, "_temporary_compressed_files")

  // spawn tcp listeners/web workers
  let servers = 
    config.bindings |> List.map (SuaveConfig.toRuntime config homeFolder compressionFolder true
              >> ParsingAndControl.startWebWorkerAsync (config.bufferSize, config.maxOps) webpart)
              
  let listening = servers |> Seq.map fst |> Async.Parallel
  let server    = servers |> Seq.map snd |> Async.Parallel |> Async.Ignore
  listening, server

/// Runs the web server and blocks waiting for the asynchronous workflow to be cancelled or
/// it returning itself.
let startWebServer (config : SuaveConfig) (webpart : WebPart) =
  Async.RunSynchronously(startWebServerAsync config webpart |> snd, cancellationToken = config.cancellationToken)

/// The default configuration binds on IPv4, 127.0.0.1:8083 with a regular 500 Internal Error handler,
/// with a timeout of one minute for computations to run. Waiting for 2 seconds for the socket bind
/// to succeed.
let defaultConfig = 
  { bindings              = [ HttpBinding.defaults ]
    serverKey             = Utils.Crypto.generateKey HttpRuntime.ServerKeyLength
    errorHandler          = defaultErrorHandler
    listenTimeout         = TimeSpan.FromSeconds(2.)
    cancellationToken     = Async.DefaultCancellationToken
    bufferSize            = 8192 // 8 KiB
    maxOps                = 100
    mimeTypesMap          = Http.Writers.defaultMimeTypesMap
    homeFolder            = None
    compressedFilesFolder = None
    logger                = Loggers.saneDefaultsFor LogLevel.Info }

/// Obsolete
[<Obsolete("Renamed to defaultErrorHandler")>]
let default_error_handler ex msg ctx = defaultErrorHandler ex msg ctx
/// Obsolete
[<Obsolete("Renamed to startWebServerAsync")>]
let web_server_async config webpart  = startWebServerAsync config webpart
/// Obsolete
[<Obsolete("Renamed to startWebServer")>]
let web_server config webpart = startWebServer config webpart
/// Obsolete
[<Obsolete("Renamed to defaultConfig")>]
let default_config = defaultConfig
