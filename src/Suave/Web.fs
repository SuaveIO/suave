module Suave.Web

/// Parsing and control flow handling for web requests
module internal ParsingAndControl =
  open System
  open System.IO
  open System.Text
  open System.Diagnostics
  open System.Net
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

  let skip_buffers (pairs : BufferSegment list) (number : int) :  BufferSegment list =
    let rec loop xxs acc = 
      match xxs with
      | [] -> []
      | x :: tail ->
        if x.length + acc >= number then 
          let segment = BufferSegment.mk x.buffer (x.offset  + (number - acc)) (x.length - number + acc)
          segment :: tail
        else loop tail (acc + x.length)
    loop pairs 0

  let split index connection select marker_length : Async<int * Connection> =
    let rec loop connection acc count =  async {
      match connection.segments with
      | [] -> return count, connection
      | pair :: tail ->
        if acc + pair.length < index then
          do! select (ArraySegment(pair.buffer.Array, pair.offset, pair.length)) pair.length
          connection.bufferManager.FreeBuffer( pair.buffer, "Suave.Web.split")
          return! loop {connection with segments = tail } (acc + pair.length) (count + acc + pair.length)
        elif acc + pair.length >= index then
          let bytes_read = index - acc
          do! select (ArraySegment(pair.buffer.Array, pair.offset, bytes_read)) bytes_read
          let remaining = pair.length - bytes_read
          if remaining = marker_length then
            connection.bufferManager.FreeBuffer( pair.buffer, "Suave.Web.split")
            return count + bytes_read, { connection with segments = tail }
          else
            if remaining - marker_length >= 0 then
              let segment = BufferSegment.mk pair.buffer
                                             (pair.offset  + bytes_read  + marker_length)
                                             (remaining - marker_length)
              return count + bytes_read, { connection with segments = segment :: tail}
            else
              let new_tail = skip_buffers tail (marker_length - remaining)
              return count + bytes_read, { connection with segments = new_tail }
        else return failwith "Suave.Web.split: invalid case"
      }
    loop connection 0 0

  type ScanResult = NeedMore | Found of int

  /// Iterates over a BufferSegment list looking for a marker, data before the marker 
  /// is sent to the function select and the corresponding buffers are released
  let scan_marker marker select connection = socket {

    match kmp_z marker connection.segments with
    | Some x -> 
      let! res, connection = lift_async <| split x connection select marker.Length
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
        do! lift_async <| select (ArraySegment(b.buffer.Array, b.offset, b.length)) b.length
        do connection.bufferManager.FreeBuffer( b.buffer,"Suave.Web.scan_marker" )
      return NeedMore,{ connection with segments = ret }
    }

  open System.Net.Sockets
  open Suave.Sockets.Connection

  let read_data (connection : Connection) buff = socket {
    let! b = receive connection buff 
    if b > 0 then
      return { buffer = buff; offset = buff.Offset; length = b }
    else
      return! abort (Error.SocketError SocketError.Shutdown)
    }

  let read_more_data connection = async {
    let buff = connection.bufferManager.PopBuffer("Suave.Web.read_till_pattern.loop")
    let! result = read_data connection buff
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
  let read_till_pattern (connection : Connection) scan_data =

    let rec loop (connection : Connection)  = socket {
      let! res, connection = scan_data connection
      match res with
      | Found a ->
        return (a, connection)
      | NeedMore ->
        let! connection = read_more_data connection
        return! loop connection
    }
    loop connection

  let read_till_EOL (connection : Connection) select =
    read_till_pattern connection (scan_marker EOL select)

  /// Read the stream until the marker appears.
  let read_until (marker : byte []) (select : ArraySegment<_> -> int -> Async<unit>) (connection : Connection) =
    read_till_pattern connection (scan_marker marker select)

  /// Read a line from the stream, calling to_string on the bytes before the EOL marker
  let read_line (connection : Connection) = socket {
    let offset = ref 0
    let buf = connection.lineBuffer
    let! count, connection = read_till_EOL connection (fun a count -> async { Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count; offset := !offset + count })
    let result = ASCII.to_string buf.Array buf.Offset count
    return result, connection
  }

  /// Read all headers from the stream, returning a dictionary of the headers found
  let read_headers connection =
    let rec loop (connection : Connection) headers = socket {
      let offset = ref 0
      let buf = connection.lineBuffer
      let! count, connection =
        read_till_EOL
          connection
          (fun a count -> async { Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count; offset := !offset + count })
      if count <> 0 then
        let line = ASCII.to_string buf.Array buf.Offset count
        let indexOfColon = line.IndexOf(':')
        let header = (line.Substring(0, indexOfColon).ToLower(), line.Substring(indexOfColon+1).TrimStart())
        return! loop connection (header :: headers)
      else return (headers, connection )
    }
    loop connection []

  open Suave.Types

  let inline array_segment_from_buffer_segment b =
    ArraySegment(b.buffer.Array, b.offset, b.length)

  /// Read the post data from the stream, given the number of bytes that makes up the post data.
  let read_post_data (connection : Connection) (bytes : int) select  : SocketOp<Connection> =

    let rec loop n (connection : Connection) : SocketOp<Connection> =
      socket {
        match connection.segments with
        | segment :: tail ->
          if segment.length > n then
            do! lift_async <| select (array_segment_from_buffer_segment { segment with offset = n }) n
            return { connection with segments = { buffer = segment.buffer; offset = segment.offset + n; length = segment.length - n } :: tail }
          else
            do! lift_async <| select (array_segment_from_buffer_segment segment) segment.length
            do connection.bufferManager.FreeBuffer(segment.buffer,"Suave.Web.read_post_data:loop")
            return! loop (n - segment.length) { connection with segments = tail }
        | [] ->
          if n = 0 then
            return connection
          else
            let! connection = read_more_data connection
            return! loop n connection
      }
    loop bytes connection

  /// Parses multipart data from the stream, feeding it into the HttpRequest's property Files.
  let parse_multipart boundary (context : HttpContext) : SocketOp<HttpContext> =

    let rec loop boundary (ctx : HttpContext) = socket {

      let! firstline, connection = read_line ctx.connection
      if not(firstline.Equals("--")) then
        let! part_headers, connection = read_headers connection
        let content_disposition =  part_headers %% "content-disposition"
        let fieldname = 
          (headerParams content_disposition) ? name 
          |> Option.get
          |> (fun x -> x.Trim('"'))
        let content_type = part_headers %% "content-type"
        match content_type with
        | Some(x) when x.StartsWith("multipart/mixed") ->
          let subboundary = "--" + x.Substring(x.IndexOf('=') + 1).TrimStart()
          return! loop subboundary { ctx with connection = connection }
        | Some(x) ->
          let tempFileName = Path.GetTempFileName()
          use tempFile = new FileStream(tempFileName, FileMode.Truncate)
          let! a, connection = read_until (ASCII.bytes(eol + boundary)) (fun x y -> async { do! tempFile.AsyncWrite(x.Array, x.Offset, y) } ) connection
          let fileLength = tempFile.Length
          tempFile.Close()
          if fileLength > 0L then
            let filename =
              (headerParams content_disposition).["filename"].Trim('"')
            let upload = 
                { fieldName     = fieldname
                  fileName      = filename
                  mimeType      = (content_type |> Option.get) 
                  tempFilePath = tempFileName }
            return! loop boundary { ctx with request = { ctx.request with files = upload :: ctx.request.files};  connection = connection }
          else
            File.Delete tempFileName
            return! loop boundary { ctx with connection = connection }
        | None ->
          use mem = new MemoryStream()
          let! a, connection = read_until (ASCII.bytes(eol + boundary)) (fun x y -> async { do! mem.AsyncWrite(x.Array, x.Offset, y) } ) connection
          let byts = mem.ToArray()
          return! loop boundary { ctx with request = { ctx.request with multiPartFields = (fieldname,ASCII.to_string byts 0 byts.Length)::(ctx.request.multiPartFields) }; connection = connection}
      else 
        return { ctx with connection = connection }
      }
    loop boundary context

  let parse_trace_headers (headers : NameValueList) =
    let parse_uint64 = (function | true, value -> Some value
                                 | false, _    -> None
                       ) << UInt64.TryParse
    let parent = "x-b3-spanid"  |> get_first headers |> Option.bind parse_uint64
    let trace  = "x-b3-traceid" |> get_first headers |> Option.bind parse_uint64
    TraceHeader.mk trace parent

  /// Reads raw POST data
  let get_raw_post_data connection content_length =
    socket {
      let offset = ref 0
      let raw_form = Array.zeroCreate content_length
      let! connection = read_post_data connection content_length (fun a count -> async { Array.blit a.Array a.Offset raw_form !offset count; offset := !offset + count })
      return raw_form , connection
    }

  let parse_post_data' (ctx : HttpContext) = socket{
    let request = ctx.request
    let content_encoding = request.headers %% "content-type"

    match request.headers %% "content-length" with 
    | Some content_length_string ->
      let content_length = Convert.ToInt32(content_length_string)

      match content_encoding with
      | Some ce when ce.StartsWith("application/x-www-form-urlencoded") ->
        let! raw_form, connection = get_raw_post_data ctx.connection content_length
        return Some { ctx with request = { request with rawForm = raw_form}; connection = connection }
      | Some ce when ce.StartsWith("multipart/form-data") ->
        let boundary = "--" + ce.Substring(ce.IndexOf('=')+1).TrimStart()
        let! ctx = parse_multipart boundary ctx
        return Some ctx
      | Some _ | None ->
        let! raw_form, connection = get_raw_post_data ctx.connection content_length
        return Some { ctx with request = { request with rawForm = raw_form}; connection = connection }
    | None ->  return Some ctx
    }

  let private parse_post_data = to_async <| parse_post_data'

  /// Process the request, reading as it goes from the incoming 'stream', yielding a HttpRequest
  /// when done
  let process_request ({ connection = connection
                         runtime    = runtime } as ctx)
                      : SocketOp<HttpContext option> = socket {

    let! (first_line : string), connection' = read_line connection

    let raw_method, path, raw_query, http_version = parse_url first_line
    let meth = HttpMethod.parse raw_method

    let! headers, connection'' = read_headers connection'

    // TODO: if client Host header not present, respond with 400 Bad Request as
    // per http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
    let host =
      headers %% "host" |> Option.get
      |> function
      | s when System.Text.RegularExpressions.Regex.IsMatch(s, ":\d+$") ->
        s.Substring(0, s.LastIndexOf(':'))
      | s -> s

    let request =
      HttpRequest.mk http_version
                     (runtime.matchedBinding.Uri path raw_query)
                     (ClientOnly host)
                     meth headers raw_query
                     (parse_trace_headers headers)
                     runtime.matchedBinding.scheme.IsSecure
                     connection''.ipaddr

    if runtime.parsePostData then
      return! parse_post_data' { ctx with request = request; connection = connection'' }
    else
      return Some { ctx with request = request; connection = connection'' }
  }


  /// Load a readable plain-text stream, based on the protocol in use. If plain HTTP
  /// is being used, the stream is returned as it, otherwise a new SslStream is created
  /// to decipher the stream, without client certificates.
  let inline load_connection (logger : Logger) proto (connection : Connection) = socket{
    match proto with
    | HTTP       ->
      return connection
    | HTTPS ssl_provider -> 
      return! ssl_provider.Wrap connection
    }

  open System.Net.Sockets

  let internal write_content_type connection (headers : (string*string) list) = socket {
    if not(List.exists(fun (x : string,_) -> x.ToLower().Equals("content-type")) headers )then
      do! async_writeln connection "Content-Type: text/html"
  }

  let internal write_headers connection (headers : (string*string) seq) = socket {
    for x,y in headers do
      if not (List.exists (fun y -> x.ToLower().Equals(y)) ["server";"date";"content-length"]) then
        do! async_writeln connection (String.Concat [| x; ": "; y |])
    }


  let write_content context = function
    | Bytes b -> socket {
      let connection = context.connection
      let! (content : byte []) = Compression.transform b context connection
      // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
      do! async_writeln connection (String.Concat [| "Content-Length: "; content.Length.ToString() |])
      do! async_writeln connection ""
      if content.Length > 0 then
        do! send connection (new ArraySegment<_>(content, 0, content.Length))
      }
    | SocketTask f -> f context.connection
    | NullContent -> failwith "TODO: unexpected NullContent value for 'write_content'"

  let response_f (context: HttpContext) = socket {
    
    let r = context.response
    let connection = context.connection
    do! async_writeln connection (String.concat " " [ "HTTP/1.1"
                                                      r.status.Code.ToString()
                                                      r.status.Reason ])
    do! async_writeln connection Internals.server_header
    do! async_writeln connection (String.Concat( [| "Date: "; Globals.utc_now().ToString("R") |]))

    do! write_headers connection r.headers
    do! write_content_type connection r.headers

    return! write_content context r.content
  }

  /// Check if the web part can perform its work on the current request. If it
  /// can't it will return None and the run method will return.
  let internal run ctx (web_part : HttpPart) = 
    let execute _ = async {
      try  
          let! q  = web_part ctx
          return q
        with ex ->
          return! ctx.runtime.errorHandler ex "request failed" ctx
    }
    socket {
      let! result = lift_async <| execute ()
      match result with 
      | Some executed_part ->
        do! response_f executed_part
        return Some <| executed_part
      | None -> return None
  }

  type HttpConsumer =
    | HttpPart of HttpPart
    | SocketPart of (HttpContext -> Async<(HttpContext -> SocketOp<HttpContext option>) option >)

  let operate consumer ctx = socket {
    match consumer with
    | HttpPart web_part ->
      return! run ctx web_part
    | SocketPart writer ->
      let! intermediate = lift_async <| writer ctx
      match intermediate with
      | Some task ->
        return! task ctx
      | None -> return Some ctx
    }

  let cleanResponse (ctx : HttpContext) =
    { ctx with response = HttpResult.empty }

  let httpLoop (ctx_outer:HttpContext) (consumer : HttpConsumer) =

    let runtime = ctx_outer.runtime
    let connection = ctx_outer.connection
    let rec loop (ctx : HttpContext) = socket {

      let verbose  = Log.verbose runtime.logger "Suave.Web.request_loop.loop" TraceHeader.empty
      let verbosef = Log.verbosef runtime.logger "Suave.Web.request_loop.loop" TraceHeader.empty

      verbose "-> processor"
      let! result = process_request ctx
      verbose "<- processor"

      match result with
      | None -> verbose "'result = None', exiting"
      | Some ctx ->
        let! result = operate consumer ctx
        match result with
        | Some ctx ->
          match ctx.request.headers %% "connection" with
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
    loop ctx_outer

  /// The request loop initialises a request with a processor to handle the
  /// incoming stream and possibly pass the request to the web parts, a protocol,
  /// a web part, an error handler and a Connection to use for read-write
  /// communication -- getting the initial request stream.
  let requestLoop
    (runtime    : HttpRuntime)
    (consumer   : HttpConsumer)
    (connection : Connection) =

    socket {
      let! connection = load_connection runtime.logger runtime.matchedBinding.scheme connection
      do! httpLoop { HttpContext.empty with runtime = runtime; connection = connection } consumer
      return ()
    }


  /// Starts a new web worker, given the configuration and a web part to serve.
  let createHttpServer (bufferSize, maxOps) (webpart : HttpPart) (runtime : HttpRuntime) =
    createTcpIpServer 
        (bufferSize, maxOps, runtime.logger, 
         requestLoop runtime (HttpPart webpart), runtime.matchedBinding.socketBinding)

  let resolveDirectory homeDirectory =
    match homeDirectory with
    | None   -> System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
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
    Response.response HTTP_500 (UTF8.bytes HTTP_500.Message) ctx

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
let createWebServerAsync (config : SuaveConfig) (webpart : HttpPart) =
  let homeFolder, compressionFolder =
    ParsingAndControl.resolveDirectory config.homeFolder,
    Path.Combine(ParsingAndControl.resolveDirectory config.compressedFilesFolder, "_temporary_compressed_files")

  // spawn tcp listeners/web workers
  let servers = 
    config.bindings |> List.map (config.ToRuntime homeFolder compressionFolder true
              >> ParsingAndControl.createHttpServer (config.bufferSize, config.maxOps) webpart)
              
  let listening = servers |> Seq.map fst |> Async.Parallel
  let server    = servers |> Seq.map snd |> Async.Parallel |> Async.Ignore
  listening, server

/// Runs the web server and blocks waiting for the asynchronous workflow to be cancelled or
/// it returning itself.
let startWebServer (config : SuaveConfig) (webpart : HttpPart) =
  Async.RunSynchronously(createWebServerAsync config webpart |> snd, cancellationToken = config.cancellationToken)

/// The default configuration binds on IPv4, 127.0.0.1:8083 with a regular 500 Internal Error handler,
/// with a timeout of one minute for computations to run. Waiting for 2 seconds for the socket bind
/// to succeed.
type SuaveConfig with 
  static member defaults = 
      { bindings         = [ HttpBinding.Defaults ]
        serverKey       = Utils.Crypto.generateKey HttpRuntime.ServerKeyLength
        errorHandler    = defaultErrorHandler
        listenTimeout   = TimeSpan.FromSeconds(2.)
        cancellationToken               = Async.DefaultCancellationToken
        bufferSize      = 8192 // 8 KiB
        maxOps          = 100
        mimeTypesMap   = Http.Writers.defaultMimeTypesMap
        homeFolder      = None
        compressedFilesFolder = None
        logger           = Loggers.sane_defaults_for LogLevel.Info }
