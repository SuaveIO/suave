module Suave.Web

/// Parsing and control flow handling for web requests
module ParsingAndControl =
  open Utils

  open System
  open System.IO

  open System.Text
  open System.Diagnostics
  open System.Threading
  open System.Threading.Tasks
  open System.Security.Permissions
  open System.Security.Principal
  open System.Collections.Generic

  open Http
  open Socket

  open Suave.Utils.Bytes
  open Suave.Utils.Parsing

  let internal free context connection (s : BufferSegment option) =
    match s with
    | None -> ()
    | Some x -> connection.free_buffer context x.buffer

  let split x (c,pairs : BufferSegment list) connection select marker_lenght =
    let rec loop i acc count =  async {
      let pair = pairs.[i]
      if acc + pair.length < x then
        do! select (ArraySegment(pair.buffer.Array, pair.offset, pair.length)) pair.length
        connection.free_buffer "Web.split" pair.buffer
        return! loop (i+1) (acc + pair.length) (count + acc)
      elif acc + pair.length >= x then
        let bytes_read = x - acc
        do! select (ArraySegment(pair.buffer.Array, pair.offset, bytes_read)) bytes_read
        let remaining = pair.length - bytes_read
        if remaining = marker_lenght then
          connection.free_buffer "Web.split" pair.buffer
          return count + bytes_read, None
        else
          return count + bytes_read, mk_buffer_segment pair.buffer (pair.offset  + bytes_read  + marker_lenght) (remaining - marker_lenght)
      else return failwith "Web.split: invalid case"
      }
    loop 0 0 c

  type ScanResult = NeedMore of (BufferSegment list) | Found of (int * BufferSegment option)

  /// Iterates over a BufferSegment list looking for a marker, data before the marker 
  /// is sent to the function select and the corresponding buffers are released
  let scan_marker marker count (pairs : BufferSegment list) connection select = async {
    match kmp_y marker (pairs |> List.map ( fun x -> ArraySegment(x.buffer.Array, x.offset, x.length))) with
    | Some x -> 
      let! res = split x (count,pairs) connection select  marker.Length
      return res |> Found
    | None   ->
      let rec loop (xs : BufferSegment list) (acc,n) =
        if n >= marker.Length then
          acc,xs
        else
          match xs with
          | x :: tail -> loop tail (acc @ [x],n + x.length)
          | []  -> acc,[]
      let rev = List.rev pairs
      let ret,free = loop rev ([],0)
      for b in free do
        do! select (ArraySegment(b.buffer.Array, b.offset, b.length)) b.length
        do connection.free_buffer "Web.scan_marker" b.buffer
      return NeedMore ret
    }

  open System.Net.Sockets

  let read_data (connection : Connection) buff = socket {
      let! b = connection.read buff
      if b > 0 then
        return { buffer = buff; offset = buff.Offset; length = b }
      else
        return! abort (Error.SocketError SocketError.Shutdown)
      }

  /// Read the passed stream into buff until the EOL (CRLF) has been reached
  /// and returns an array containing excess data read past the marker
  let read_till_pattern (connection : Connection) select (preread : BufferSegment option) scan_data =

    let rec loop state = async {
      let! res = scan_data 0 state connection select
      match res with
      | Found a ->
        return Choice1Of2 a
      | NeedMore buffer_segment_list ->
          let buff = connection.get_buffer "read_till_pattern.loop"
          let! result = read_data connection buff
          match result with
          | Choice1Of2 data ->
            return! loop (buffer_segment_list @ [data])
          | Choice2Of2 error ->
            for b in buffer_segment_list do
              do connection.free_buffer "read_till_pattern.loop" b.buffer
            connection.free_buffer "read_till_pattern.loop" buff
            return Choice2Of2 error
    }

    match preread with
    | Some data -> loop [ data ]
    | None      -> loop []

  let read_till_EOL (connection : Connection) select (preread : BufferSegment option) =
    read_till_pattern connection select preread (scan_marker EOL)

  /// Read the stream until the marker appears.
  let read_until (marker : byte []) (select : ArraySegment<_> -> int -> Async<unit>) (connection : Connection) (preread : BufferSegment option) =
    read_till_pattern connection select preread (scan_marker marker)

  /// Read a line from the stream, calling to_string on the bytes before the EOL marker
  let read_line (connection : Connection) ahead (buf : ArraySegment<byte>) = socket {
    let offset = ref 0
    let! count, rem = read_till_EOL connection (fun a count -> async { Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count; offset := !offset + count }) ahead
    let result = ASCII.to_string buf.Array buf.Offset count
    return result, rem
  }

  /// Read all headers from the stream, returning a dictionary of the headers found
  let read_headers connection read (headers : Dictionary<string,string>) (buf : ArraySegment<byte>) =
    let rec loop (rem : BufferSegment option) = socket {
      let offset = ref 0
      let! count, new_rem =
        read_till_EOL
          connection
          (fun a count -> async { Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count; offset := !offset + count })
          rem
      if count <> 0 then
        let line = ASCII.to_string buf.Array buf.Offset count
        let indexOfColon = line.IndexOf(':')
        headers.Add (line.Substring(0, indexOfColon).ToLower(), line.Substring(indexOfColon+1).TrimStart())
        return! loop new_rem
      else return new_rem
    }
    loop read

  open Suave.Types

  /// Gets the empty query string dictionary
  let empty_query_string () = new Dictionary<string,string>()

  /// Read the post data from the stream, given the number of bytes that makes up the post data.
  let read_post_data (connection : Connection) (bytes : int) (read : BufferSegment option) =

    let free_buffer buff a =
      match buff with
      | None -> connection.free_buffer "read_post_data" a 
      | _ -> ()
    
    let read_bytes bytes_needed (missing : byte[]) read_offset = socket {
      let counter = ref 0
      let rem = ref None
      let a = connection.get_buffer "read_post_data"
      while !counter < bytes_needed do
        let! bytes_transmited = connection.read a
        let need_to_read = bytes_needed - !counter
        if bytes_transmited > need_to_read
        then
          Array.blit a.Array a.Offset missing (read_offset + !counter) need_to_read
          rem := Some { buffer = a; offset =  a.Offset + need_to_read; length = bytes_transmited - need_to_read }
        else
          Array.blit a.Array a.Offset missing (read_offset + !counter) bytes_transmited
        counter := !counter + bytes_transmited
      do free_buffer !rem a
      return (ArraySegment missing, !rem)
    }

    socket {
      match read with
      | Some segment ->
        if segment.length >= bytes then
          return (ArraySegment(segment.buffer.Array, segment.offset, bytes), Some { buffer = segment.buffer; offset = segment.offset + bytes; length = segment.length - bytes })
        else 
          let missing = Array.zeroCreate bytes
          Array.blit segment.buffer.Array segment.offset missing 0 segment.length
          connection.free_buffer "read_post_data" segment.buffer
          return! read_bytes (bytes - segment.length) missing segment.length
      | None ->
        let missing = Array.zeroCreate bytes
        return! read_bytes bytes missing 0
    }

  /// Parses multipart data from the stream, feeding it into the HttpRequest's property Files.
  let parse_multipart (connection : Connection)
                      boundary
                      (request : HttpRequest)
                      (ahead : BufferSegment option)
                      line_buffer : SocketOp<BufferSegment option> =
    let rec loop boundary read = socket {

      let! firstline, read = read_line connection read line_buffer

      if not(firstline.Equals("--")) then

        let part_headers = new Dictionary<string,string>()
        let! rem = read_headers connection read part_headers line_buffer

        let content_disposition = look_up part_headers "content-disposition"

        let fieldname = (header_params content_disposition) ? name |> Option.get

        let content_type = look_up part_headers "content-type"

        return! parse_content content_type content_disposition fieldname rem
      else return None
      }
    and parse_content content_type content_disposition fieldname rem = socket {
      match content_type with
      | Some(x) when x.StartsWith("multipart/mixed") ->
        let subboundary = "--" + x.Substring(x.IndexOf('=') + 1).TrimStart()
        return! loop subboundary rem
      | Some(x) ->
        let temp_file_name = Path.GetTempFileName()
        use temp_file = new FileStream(temp_file_name, FileMode.Truncate)
        let! a, b = read_until (ASCII.bytes(eol + boundary)) (fun x y -> async { do! temp_file.AsyncWrite(x.Array, x.Offset, y) } ) connection rem
        let file_length = temp_file.Length
        temp_file.Close()
        if file_length > int64(0) then
          let filename =
            (header_params content_disposition) ? filename |> Option.get
          request.files.Add(new HttpUpload(fieldname, filename, content_type |> Option.get, temp_file_name))
        else
          File.Delete temp_file_name
        return! loop boundary b
      | None ->
        use mem = new MemoryStream()
        let! a, b = read_until (ASCII.bytes(eol + boundary)) (fun x y -> async { do! mem.AsyncWrite(x.Array, x.Offset, y) } ) connection rem
        let byts = mem.ToArray()
        request.form.Add(fieldname, (ASCII.to_string byts 0 byts.Length))

        return! loop boundary b
      }
    loop boundary ahead

  let parse_trace_headers (headers : IDictionary<string, string>) =
    let parse_uint64 = (function | true, value -> Some value
                                 | false, _    -> None
                       ) << UInt64.TryParse
    let parent = "x-b3-spanid"  |> look_up headers |> Option.bind parse_uint64
    let trace  = "x-b3-traceid" |> look_up headers |> Option.bind parse_uint64
    Log.TraceHeader.Create(trace, parent)

  /// Process the request, reading as it goes from the incoming 'stream', yielding a HttpRequest
  /// when done
  let process_request proxy_mode (ctx : HttpContext)(bytes : BufferSegment option) connection : SocketOp<(HttpRequest * BufferSegment option) option> = socket {

    let request : HttpRequest = ctx.request

    let verbose = Log.verbose ctx.runtime.logger "Web.process_request" request.trace
    let line_buffer = connection.line_buffer

    let! (first_line : string), rem = read_line connection bytes line_buffer

    let meth, url, raw_query, http_version = parse_url first_line request.query

    request.url          <- url
    request.``method``   <- meth
    request.raw_query    <- raw_query
    request.http_version <- http_version

    let! rem = read_headers connection rem request.headers line_buffer
    request.trace <- parse_trace_headers request.headers

    // won't continue parsing if on proxyMode with the intention of forwarding the stream as it is
    // TODO: proxy mode might need headers and contents of request, but won't get it through this impl
    if proxy_mode then return Some (request, rem)
    else
      request.headers
      |> Seq.filter (fun x -> x.Key.Equals("cookie"))
      |> Seq.iter (fun x ->
                    parse_cookie x.Value
                    |> Array.iter (fun y -> request.cookies.Add (fst y,snd y)))

      if meth.Equals("POST") || meth.Equals("PUT") then

        let content_encoding =
          match request.headers.TryGetValue("content-type") with
          | true, encoding -> Some encoding
          | false, _ -> None

        let content_length = Convert.ToInt32(request.headers.["content-length"])

        match content_encoding with
        | Some ce when ce.StartsWith("application/x-www-form-urlencoded") ->
          let! (rawdata : ArraySegment<_>), rem = read_post_data connection content_length rem
          let str = ASCII.to_string rawdata.Array rawdata.Offset rawdata.Count
          let _  = parse_data str request.form
          // TODO: we can defer reading of body until we need it
          let raw_form = Array.zeroCreate rawdata.Count
          Array.blit rawdata.Array rawdata.Offset raw_form 0 rawdata.Count
          request.raw_form <- raw_form
          return Some (request, rem)
        | Some ce when ce.StartsWith("multipart/form-data") ->
          let boundary = "--" + ce.Substring(ce.IndexOf('=')+1).TrimStart()
          // TODO: we can defer reading of body until we need it
          let! rem = parse_multipart connection boundary request rem line_buffer
          return Some (request, rem)
        | Some _ | None ->
          // TODO: we can defer reading of body until we need it
          let! (rawdata : ArraySegment<_>), rem = read_post_data connection content_length rem
          let raw_form = Array.zeroCreate rawdata.Count
          Array.blit rawdata.Array rawdata.Offset raw_form 0 rawdata.Count
          request.raw_form <- raw_form
          return Some (request, rem)
      else return Some (request, rem)
  }

  open System.Net
  open Types

  /// Load a readable plain-text stream, based on the protocol in use. If plain HTTP
  /// is being used, the stream is returned as it, otherwise a new SslStream is created
  /// to decipher the stream, without client certificates.
  let inline load_connection (logger : Log.Logger) proto (connection : Connection) = socket{
    match proto with
    | HTTP       ->
      return connection
    | HTTPS ssl_provider -> 
      return! ssl_provider.Wrap connection
    }

  open System.Net.Sockets

  /// A HttpProcessor takes a HttpRequest instance, returning asynchronously a HttpRequest that has been parsed
  type HttpProcessor = HttpRequest -> Connection -> BufferSegment option -> Async<(HttpRequest * (BufferSegment option)) option>
  type RequestResult = Done

  let internal mk_request connection proto ipaddr : HttpRequest =
    { http_version   = null
    ; url            = null
    ; ``method``     = null
    ; query          = new Dictionary<string,string>()
    ; headers        = new Dictionary<string,string>()
    ; form           = new Dictionary<string,string>()
    ; raw_form       = null
    ; raw_query      = null
    ; cookies        = new Dictionary<string,string>()
    ; user_name      = null
    ; password       = null
    ; session_id     = null
    ; files          = new List<HttpUpload>()
    ; is_secure      = match proto with HTTP -> false | HTTPS _ -> true
    ; trace          = Log.TraceHeader.Empty
    ; ipaddr = ipaddr }
  
  let internal write_content_type connection (headers : (string*string) list) = socket {
    if not(List.exists(fun (x : string,_) -> x.ToLower().Equals("content-type")) headers )then
      do! async_writeln connection "Content-Type: text/html"
  }

  let internal write_headers connection (headers : (string*string) seq) = socket {
    for (x,y) in headers do
      if not (List.exists (fun y -> x.ToLower().Equals(y)) ["server";"date";"content-length"]) then
        do! async_writeln connection (String.Concat [| x; ": "; y |])
    }

  open Types.Codes
  open Globals
  open Suave.Compression

  let rec write_content context connection = function
    | Bytes b -> socket {
      let! (content : byte []) = Compression.transform b context connection
      // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
      do! async_writeln connection (String.Concat [| "Content-Length: "; content.Length.ToString() |])
      do! async_writeln connection ""
      if content.Length > 0 then
        do! connection.write (new ArraySegment<_>(content, 0, content.Length))
      }
    | SocketTask f -> f connection
    | NullContent -> write_content context connection (Bytes [||])

  let response_f ({ response = r } as context : HttpContext) connection = socket {
    do! async_writeln connection (String.concat " " [ "HTTP/1.1"
                                                    ; (http_code r.status).ToString()
                                                    ; http_reason r.status ])
    do! async_writeln connection Internals.server_header
    do! async_writeln connection (String.Concat( [| "Date: "; Globals.utc_now().ToString("R") |]))

    do! write_headers connection r.headers
    do! write_content_type connection r.headers

    return! write_content context connection r.content
  }

  /// Check if the web part can perform its work on the current request. If it
  /// can't it will return None and the run method will return.
  let internal run ctx (web_part : WebPart) connection = socket {
    match web_part ctx with
    | Some(Choice1Of2 executed_part) ->
      return! response_f executed_part connection
    | Some(Choice2Of2(async_part)) ->
      let! res = async { let! res = async_part
                         return Choice1Of2 res }
      return! response_f res connection
    | None -> return ()
  }

  type HttpConsumer =
    | WebPart of WebPart
    | SocketPart of (HttpContext -> Connection -> SocketOp<unit>)

  let http_loop (proxy_mode : bool) (ctx : HttpContext) (consumer : HttpConsumer) (connection : Connection) =

    let rec loop (bytes : BufferSegment option) request = socket {

      let verbose  = Log.verbose ctx.runtime.logger "Web.request_loop.loop" request.trace
      let verbosef = Log.verbosef ctx.runtime.logger "Web.request_loop.loop" request.trace

      verbose "-> processor"
      let! result = process_request proxy_mode ctx bytes connection
      verbose "<- processor"

      match result with
      | None -> verbose "'result = None', exiting"
      | Some (request : HttpRequest, rem) ->
        let ctx = { ctx with request = request }
        match consumer with
        | WebPart web_part ->
          do! run ctx web_part connection
        | SocketPart writer ->
          do! writer ctx connection
        if connection.is_connected () then
          match request.headers?connection with
          | Some (x : string) when x.ToLower().Equals("keep-alive") ->
            clear request
            verbosef (fun fmt -> fmt "'Connection: keep-alive' recurse, rem: %A" rem)
            return! loop rem request
          | Some _ ->
            free "http_loop.loop (case Some _)" connection rem;
            verbose "'Connection: close', exiting"
            return ()
          | None ->
            if request.http_version.Equals("HTTP/1.1") then
              clear request
              verbose "'Connection: keep-alive' recurse (!)"
              return! loop rem request
            else
              free "http_loop.loop (case None, else branch)" connection rem;
              verbose "'Connection: close', exiting"
              return ()
        else
          free "http_loop.loop (not connected)" connection rem;
          verbose "'is_connected = false', exiting"
          return ()
    }
    loop None ctx.request

  /// The request loop initialises a request with a processor to handle the
  /// incoming stream and possibly pass the request to the web parts, a protocol,
  /// a web part, an error handler and a Connection to use for read-write
  /// communication -- getting the initial request stream.
  let request_loop
    (proxy_mode : bool)
    (runtime    : HttpRuntime)
    (consumer   : HttpConsumer)
    (connection : Connection) =

    socket {
      let! connection = load_connection runtime.logger runtime.protocol connection
      let request     = mk_request connection runtime.protocol connection.ipaddr
      do! http_loop proxy_mode { request = request; user_state = Map.empty; runtime = runtime; response = { status = HTTP_404; headers = []; content = NullContent }} consumer connection
      return ()
    }

  open Suave.Tcp

  let mk_http_runtime proto error_handler mime_types home_directory compression_folder logger session_provider =
    { protocol           = proto
    ; error_handler      = error_handler
    ; mime_types_map     = mime_types
    ; home_directory     = home_directory
    ; compression_folder = compression_folder
    ; logger             = logger
    ; session_provider   = session_provider }

  /// Starts a new web worker, given the configuration and a web part to serve.
  let web_worker (ip, port, buffer_size, max_ops, runtime : HttpRuntime) (webpart : WebPart) =
    tcp_ip_server (ip, port, buffer_size, max_ops) runtime.logger (request_loop false runtime (WebPart webpart))

  let resolve_directory home_directory =
    match home_directory with
    | None   -> System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
    | Some s -> s

////////////////////////////////////////////////////

open System
open System.Net
open Suave.Types
open Suave.Http
open Suave.Socket
open Suave.Session

/// The default error handler returns a 500 Internal Error in response to
/// thrown exceptions.
let default_error_handler (ex : Exception) msg (ctx : HttpContext) = 
  let request = ctx.request
  msg |> Log.verbosee ctx.runtime.logger "Web.default_error_handler" ctx.request.trace ex
  if IPAddress.IsLoopback ctx.request.ipaddr then
    Response.response Codes.HTTP_500 (UTF8.bytes (sprintf "<h1>%s</h1><br/>%A" ex.Message ex)) ctx
  else 
    Response.response Codes.HTTP_500 (UTF8.bytes (Codes.http_message Codes.HTTP_500)) ctx

/// Returns the webserver as a tuple of 1) an async computation the yields unit when
/// the web server is ready to serve quests, and 2) an async computation that yields
/// when the web server is being shut down and is being terminated. The async values
/// returned are not 'hot' in the sense that they have started running, so you must manually
/// start the 'server' (second item in tuple), as this starts the TcpListener.
/// Have a look at the example and the unit tests for more documentation.
/// In other words: don't block on 'listening' unless you have started the server.
/// The return value from 'listening' (first item in tuple) gives you some metrics on
/// how quickly suave started.
let web_server_async (config : SuaveConfig) (webpart : WebPart) =
  let content_folder = ParsingAndControl.resolve_directory config.home_folder
  let compression_folder = System.IO.Path.Combine(ParsingAndControl.resolve_directory config.compressed_files_folder, "_temporary_compressed_files")
  let all =
    config.bindings
    |> List.map (fun { scheme = proto; ip = ip; port = port } ->
      let http_runtime =
        ParsingAndControl.mk_http_runtime
          proto config.error_handler
          config.mime_types_map content_folder compression_folder config.logger config.session_provider
      ParsingAndControl.web_worker (ip, port, config.buffer_size, config.max_ops, http_runtime) webpart)
  let listening = all |> Seq.map fst |> Async.Parallel
  let server    = all |> Seq.map snd |> Async.Parallel |> Async.Ignore
  listening, server

/// Runs the web server and blocks waiting for the asynchronous workflow to be cancelled or
/// it returning itself.
let web_server (config : SuaveConfig) (webpart : WebPart) =
  Async.RunSynchronously(web_server_async config webpart |> snd, cancellationToken = config.ct)

/// The default configuration binds on IPv4, 127.0.0.1:8083 with a regular 500 Internal Error handler,
/// with a timeout of one minute for computations to run. Waiting for 2 seconds for the socket bind
/// to succeed.
let default_config : SuaveConfig =
  { bindings         = [ { scheme = HTTP; ip = IPAddress.Loopback; port = 8083us } ]
  ; error_handler    = default_error_handler
  ; listen_timeout   = TimeSpan.FromSeconds(2.)
  ; ct               = Async.DefaultCancellationToken
  ; buffer_size      = 8192 // 8 KiB
  ; max_ops          = 100
  ; mime_types_map   = Http.Writers.default_mime_types_map
  ; home_folder      = None
  ; compressed_files_folder = None
  ; logger           = Log.Loggers.sane_defaults_for Log.LogLevel.Info
  ; session_provider = new DefaultSessionProvider() }
