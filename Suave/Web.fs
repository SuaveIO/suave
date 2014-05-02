module Suave.Web

/// Parsing and control flow handling for webr requests
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

  /// Read the passed stream into buff until the EOL (CRLF) has been reached
  /// and returns an array containing excess data read past the marker
  let read_till_EOL (connection : Connection) select (preread : BufferSegment option) =

    let rec scan_data count (pair : BufferSegment) = async {
      match scan_crlf (ArraySegment(pair.buffer.Array, pair.offset, pair.length)) with
      // returns index relating to the original array
      | Some x when x = pair.offset ->
        if pair.length = 2 then
          //there is only a '\r','\n' left
          connection.free_buffer "read_till_EOL.scan_data (Some x = pair.offset (if1))" pair.buffer
          return count, None
        else 
          // TODO: not sure about correctness of this, after all we're passing the buffer back,
          // which piece of code owns it???
          connection.free_buffer "read_till_EOL.scan_data (Some x = pair.offset (if2))" pair.buffer
          return count, mk_buffer_segment pair.buffer (pair.offset + 2) (pair.length - 2)
      | Some index ->
        select (ArraySegment(pair.buffer.Array, pair.offset, index - pair.offset)) (index - pair.offset)
        let number_of_bytes_to_select = pair.length - index + pair.offset - 2
        if number_of_bytes_to_select = 0 then
          connection.free_buffer "read_till_EOL.scan_data (Some index)" pair.buffer
          return count + (index - pair.offset ), None
        else
          // TODO: free buffer?
          return count + (index - pair.offset ), mk_buffer_segment pair.buffer (index  + 2) number_of_bytes_to_select
      | None ->
        // TODO: free buffer?
        return! read_data count (Some pair)
      }

    and scan_data_x count (left : BufferSegment) (right : BufferSegment) = async  {
      let cn = left.length
      let dn = right.length

      // TODO: cleanup -- repeated code!
      match scan_crlf_x (ArraySegment(left.buffer.Array, left.offset, cn)) (ArraySegment(right.buffer.Array, right.offset, dn)) with
      | Some index when index < cn ->
        select (ArraySegment(left.buffer.Array, left.offset, index)) index
        let number_of_bytes_to_select = cn - index - 2
        if number_of_bytes_to_select = 0 then
          connection.free_buffer "read_till_EOL.scan_crlf_x (Some index < cn)" left.buffer
          // asumes d is empty
          return (count + index, None)
        else
          // assumes d is empty
          // TODO: free buffer?
          return (count + index, mk_buffer_segment left.buffer (left.offset + index + 2) number_of_bytes_to_select)
      | Some index ->
        // here we can free c's original buffer
        select (ArraySegment(left.buffer.Array, left.offset, left.length)) left.length
        connection.free_buffer "read_till_EOL.scan_crlf_x (Some index)" left.buffer
        select (ArraySegment(right.buffer.Array, right.offset, index - left.length)) (index - left.length)
        let number_of_bytes_to_select = dn - (index - cn) - 2
        if number_of_bytes_to_select = 0 then
          return (count + index, None)
        else
          return (count + index, mk_buffer_segment right.buffer (right.offset + (index - cn) + 2) number_of_bytes_to_select)
      | None ->
        // free c's original buffer
        select (ArraySegment(left.buffer.Array, left.offset, left.length)) left.length
        connection.free_buffer "read_till_EOL.scan_crlf_x (None)" left.buffer
        let count' = count + cn + dn - 2
        let number_of_bytes_to_select  = right.length - 1
        if number_of_bytes_to_select > 0 then
          select (ArraySegment(right.buffer.Array, right.offset, right.length-1)) number_of_bytes_to_select
          return! read_data (count' + number_of_bytes_to_select) (mk_buffer_segment right.buffer (right.offset + right.length - 1) 1)
        else
          return! read_data count' (Some right)
      }
    and read_data count (ahead : BufferSegment option) = async {
      let buff = connection.get_buffer "read_till_EOL.read_data"
      try
        let! b = connection.read buff
        if b > 0 then
          match ahead with
          | Some data ->
            return! scan_data_x count data { buffer = buff; offset = buff.Offset; length = b }
          | None ->
            return! scan_data count { buffer = buff; offset = buff.Offset; length = b }
        else
          return failwith "client closed"
      with ex ->
        connection.free_buffer "read_till_EOL.read_data (exception case)" buff
        return raise ex
      }
    match preread with
    | Some data ->
      if data.length > 1 then scan_data 0 data
      else read_data 0 preread
    | None ->
      read_data 0 preread

  /// Read the stream until the marker appears.
  let read_until (marker : byte array) (f : ArraySegment<_> -> int -> Async<unit>) (connection : Connection) (preread : BufferSegment option) =

    let marker_length = marker.Length

    let rec scan_data count (segment : BufferSegment) = async {
      match kmp_x marker (ArraySegment(segment.buffer.Array, segment.offset, segment.length)) with
      | Some 0 ->
        if segment.length = marker_length then
          connection.free_buffer "scan_data (Some 0)" segment.buffer
          return count, None
        else
          // TODO: verify buffer usage
          return count, mk_buffer_segment segment.buffer (segment.offset + marker_length) (segment.length - marker_length)
      | Some index ->
        do! f (ArraySegment(segment.buffer.Array, segment.offset, index)) index
        // discard the marker
        if index = segment.length - marker_length then
          connection.free_buffer "scan_data (Some index)" segment.buffer
          return (count, None)
        else
          return (count + index, mk_buffer_segment segment.buffer (segment.offset + index + marker_length) (segment.length - index - marker_length))
      | None ->
        return! read_data count (Some segment)
      }
    and scan_data_x  count (left : BufferSegment) (right: BufferSegment) = async {

      let cn = left.length
      let dn = right.length

      match kmp_x_x marker (ArraySegment(left.buffer.Array, left.offset,cn)) (ArraySegment(right.buffer.Array, right.offset,dn)) with
      | Some index when index < cn ->
        do! f (ArraySegment(left.buffer.Array, left.offset,index)) index
        let number_of_bytes_to_select = cn - index - marker_length
        if number_of_bytes_to_select = 0 then
          connection.free_buffer "scan_data (Some index < cn)" left.buffer
          return (count + index, None)
        else
          return (count + index, mk_buffer_segment left.buffer (left.offset + index + marker_length) number_of_bytes_to_select) // assumes d is empty
      | Some index ->
        do! f (ArraySegment(left.buffer.Array, left.offset, left.length)) left.length //here we can free c's original buffer --
        connection.free_buffer "scan_data (Some index)" left.buffer
        do! f (ArraySegment(right.buffer.Array, right.offset, index)) index
        let number_of_bytes_to_select = dn - (index - cn) - marker_length
        if number_of_bytes_to_select = 0 then
          return (count + index, None)
        else
          return (count + index, mk_buffer_segment right.buffer (right.offset + (index - cn) + marker_length) number_of_bytes_to_select)
      | None ->
        do! f  (ArraySegment(left.buffer.Array, left.offset, left.length)) left.length //free c's original buffer
        connection.free_buffer "scan_data (None)" left.buffer
        let number_of_bytes_to_select  = right.length - marker_length + 1
        if number_of_bytes_to_select > 0 then 
          do! f (ArraySegment(right.buffer.Array, right.offset, right.length-1)) number_of_bytes_to_select
          return! read_data (count + cn + number_of_bytes_to_select) (mk_buffer_segment right.buffer (right.offset + right.length - marker_length + 1) (marker_length - 1))
        else
          return! read_data (count + cn) (Some right)
      } 
    and read_data count (ahead : BufferSegment option) = async {
      let a = connection.get_buffer "read_util.read_data"
      let! bytes_read = connection.read a
      if bytes_read > 0 then
        match ahead with
        | Some data ->
          return! scan_data_x count data { buffer = a; offset = a.Offset ; length = bytes_read }
        | None ->
          return! scan_data count { buffer = a; offset = a.Offset ; length = bytes_read }
      else
        return failwith "client disconnected."
      }
    match preread with
    | Some data ->
      if data.length > marker.Length then scan_data 0 data
      else read_data 0 preread
    | None -> read_data 0 preread

  /// Read a line from the stream, calling to_string on the bytes before the EOL marker
  let read_line (connection : Connection) ahead (buf : ArraySegment<byte>) = async {
    let offset = ref 0
    let! count, rem = read_till_EOL connection (fun a count -> Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count; offset := !offset + count) ahead
    let result = ASCII.to_string buf.Array buf.Offset count
    return result, rem
  }

  /// Read all headers from the stream, returning a dictionary of the headers found
  let read_headers connection read (headers : Dictionary<string,string>) (buf : ArraySegment<byte>) =
    let rec loop (rem : BufferSegment option) = async {
      let offset = ref 0
      let! count, new_rem =
        read_till_EOL
          connection
          (fun a count -> Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count
                          offset := !offset + count)
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
    let read_bytes bytes_needed (missing : byte[]) read_offset = async {
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
      connection.free_buffer "read_post_data" a
      return (ArraySegment missing, !rem)
    }
    async {
      match read with
      | Some segment ->
        if segment.length >= bytes then
          return (ArraySegment(segment.buffer.Array, segment.offset, bytes), Some { buffer = segment.buffer; offset = segment.offset + bytes; length = segment.length - bytes })
        else 
          let missing = Array.zeroCreate bytes
          Array.blit segment.buffer.Array segment.offset missing 0 segment.length
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
                      line_buffer : Async<BufferSegment option> =
    let rec loop boundary read = async {

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
    and parse_content content_type content_disposition fieldname rem = async {
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
  let process_request proxy_mode
                      (request : HttpRequest)
                      (connection : Connection)
                      (bytes : BufferSegment option)
                      : Async<(HttpRequest * BufferSegment option) option> = async {

    let line_buffer = connection.line_buffer
    try
      let! (first_line : string), rem = read_line connection bytes line_buffer

      if first_line.Length = 0 then
        return None
      else
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
                        let cookie = parse_cookie x.Value
                        request.cookies.Add (fst(cookie.[0]),cookie))

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
    with ex ->
      return None
  }

  open System.Net
  open OpenSSL.SSL
  open OpenSSL.X509
  open Types
  open OpenSSL

  /// Load a readable plain-text stream, based on the protocol in use. If plain HTTP
  /// is being used, the stream is returned as it, otherwise a new SslStream is created
  /// to decipher the stream, without client certificates.
  let inline load_connection (logger : Log.Logger) proto (connection : Connection) = async{
    match proto with
    | HTTP       -> return connection
    | HTTPS cert ->
      let ssl = authenticate_as_server cert
      do! accept logger connection ssl
      return { connection with read = ssl_receive connection ssl; write = ssl_send connection ssl }
    }

  open System.Net.Sockets

  /// A HttpProcessor takes a HttpRequest instance, returning asynchronously a HttpRequest that has been parsed
  type HttpProcessor = HttpRequest -> Connection -> BufferSegment option -> Async<(HttpRequest * (BufferSegment option)) option>
  type RequestResult = Done

  let internal mk_request connection proto =
    { http_version   = null
    ; url            = null
    ; ``method``     = null
    ; query          = new Dictionary<string,string>()
    ; headers        = new Dictionary<string,string>()
    ; form           = new Dictionary<string,string>()
    ; raw_form       = null
    ; raw_query      = null
    ; cookies        = new Dictionary<string,(string*string)[]>()
    ; user_name      = null
    ; password       = null
    ; session_id     = null
    ; response       = new HttpResponse()
    ; files          = new List<HttpUpload>()
    ; is_secure      = match proto with HTTP -> false | HTTPS _ -> true
    ; trace          = Log.TraceHeader.Empty }

  let internal free context connection (s : BufferSegment option) =
    match s with
    | None -> ()
    | Some x -> connection.free_buffer context x.buffer

  /// Check if the web part can perform its work on the current request. If it can't
  /// it will return None and the run method will return.
  let internal run ctx web_part = async {
    match web_part ctx with // run the web part
    | Some x -> do! x
    | None -> return ()
  }

  let http_loop processor (runtime : HttpRuntime) req cn web_part =

    let rec loop (bytes : BufferSegment option) request = async {
      let verbose  = Log.verbose runtime.logger "Web.request_loop.loop" request.trace
      let verbosef = Log.verbosef runtime.logger "Web.request_loop.loop" request.trace
      verbose "-> processor"
      let! result = processor request cn bytes
      verbose "<- processor"

      match result with
      | None -> verbose "'result = None', exiting"
      | Some (request : HttpRequest, rem) ->
        let ctx = { request = request; runtime = runtime; connection = cn }
        try
          do! Async.WithTimeout (runtime.web_part_timeout, run ctx web_part)
        with
          | InternalFailure(_) as ex  -> free "http_loop.loop" cn rem; raise ex
          | :? TimeoutException as ex -> free "http_loop.loop" cn rem; raise ex
          | :? SocketIssue as ex      -> free "http_loop.loop" cn rem; raise ex
          | ex -> do! runtime.error_handler ex "Routing request failed" ctx
        if cn.is_connected () then
          match request.headers?connection with
          | Some (x : string) when x.ToLower().Equals("keep-alive") ->
            clear request
            verbosef (fun fmt -> fmt "'Connection: keep-alive' recurse, rem: %A" rem)
            return! loop rem request
          | Some _ ->
            free "http_loop.loop (case Some _)" cn rem;
            verbose "'Connection: close', exiting"
            return ()
          | None ->
            if request.http_version.Equals("HTTP/1.1") then
              clear request
              verbose "'Connection: keep-alive' recurse (!)"
              return! loop rem request
            else
              free "http_loop.loop (case None, else branch)" cn rem;
              verbose "'Connection: close', exiting"
              return ()
        else
          free "http_loop.loop (not connected)" cn rem;
          verbose "'is_connected = false', exiting"
          return ()
    }
    loop None req

  /// The request loop initialises a request with a processor to handle the
  /// incoming stream and possibly pass the request to the web parts, a protocol,
  /// a web part, an error handler, a timeout value for executing the web part
  /// in milliseconds and a Connection to use for read-write
  /// communication -- getting the initial request stream.
  let request_loop
    (processor  : HttpProcessor)
    (runtime    : HttpRuntime)
    (web_part   : WebPart)
    (connection : Connection) =

    async {
      let! connection = load_connection runtime.logger runtime.protocol connection
      let request     = mk_request connection runtime.protocol
      try
        do! http_loop processor runtime request connection web_part
      with
      | InternalFailure(_)
      | :? EndOfStreamException
      | :? IOException as ex
        when ex.InnerException <> null && ex.InnerException.GetType() = typeof<SocketException> ->
        "client disconnected" |> Log.verbose runtime.logger "web.request_loop" request.trace
        return ()
      | ex ->
        "request failed" |> Log.verbosee runtime.logger "web.request_loop" request.trace ex
        return ()
    }

  open Suave.Tcp

  let mk_http_runtime proto timeout error_handler mime_types home_directory compression_folder logger =
    { protocol           = proto
    ; web_part_timeout   = timeout
    ; error_handler      = error_handler
    ; mime_types_map     = mime_types
    ; home_directory     = home_directory
    ; compression_folder = compression_folder
    ; logger             = logger }

  /// Starts a new web worker, given the configuration and a web part to serve.
  let web_worker (ip, port, buffer_size, max_ops, runtime : HttpRuntime) (webpart : WebPart) =
    tcp_ip_server (ip, port, buffer_size, max_ops) runtime.logger (request_loop (process_request false) runtime webpart)

  let resolve_directory home_directory =
    match home_directory with
    | None   -> System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
    | Some s -> s

////////////////////////////////////////////////////

open System
open System.Net
open Suave.Types
open Suave.Http

/// The default error handler returns a 500 Internal Error in response to
/// thrown exceptions.
let default_error_handler (ex : Exception) msg (ctx : HttpContext) = async {
  let request = ctx.request
  msg |> Log.verbosee ctx.runtime.logger "Web.default_error_handler" ctx.request.trace ex
  if IPAddress.IsLoopback ctx.connection.ipaddr then
    do! (Response.response Codes.HTTP_500 (UTF8.bytes (sprintf "<h1>%s</h1><br/>%A" ex.Message ex)) ctx)
  else do! (Response.response Codes.HTTP_500 (UTF8.bytes (Codes.http_message Codes.HTTP_500)) ctx)
}

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
          proto config.web_part_timeout config.error_handler
          config.mime_types_map content_folder compression_folder config.logger
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
  ; web_part_timeout = TimeSpan.FromHours(5.)
  ; listen_timeout   = TimeSpan.FromSeconds(2.)
  ; ct               = Async.DefaultCancellationToken
  ; buffer_size      = 8192 // 8 KiB
  ; max_ops          = 100
  ; mime_types_map   = Http.Writers.default_mime_types_map
  ; home_folder      = None
  ; compressed_files_folder = None
  ; logger           = Log.Loggers.sane_defaults_for Log.LogLevel.Info }
