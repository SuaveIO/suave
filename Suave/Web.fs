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

  /// Free up a list of buffers
  let internal free context connection (s : BufferSegment list) =
    List.iter (fun x -> connection.free_buffer context x.buffer) s

  let skip_buffers (pairs : BufferSegment list) (number : int) :  BufferSegment list =
    let rec loop xxs acc = 
      match xxs with
      | [] -> []
      | x :: tail ->
        if x.length + acc >= number then 
          let segment = mk_buffer_segment x.buffer (x.offset  + (number - acc)) (x.length - number + acc)
          segment :: tail
        else loop tail (acc + x.length)
    loop pairs 0

  let split index (pairs : BufferSegment list) connection select marker_lenght : Async<int * BufferSegment list>=
    let rec loop xxs acc count =  async {
      match xxs with
      | [] -> return count, []
      | pair :: tail ->
        if acc + pair.length < index then
          do! select (ArraySegment(pair.buffer.Array, pair.offset, pair.length)) pair.length
          connection.free_buffer "Web.split" pair.buffer
          return! loop tail (acc + pair.length) (count + acc + pair.length)
        elif acc + pair.length >= index then
          let bytes_read = index - acc
          do! select (ArraySegment(pair.buffer.Array, pair.offset, bytes_read)) bytes_read
          let remaining = pair.length - bytes_read
          if remaining = marker_lenght then
            connection.free_buffer "Web.split" pair.buffer
            return count + bytes_read, tail
          else
            
            if remaining - marker_lenght >= 0 then
              return count + bytes_read,  mk_buffer_segment pair.buffer (pair.offset  + bytes_read  + marker_lenght) (remaining - marker_lenght) :: tail
            else
              let new_tail = skip_buffers tail (marker_lenght - remaining) 
              return count + bytes_read,  new_tail
        else return failwith "Web.split: invalid case"
      }
    loop pairs 0 0

  type ScanResult = NeedMore of (BufferSegment list) | Found of (int * BufferSegment list)

  /// Iterates over a BufferSegment list looking for a marker, data before the marker 
  /// is sent to the function select and the corresponding buffers are released
  let scan_marker marker select connection (pairs : BufferSegment list) = async {

    match kmp_z marker pairs with
    | Some x -> 
      let! res = split x pairs connection select marker.Length
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
        assert (b.length >= 0)
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

  let read_more_data connection continuation buffer_segment_list = async {
    let buff = connection.get_buffer "read_till_pattern.loop"
    let! result = read_data connection buff
    match result with
    | Choice1Of2 data ->
      return! continuation (buffer_segment_list @ [data])
    | Choice2Of2 error ->
      for b in buffer_segment_list do
        do connection.free_buffer "read_till_pattern.loop" b.buffer
      do connection.free_buffer "read_till_pattern.loop" buff
      return Choice2Of2 error
    }

  /// Read the passed stream into buff until the EOL (CRLF) has been reached
  /// and returns an array containing excess data read past the marker
  let read_till_pattern (connection : Connection) (preread : BufferSegment list) scan_data =

    let rec loop state = async {
      let! res = scan_data connection state
      match res with
      | Found a ->
        return Choice1Of2 a
      | NeedMore buffer_segment_list ->
        return! read_more_data connection loop buffer_segment_list
    }
    loop preread

  let read_till_EOL (connection : Connection) select (preread : BufferSegment list) =
    read_till_pattern connection preread (scan_marker EOL select)

  /// Read the stream until the marker appears.
  let read_until (marker : byte []) (select : ArraySegment<_> -> int -> Async<unit>) (connection : Connection) (preread : BufferSegment list) =
    read_till_pattern connection preread (scan_marker marker select)

  /// Read a line from the stream, calling to_string on the bytes before the EOL marker
  let read_line (connection : Connection) ahead (buf : ArraySegment<byte>) = socket {
    let offset = ref 0
    let! count, rem = read_till_EOL connection (fun a count -> async { Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count; offset := !offset + count }) ahead
    let result = ASCII.to_string buf.Array buf.Offset count
    return result, rem
  }

  /// Read all headers from the stream, returning a dictionary of the headers found
  let read_headers connection read (buf : ArraySegment<byte>) =
    let rec loop (rem : BufferSegment list) headers = socket {
      let offset = ref 0
      let! count, new_rem =
        read_till_EOL
          connection
          (fun a count -> async { Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count; offset := !offset + count })
          rem
      if count <> 0 then
        let line = ASCII.to_string buf.Array buf.Offset count
        let indexOfColon = line.IndexOf(':')
        let header = (line.Substring(0, indexOfColon).ToLower(), line.Substring(indexOfColon+1).TrimStart())
        return! loop new_rem (header :: headers)
      else return (headers, new_rem)
    }
    loop read []

  open Suave.Types

  let inline array_segment_from_buffer_segment b =
    ArraySegment(b.buffer.Array, b.offset, b.length)

  /// Read the post data from the stream, given the number of bytes that makes up the post data.
  let read_post_data (connection : Connection) (bytes : int) select (read : BufferSegment list) : SocketOp<BufferSegment list> =

    let rec loop n xxs : SocketOp<BufferSegment list> =
      socket {
        match xxs with
        | segment :: tail ->
          if segment.length > n then
            do! lift_async <| select (array_segment_from_buffer_segment { segment with offset = n }) n
            return { buffer = segment.buffer; offset = segment.offset + n; length = segment.length - n } :: tail
          else
            do! lift_async <| select (array_segment_from_buffer_segment segment) segment.length
            do connection.free_buffer "read_post_data:loop" segment.buffer
            return! loop (n - segment.length) tail
        | [] ->
          if n = 0 then
            return []
          else
            return! read_more_data connection (loop n) []
      }
    loop bytes read

  /// Parses multipart data from the stream, feeding it into the HttpRequest's property Files.
  let parse_multipart (connection : Connection)
                      boundary
                      (request : HttpRequest)
                      (ahead : BufferSegment list)
                      line_buffer : SocketOp<HttpRequest * BufferSegment list> =

    let rec loop boundary rem (r : HttpRequest) = socket {

      let! firstline, read = read_line connection rem line_buffer

      if not(firstline.Equals("--")) then

        let! part_headers, rem = read_headers connection read line_buffer

        let content_disposition =  part_headers %% "content-disposition"

        let fieldname = 
          (header_params content_disposition) ? name 
          |> Option.get
          |> (fun x -> x.Trim('"'))

        let content_type = part_headers %% "content-type"

        match content_type with
        | Some(x) when x.StartsWith("multipart/mixed") ->
          let subboundary = "--" + x.Substring(x.IndexOf('=') + 1).TrimStart()
          return! loop subboundary rem r
        | Some(x) ->
          let temp_file_name = Path.GetTempFileName()
          use temp_file = new FileStream(temp_file_name, FileMode.Truncate)
          let! a, b = read_until (ASCII.bytes(eol + boundary)) (fun x y -> async { do! temp_file.AsyncWrite(x.Array, x.Offset, y) } ) connection rem
          let file_length = temp_file.Length
          temp_file.Close()
          if file_length > int64(0) then
            let filename =
              (header_params content_disposition) ? filename |> Option.get |> (fun x -> x.Trim('"'))
            let upload = new HttpUpload(fieldname, filename, content_type |> Option.get, temp_file_name)
            return! loop boundary b { r with files = upload :: r.files }
          else
            File.Delete temp_file_name
            return! loop boundary b r
          
        | None ->
          use mem = new MemoryStream()
          let! a, b = read_until (ASCII.bytes(eol + boundary)) (fun x y -> async { do! mem.AsyncWrite(x.Array, x.Offset, y) } ) connection rem
          let byts = mem.ToArray()
          return! loop boundary b { r with multipart_fields = (fieldname,ASCII.to_string byts 0 byts.Length)::(r.multipart_fields) }
      else 
        return (r,read)
      }
    loop boundary ahead request

  let parse_trace_headers (headers : NameValueList) =
    let parse_uint64 = (function | true, value -> Some value
                                 | false, _    -> None
                       ) << UInt64.TryParse
    let parent = "x-b3-spanid"  |> get_first headers |> Option.bind parse_uint64
    let trace  = "x-b3-traceid" |> get_first headers |> Option.bind parse_uint64
    Log.TraceHeader.Create(trace, parent)

  /// Reads raw POST data
  let get_raw_post_data connection content_length rem =
    socket {
      let offset = ref 0
      let raw_form = Array.zeroCreate content_length
      let! (rem : BufferSegment list) = read_post_data connection content_length (fun a count -> async { Array.blit a.Array a.Offset raw_form !offset count; offset := !offset + count }) rem
      return raw_form , rem
    }

  /// Process the request, reading as it goes from the incoming 'stream', yielding a HttpRequest
  /// when done
  let process_request proxy_mode is_secure (bytes : BufferSegment list) connection : SocketOp<(HttpRequest * BufferSegment list) option> = socket {

    let line_buffer = connection.line_buffer

    let! (first_line : string), rem = read_line connection bytes line_buffer

    let meth, url, raw_query, http_version = parse_url first_line
    let! headers, rem = read_headers connection rem line_buffer

    let request = { http_version = http_version
      ; url         = url
      ; ``method``  = meth
      ; headers     = headers
      ; raw_form    = Array.empty
      ; raw_query   = raw_query
      ; files       = []
      ; multipart_fields = []
      ; trace       = parse_trace_headers headers
      ; is_secure   = is_secure
      ; ipaddr      = connection.ipaddr
      }

    // won't continue parsing if on proxyMode with the intention of forwarding the stream as it is
    // TODO: proxy mode might need headers and contents of request, but won't get it through this impl
    if proxy_mode then return Some (request, rem)
    else

      if meth.Equals("POST") || meth.Equals("PUT") then

        let content_encoding = request.headers %% "content-type"

        match request.headers %% "content-length" with 
        | Some content_length_string ->
          let content_length = Convert.ToInt32(content_length_string)

          match content_encoding with
          | Some ce when ce.StartsWith("application/x-www-form-urlencoded") ->
            let! raw_form, rem = get_raw_post_data connection content_length rem
            return Some ({ request with raw_form = raw_form}, rem)
          | Some ce when ce.StartsWith("multipart/form-data") ->
            let boundary = "--" + ce.Substring(ce.IndexOf('=')+1).TrimStart()
            let! r, rem = parse_multipart connection boundary request rem line_buffer
            return Some (r, rem)
          | Some _ | None ->
            let! raw_form, rem = get_raw_post_data connection content_length rem
            return Some ({ request with raw_form = raw_form}, rem)
        | None ->  return Some (request, rem)
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
    { http_version   = ""
    ; url            = ""
    ; ``method``     = ""
    ; headers        = []
    ; raw_form       = Array.empty
    ; raw_query      = ""
    ; files          = []
    ; multipart_fields = []
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

  // TODO: make function continuous
  let write_content context connection = function
    | Bytes b -> socket {
      let! (content : byte []) = Compression.transform b context connection
      // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
      do! async_writeln connection (String.Concat [| "Content-Length: "; content.Length.ToString() |])
      do! async_writeln connection ""
      if content.Length > 0 then
        do! connection.write (new ArraySegment<_>(content, 0, content.Length))
      }
    | SocketTask f -> f connection

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
  let internal run ctx (web_part : WebPart) connection = 
    let execute _ = async {
      try  
          let! q  = web_part ctx
          return q
        with ex ->
          return! ctx.runtime.error_handler ex "request failed" ctx
    }
    socket {
      let! result = lift_async <| execute ()
      match result with 
      | Some executed_part ->
        return! response_f executed_part connection
      | None -> return ()
  }

  type HttpConsumer =
    | WebPart of WebPart
    | SocketPart of (HttpContext -> Async<(Connection -> SocketOp<unit>) option >)

  let http_loop (proxy_mode : bool) (runtime : HttpRuntime) (consumer : HttpConsumer) (connection : Connection) =

    let rec loop (bytes : BufferSegment list) = socket {

      let verbose  = Log.verbose runtime.logger "Web.request_loop.loop" Log.TraceHeader.Empty
      let verbosef = Log.verbosef runtime.logger "Web.request_loop.loop" Log.TraceHeader.Empty

      verbose "-> processor"
      let! result = process_request proxy_mode (match runtime.protocol with HTTP -> false | HTTPS _ -> true) bytes connection
      verbose "<- processor"

      match result with
      | None -> verbose "'result = None', exiting"
      | Some (request : HttpRequest, rem) ->
        let ctx = { request = request; user_state = Map.empty; runtime = runtime; response = { status = HTTP_404; headers = []; content = NullContent }}
        match consumer with
        | WebPart web_part ->
          do! run ctx web_part connection
        | SocketPart writer ->
          let! intermediate = lift_async <| writer ctx
          match intermediate with
          | Some task ->
            do! task connection
          | None -> () // do nothing
        if connection.is_connected () then
          match request.headers %% "connection" with
          | Some (x : string) when x.ToLower().Equals("keep-alive") ->
            verbosef (fun fmt -> fmt "'Connection: keep-alive' recurse, rem: %A" rem)
            return! loop rem
          | Some _ ->
            free "http_loop.loop (case Some _)" connection rem
            verbose "'Connection: close', exiting"
            return ()
          | None ->
            if request.http_version.Equals("HTTP/1.1") then
              verbose "'Connection: keep-alive' recurse (!)"
              return! loop rem
            else
              free "http_loop.loop (case None, else branch)" connection rem
              verbose "'Connection: close', exiting"
              return ()
        else
          free "http_loop.loop (not connected)" connection rem
          verbose "'is_connected = false', exiting"
          return ()
    }
    loop []

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
      do! http_loop proxy_mode runtime consumer connection
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
