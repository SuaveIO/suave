module Suave.Web

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
/// Chunk sizes

/// When parsing headers and lines we will prefer reading from the stream in small chunk sizes
let SHORT_BUFFER_SIZE = 512

/// When parsing POST data and file uploads we will read from the stream in larger chunk sizes
let BIG_BUFFER_SIZE = 1048576

/// Returns the index of the first CRLF in the buffer
let inline scan_crlf (b : ArraySegment<byte>) =
  let a = b.Array
  let rec loop i =
    if i > b.Offset + b.Count - 1 then None
    elif i > 0 && a.[i - 1] = EOL.[0] && a.[i] = EOL.[1] then Some (i - 1)
    else loop (i + 1)
  loop b.Offset

/// Read the passed stream into buff until the EOL (CRLF) has been reached 
/// and returns an array containing excess data read past the marker
let inline read_till_EOL (connection : Connection) (buff : byte[]) (preread : ArraySegment<_>) chunk_size =

  let rec scan_data count (segment : ArraySegment<byte>) = async {
    let bytes = segment.Array
    match scan_crlf segment with
    | Some x when x = segment.Offset ->
      return (count, ArraySegment( bytes, segment.Offset + 2, segment.Count - 2))
    | Some index ->
      Array.blit bytes segment.Offset buff count (index - segment.Offset)
      return (count + (index - segment.Offset ), ArraySegment( bytes, index  + 2, segment.Count - index + segment.Offset - 2))
    | None ->
      return! read_data count segment
    }
  and read_data count (ahead : ArraySegment<_>)  = async {
    let inp = Array.zeroCreate SHORT_BUFFER_SIZE
    let! bytesread = connection.reader (fun a -> Array.blit a.Array a.Offset inp 0 a.Count; a.Count)
    if bytesread <> 0 then
      let sub = ArraySegment<byte>(inp, 0, bytesread)
      if ahead.Count = 0 then 
        return! scan_data count sub
      else
        let arr = Array.zeroCreate (bytesread + ahead.Count)
        Array.blit (ahead.Array) ahead.Offset arr 0 ahead.Count
        Array.blit inp 0 arr ahead.Count bytesread
        return! scan_data count (ArraySegment arr)
    else return (count, ahead)
    }
  if preread.Count > 1 then scan_data 0  preread 
  else read_data 0  preread

/// Read the stream until the marker appears.
let read_until (marker : byte array) (f : ArraySegment<_> -> int -> Async<unit>) (connection : Connection) (preread : ArraySegment<_>) chunk_size =

  let rec scan_data count  (segment : ArraySegment<byte>)  = async {
    //Log.tracef (fun fmt -> fmt "read_until -> scan_data count:%d segment.Offset: %d segment.Count: %d" count segment.Offset segment.Count)
    // kmp_x returns an index relative to the Offset of segment
    match kmp_x marker segment  with
    | Some 0 ->
      return (count,  ArraySegment<_>(segment.Array, segment.Offset + marker.Length, segment.Count - marker.Length))
    | Some index ->
      do! f segment index
      return (count + index, new ArraySegment<_>(segment.Array, segment.Offset + index + marker.Length, segment.Count - index - marker.Length))
    | None -> 
      do! f segment (segment.Count - marker.Length)
      return! read_data (count + segment.Count - marker.Length) (ArraySegment<_>(segment.Array, segment.Offset + segment.Count - marker.Length, marker.Length))
    }
  and read_data count (ahead : ArraySegment<_>) = async {
      //Log.tracef (fun fmt -> fmt "read_until -> read_data count:%d segment.Offset: %d segment.Count: %d" count ahead.Offset ahead.Count)
      let inp = Array.zeroCreate chunk_size 
      let! bytesread = connection.reader  (fun a -> Array.blit a.Array a.Offset inp 0 a.Count; a.Count)
     if bytesread <> 0 then
      let sub = ArraySegment<byte>(inp, 0, bytesread)
      if ahead.Count = 0 then 
        return! scan_data count sub
      else
        let arr = Array.zeroCreate (bytesread + ahead.Count)
        Array.blit (ahead.Array) ahead.Offset arr 0 ahead.Count
        Array.blit inp 0 arr ahead.Count bytesread
        return! scan_data count (ArraySegment arr)
    else return (count, ahead)
    }
  
  if preread.Count > marker.Length then scan_data 0 preread
  else read_data 0 preread

/// Alternative read_till_EOL
let _read_till_EOL(connection : Connection) (buff : byte[]) (preread : ArraySegment<_>) =
  read_until EOL (fun b c -> async { do Array.blit b.Array b.Offset buff 0 c }) connection preread

/// Convert the byte array of ASCII-encoded chars to a string, starting at 'index' for 'count' characters
/// (each character is necessarily one byte)
let to_string (buff : byte[]) (index : int) (count : int) =
  Encoding.ASCII.GetString(buff, index, count)

/// The maximun line size we will read from the stream
let max_line_size = 1024

/// Read a line from the stream, calling to_string on the bytes before the EOL marker
let read_line (connection:Connection) ahead = async {
  let buf = Array.zeroCreate max_line_size
  let! count, rem = read_till_EOL connection buf ahead SHORT_BUFFER_SIZE
  return (to_string buf 0 count, rem)
}

/// Read all headers from the stream, returning a dictionary of the headers found
let inline read_headers connection read (headers: Dictionary<string,string>) =
  let rec loop (rem: ArraySegment<_>) = async {
    let buf = Array.zeroCreate max_line_size
    let! count,new_rem = read_till_EOL connection buf rem SHORT_BUFFER_SIZE
    if count <> 0 then
      let line = to_string buf 0 count
      let indexOfColon = line.IndexOf(':')
      headers.Add (line.Substring(0,indexOfColon).ToLower(),line.Substring(indexOfColon+1).TrimStart())
      return! loop new_rem
    else return new_rem
  }
  loop read

open Suave.Types

/// Gets the empty query string dictionary
let empty_query_string () = new Dictionary<string,string>()

/// Gets the query from the HttpRequest // TODO: Move to a module for HttpRequest
let query (x : HttpRequest) = x.query
/// Gets the form from the HttpRequest // TODO: Move to a module for HttpRequest
let form  (x : HttpRequest) = x.form

/// Parse the data in the string to a dictionary, assuming k/v pairs are separated
/// by the ampersand character.
let parse_data (s : string) (param : Dictionary<string,string>) =
  s.Split('&')
  |> Array.iter (fun (k : string) ->
       k.Split('=')
       |> (fun d -> if d.Length = 2 then param.Add(d.[0], System.Web.HttpUtility.UrlDecode(d.[1]))))
  param

/// TO BE DONE
let parse_url (line : string) (dict : Dictionary<string,string>) =
  let parts = line.Split(' ')
  if parts.Length < 2 || parts.Length > 3 then failwith "400"
  let indexOfMark = parts.[1].IndexOf('?')

  if indexOfMark > 0 then
    let raw_query = parts.[1].Substring(indexOfMark + 1)
    (parts.[0], parts.[1].Substring(0,indexOfMark), parse_data raw_query dict, "?" + raw_query)
  else
    (parts.[0],parts.[1],dict, String.Empty)

/// Read the post data from the stream, given the number of bytes that makes up the post data.
let read_post_data (connection : Connection) (bytes : int) (read : ArraySegment<_>) = async {
    if read.Count >= bytes then
      return (ArraySegment( read.Array, read.Offset,bytes), ArraySegment( read.Array, read.Offset + bytes, (read.Count - bytes)))
    else 
      let missing = Array.zeroCreate bytes
      Array.blit read.Array read.Offset missing 0 read.Count
      let! _ = connection.reader (fun a -> Array.blit a.Array a.Offset missing read.Count (bytes - read.Count); a.Count)
      return (ArraySegment missing, ArraySegment(Array.zeroCreate(0)))
  }

/// Parse the cookie data in the string into a dictionary
let parse_cookie (s : string) =
  s.Split(';')
  |> Array.map (fun (x : string) ->
                let parts = x.Split('=')
                (parts.[0], parts.[1]))

/// Parse a string array of key-value-pairs, combined using the equality character '='
/// into a dictionary
let parse_key_value_pairs arr =
  let dict = new Dictionary<string,string>()
  arr
  |> Array.iter (fun (x : String) ->
                 let parts = x.Split('=')
                 dict.Add(parts.[0], parts.[1]))
  dict

/// Parse the header parameters into key-value pairs, as a dictionary.
/// Fails if the header is a None.
let header_params (header : string option) =
  match header with
  | Some x ->
    let parts = x.Split(';') |> Array.map (fun x -> x.TrimStart())
    parse_key_value_pairs (Array.sub parts 1 (parts.Length - 1))
  | None ->
    failwith "did not find header, because header_params received None"

/// Parses multipart data from the stream, feeding it into the HttpRequest's property Files.
let parse_multipart (connection : Connection) boundary (request : HttpRequest) (ahead : ArraySegment<_>) : Async<ArraySegment<_>> =
  let rec loop boundary read = async {

    let! firstline, read = read_line connection read

    if not(firstline.Equals("--")) then

      let part_headers = new Dictionary<string,string>()
      let! rem = read_headers connection read part_headers

      let content_disposition = look_up part_headers "content-disposition"

      let fieldname = (header_params content_disposition) ? name |> opt

      let content_type = look_up part_headers "content-type"

      return! parse_content content_type content_disposition fieldname rem
    else return (ArraySegment(Array.zeroCreate(0)))
    }
  and parse_content content_type content_disposition fieldname rem = async {
    match content_type with
    | Some(x) when x.StartsWith("multipart/mixed") ->
      let subboundary = "--" + x.Substring(x.IndexOf('=') + 1).TrimStart()
      return! loop subboundary rem
    | Some(x) ->
      let temp_file_name = Path.GetTempFileName()
      use temp_file = new FileStream(temp_file_name,FileMode.Truncate)
      Log.trace (fun () -> "parse_content -> read_until")
      let! a,b = read_until (bytes(eol + boundary)) (fun x y -> async { do! temp_file.AsyncWrite(x.Array,x.Offset,y) } ) connection rem BIG_BUFFER_SIZE
      Log.trace (fun () -> "parse_content <- read_until")
      let file_leght = temp_file.Length
      temp_file.Close()
      if  file_leght > int64(0) then
        let filename =
          (header_params content_disposition) ? filename |> opt
        request.files.Add(new HttpUpload(fieldname,filename,content_type |> opt,temp_file_name))
      else
        File.Delete temp_file_name
      return! loop boundary b
    | None ->
      use mem = new MemoryStream()
      let! a,b = read_until (bytes(eol + boundary)) (fun x y -> async { do! mem.AsyncWrite(x.Array,x.Offset,y) } ) connection rem BIG_BUFFER_SIZE
      let byts = mem.ToArray()
      request.form.Add(fieldname, (to_string byts 0 byts.Length)) 

      return! loop boundary b
    }
  loop boundary ahead 

/// Process the request, reading as it goes from the incoming 'stream', yielding a HttpRequest
/// when done
let process_request proxy_mode (request : HttpRequest) (bytes:ArraySegment<_>) = async {

  let connection = request.connection

  Log.tracef(fun fmt -> fmt "web:process_request proxy:%b bytes:%A -> read_line" proxy_mode bytes)
  let! (first_line : string), rem = read_line connection bytes
  Log.tracef(fun fmt -> fmt "web:process_request proxy:%b <- read_line" proxy_mode)

  if first_line.Length = 0 then
    return None, rem
  else
    let meth, url, _, raw_query as q = parse_url first_line request.query

    request.url      <- url
    request.``method``   <- meth
    request.raw_query <- raw_query

    let headers = request.headers
    let! rem = read_headers connection rem headers
    
    // won't continue parsing if on proxyMode with the intention of forwarding the stream as it is
    if proxy_mode then return Some request, rem
    else
      request.headers
      |> Seq.filter (fun x -> x.Key.Equals("cookie"))
      |> Seq.iter (fun x ->
                    let cookie = parse_cookie x.Value
                    request.cookies.Add (fst(cookie.[0]),cookie))

      if meth.Equals("POST") then

        let content_encoding =
            match headers.TryGetValue("content-type") with
            | true, encoding -> Some encoding
            | false, _ -> None

        let content_length = Convert.ToInt32(headers.["content-length"])

        match content_encoding with
        | Some ce when ce.StartsWith("application/x-www-form-urlencoded") ->
              let! (rawdata : ArraySegment<_>),_ = read_post_data connection content_length rem
              let str = to_string rawdata.Array rawdata.Offset rawdata.Count
              let _  = parse_data str request.form
              let raw_form = Array.zeroCreate rawdata.Count
              Array.blit rawdata.Array rawdata.Offset raw_form 0 rawdata.Count
              request.raw_form <- raw_form
              return Some request, rem
        | Some ce when ce.StartsWith("multipart/form-data") ->
              let boundary = "--" + ce.Substring(ce.IndexOf('=')+1).TrimStart()
              let! (rem : ArraySegment<_>) = parse_multipart connection boundary request rem
              assert (rem.Count = 0)
              return Some request, rem
        | Some _ | None ->
              let! (rawdata : ArraySegment<_>),_ = read_post_data connection content_length rem
              let raw_form = Array.zeroCreate rawdata.Count
              Array.blit rawdata.Array rawdata.Offset raw_form 0 rawdata.Count
              request.raw_form <- raw_form
              return Some request, rem
      else return Some request, rem
}

open System.Net
open OpenSSL.SSL
open OpenSSL.X509

open Types
open OpenSSL

/// Load a readable plain-text stream, based on the protocol in use. If plain HTTP
/// is being used, the stream is returned as it, otherwise a new SslStream is created
/// to decipher the stream, without client certificates.
let inline load_connection proto (connection : Connection) = async{
  match proto with
  | HTTP       -> return connection
  | HTTPS cert -> 
    let ssl = authenticate_as_server cert
    do! accept connection ssl
    return { connection with reader = ssl_receive connection ssl; writer = ssl_send connection ssl }
  }

open System.Net.Sockets

/// A HttpProcessor takes a HttpRequest instance, returning asynchronously a HttpRequest that has been parsed
type HttpProcessor = HttpRequest -> ArraySegment<byte> -> Async<HttpRequest option * ArraySegment<byte>>
type RequestResult = Done

/// The request loop initialises a request against a web part, with a protocol, a processor to handle the
/// incoming stream, an error handler, a timeout value (milliseconds) and a TcpClient to use for read-write
/// communication -- getting the initial request stream.
let request_loop webpart proto (processor : HttpProcessor) error_handler (timeout : TimeSpan) (connection : Connection) =
  /// Evaluate the (web part) action as an async value, handling errors with error_handler should one
  /// occur.
  let eval_action x r = async {
    try
      do! x
    with
    | InternalFailure(_) as ex -> raise ex
    | ex -> do! error_handler ex "web:request_loop - action failed" r
  }
  /// Check if the web part can perform its work on the current request. If it can't
  /// it will return None and the run method will return.
  let run request = async {
    match webpart request with // routing
    | Some x -> do! eval_action x request
    | None -> return ()
  }

  let rec loop (bytes:ArraySegment<_>) request = async {
    Log.trace(fun () -> "web:request_loop:loop -> processor")
    let! result, rem = processor request bytes
    Log.trace(fun () -> "web:request_loop:loop <- processor")

    match result with
    | Some (request : HttpRequest) ->
      try
        Log.trace(fun () -> "web:request_loop:loop -> unblock")
        do! Async.WithTimeout (timeout, run request)
        Log.trace(fun () -> "web:request_loop:loop <- unblock")
      with
        | InternalFailure(_) as ex  -> raise ex
        | :? TimeoutException as ex -> do! error_handler ex "script timeout" request
        | ex -> do! error_handler ex "Routing request failed" request
      match request.headers?connection with
      | Some (x : string) when x.ToLower().Equals("keep-alive") ->
        clear request
        Log.tracef(fun fmt -> fmt "web:request_loop:loop 'Connection: keep-alive' recurse (!), rem: %A" rem)
        return! loop rem request
      | _ ->
        Log.trace(fun () -> "web:request_loop:loop  'Connection: close', exiting")
        return ()
    | None ->
      Log.trace(fun () -> "web:request_loop:loop 'result = None', exiting")
      return ()
  }
  async {
    let! connection = load_connection proto connection
    let request = {
      connection = connection;
      url = null;
      ``method`` = null;
      query = new Dictionary<string,string>();
      headers = new Dictionary<string,string>();
      form = new Dictionary<string,string>();
      raw_form = null;
      raw_query = null;
      cookies = new Dictionary<string,(string*string)[]>();
      user_name = null;
      password = null;
      session_id = null;
      response = new HttpResponse();
      files = new List<HttpUpload>()
      remote_address = connection.ipaddr;
      is_secure = match proto with HTTP -> false | HTTPS _ -> true;
      }
    try
      return! loop (ArraySegment(Array.empty)) request
    with
    | InternalFailure(_)
    | :? EndOfStreamException
    | :? IOException as ex
      when ex.InnerException <> null && ex.InnerException.GetType() = typeof<SocketException> ->
      Log.trace(fun () -> "web:request_loop - client disconnected")
      return ()
    | ex ->
      Log.tracef(fun fmt -> fmt "web:request_loop - Request failed.\n%A" ex)
      return ()
  }

/// Parallelise the map of 'f' onto all items in the 'input' seq.
let parallelize input f = input |> Seq.map f |> Async.Parallel

open Suave.Tcp

/// Gets whether the passed ip is a local IPv4 or IPv6 address.
/// Example: 127.0.0.1, ::1 return true. If the IP cannot be parsed,
/// returns false.
let is_local_address (ip : string) =
  match IPAddress.TryParse ip with
  | false, _   -> false
  | true,  ip' -> IPAddress.IsLoopback ip'

/// The default error handler returns a 500 Internal Error in response to
/// thrown exceptions.
let default_error_handler (ex : Exception) msg (request : HttpRequest) = async {
  Log.logf "web:default_error_handler - %s.\n%A" msg ex
  if is_local_address request.remote_address then
    do! (response 500 "Internal Error" (bytes_utf8 (sprintf "<h1>%s</h1><br/>%A" ex.Message ex)) request)
  else do! (response 500 "Internal Error" (bytes_utf8 (request.remote_address)) request)
}

/// Starts a new web worker, given the configuration and a web part to serve.
let web_worker (proto, ip, port, error_handler, timeout) (webpart : WebPart) =
  tcp_ip_server (ip, port) (request_loop webpart proto (process_request false) error_handler timeout)

/// Returns the webserver as a tuple of 1) an async computation the yields unit when
/// the web server is ready to serve quests, and 2) an async computation that yields
/// when the web server is being shut down and is being terminated. The async values
/// returned are not 'hot' in the sense that they have started running, so you must manually
/// start the 'server' (second item in tuple), as this starts the TcpListener.
/// Have a look at the example and the unit tests for more documentation.
/// In other words: don't block on 'listening' unless you have started the server.
let web_server_async (config : SuaveConfig) (webpart : WebPart) =
  let all =
    config.bindings
    |> List.map (fun { scheme = proto; ip = ip; port = port } ->
        web_worker (proto, ip, port, config.error_handler, config.timeout) webpart)
  let listening = all |> Seq.map fst |> Async.Parallel |> Async.Ignore
  let server    = all |> Seq.map snd |> Async.Parallel |> Async.Ignore
  listening, server

/// Runs the web server and blocks waiting for the asynchronous workflow to be cancelled or
/// it returning itself.
let web_server (config : SuaveConfig) (webpart : WebPart) =
  Async.RunSynchronously(web_server_async config webpart |> snd, cancellationToken = config.ct)

/// The default configuration binds on IPv4, 127.0.0.1:8083 with a regular 500 Internal Error handler,
/// with a timeout of one minute for computations to run. Waiting for 2 seconds for the socket bind
/// to succeed.
let default_config =
  { bindings       = [ { scheme = HTTP; ip = IPAddress.Loopback; port = 8083us } ]
  ; error_handler  = default_error_handler
  ; timeout        = TimeSpan.FromMinutes(1.)
  ; listen_timeout = TimeSpan.FromSeconds(2.)
  ; ct             = Async.DefaultCancellationToken }
