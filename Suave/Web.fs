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

/// Returns the index of the first CRLF in the buffer
let inline scan_crlf (b : ArraySegment<byte>) =
  let a = b.Array
  let rec loop i =
    if i > b.Offset + b.Count - 1 then None
    elif i > 0 && a.[i - 1] = EOL.[0] && a.[i] = EOL.[1] then Some (i - 1)
    else loop (i + 1)
  loop b.Offset

/// Returns the index of the first CRLF in the union of two ArraySegment
let inline scan_crlf_x (c : ArraySegment<byte>) (d : ArraySegment<byte>) =
  let a = unite c d
  let rec loop i =
    if i > c.Count + d.Count - 1 then None
    elif i > 0 && a (i - 1) = EOL.[0] && a i = EOL.[1] then Some (i - 1)
    else loop (i + 1)
  loop 0

type BufferSegment = { 
  buffer : ArraySegment<byte>;
  offset : int;
  lenght : int;
  }

let inline mk_buffer_segment buffer offset lenght = 
  Some { buffer = buffer; offset = offset; lenght = lenght}

/// Read the passed stream into buff until the EOL (CRLF) has been reached 
/// and returns an array containing excess data read past the marker
let read_till_EOL (connection : Connection) (select) (preread : BufferSegment option) =

  let rec scan_data count (pair : BufferSegment) = async {
    match scan_crlf (ArraySegment(pair.buffer.Array, pair.offset, pair.lenght)) with
    //returns index relating to the original array
    | Some x when x = pair.offset ->
      if pair.lenght = 2 then 
        //there is only a '\r','\n' left
        connection.free_buffer pair.buffer
        return (count, None )
      else 
        return (count, mk_buffer_segment pair.buffer (pair.offset + 2) (pair.lenght - 2))
    | Some index ->
      select (ArraySegment(pair.buffer.Array, pair.offset, index - pair.offset)) (index - pair.offset)
      let number_of_bytes_to_select = pair.lenght - index + pair.offset - 2
      if number_of_bytes_to_select = 0 then
        connection.free_buffer pair.buffer
        return (count + (index - pair.offset ), None)
      else
        return (count + (index - pair.offset ), mk_buffer_segment pair.buffer (index  + 2) number_of_bytes_to_select)
    | None ->
      return! read_data count (Some pair)
    }
  and scan_data_x  count (left : BufferSegment) (right: BufferSegment) = async  {

    let cn = left.lenght
    let dn = right.lenght

    match scan_crlf_x (ArraySegment(left.buffer.Array, left.offset, cn)) (ArraySegment(right.buffer.Array, right.offset, dn)) with
    | Some index when index < cn ->
      select (ArraySegment(left.buffer.Array,left.offset,index)) index
      let number_of_bytes_to_select = cn - index - 2
      if number_of_bytes_to_select = 0 then
        connection.free_buffer left.buffer
        return (count + index, None) // asumes d is empty
      else 
        return (count + index, mk_buffer_segment left.buffer (left.offset + index + 2) number_of_bytes_to_select) // asumes d is empty
    | Some index -> 
      select (ArraySegment(left.buffer.Array, left.offset, left.lenght)) left.lenght // here we can free c's original buffer --
      connection.free_buffer  left.buffer
      select (ArraySegment(right.buffer.Array, right.offset, index - left.lenght)) (index - left.lenght)
      let number_of_bytes_to_select = dn - (index - cn) - 2
      if number_of_bytes_to_select = 0 then
        return (count + index, None )
      else 
        return (count + index, mk_buffer_segment right.buffer (right.offset + (index - cn) + 2) number_of_bytes_to_select)
    | None -> 
      select (ArraySegment(left.buffer.Array, left.offset, left.lenght)) left.lenght // free c's original buffer
      connection.free_buffer  left.buffer
      let count' = count + cn + dn - 2
      let number_of_bytes_to_select  = right.lenght - 1
      if number_of_bytes_to_select > 0 then
        select (ArraySegment(right.buffer.Array, right.offset, right.lenght-1)) number_of_bytes_to_select
        return! read_data (count' + number_of_bytes_to_select) (mk_buffer_segment right.buffer (right.offset + right.lenght - 1) 1)
      else
        return! read_data count' (Some right)
    }
  and read_data count (ahead :BufferSegment option)  = async {
    let buff = connection.get_buffer ()
    try
      let! b = connection.read buff 
      if b > 0 then 
        match ahead with 
        | Some data ->
          return! scan_data_x count data {buffer = buff; offset = buff.Offset ; lenght = b}
        | None ->
          return! scan_data count {buffer = buff; offset = buff.Offset ; lenght = b}
      else 
        return failwith "client closed"
    with ex ->
      connection.free_buffer buff
      return raise ex
    }
  match preread with 
  | Some data ->
    if data.lenght > 1 then scan_data 0  data 
    else read_data 0  preread
  | None ->
    read_data 0 preread

/// Read the stream until the marker appears.
let read_until (marker : byte array) (f : ArraySegment<_> -> int -> Async<unit>) (connection : Connection) (preread : BufferSegment option) =

  let marker_lenght = marker.Length

  let rec scan_data count  (segment : BufferSegment)  = async {
    match kmp_x marker (ArraySegment(segment.buffer.Array, segment.offset, segment.lenght))  with
    | Some 0 ->
      if segment.lenght = marker_lenght then 
        connection.free_buffer segment.buffer
        return (count, None )
      else
        return (count,  mk_buffer_segment segment.buffer (segment.offset + marker_lenght) (segment.lenght - marker_lenght))
    | Some index ->
      do! f (ArraySegment(segment.buffer.Array, segment.offset, index)) index
      //discard the marker
      if index = segment.lenght - marker_lenght then
        connection.free_buffer segment.buffer
        return (count, None )
      else
        return (count + index, mk_buffer_segment segment.buffer (segment.offset + index + marker_lenght) (segment.lenght - index - marker_lenght))
    | None -> 
      return! read_data count (Some segment)
    }
  and scan_data_x  count (left : BufferSegment) (right: BufferSegment) = async  {

    let cn = left.lenght
    let dn = right.lenght

    match kmp_x_x marker (ArraySegment(left.buffer.Array, left.offset,cn)) (ArraySegment(right.buffer.Array, right.offset,dn)) with
    | Some index when index < cn ->
      do! f (ArraySegment(left.buffer.Array, left.offset,index)) index
      let number_of_bytes_to_select = cn - index - marker_lenght
      if number_of_bytes_to_select = 0 then 
        connection.free_buffer left.buffer
        return (count + index, None)
      else
        return (count + index, mk_buffer_segment  left.buffer (left.offset + index + marker_lenght) number_of_bytes_to_select) // asumes d is empty
    | Some index -> 
      do! f (ArraySegment(left.buffer.Array, left.offset, left.lenght)) left.lenght//here we can free c's original buffer --
      connection.free_buffer  left.buffer
      do! f (ArraySegment(right.buffer.Array, right.offset, index)) index
      let number_of_bytes_to_select = dn - (index - cn) - marker_lenght
      if number_of_bytes_to_select = 0 then
        return (count + index, None)
      else
        return (count + index, mk_buffer_segment right.buffer (right.offset + (index - cn) + marker_lenght) number_of_bytes_to_select)
    | None -> 
      do! f  (ArraySegment(left.buffer.Array, left.offset, left.lenght)) left.lenght //free c's original buffer
      connection.free_buffer  left.buffer
      let number_of_bytes_to_select  = right.lenght - marker_lenght + 1
      if number_of_bytes_to_select > 0 then 
        do! f (ArraySegment(right.buffer.Array, right.offset, right.lenght-1)) number_of_bytes_to_select
        return! read_data (count + cn + number_of_bytes_to_select) (mk_buffer_segment right.buffer (right.offset + right.lenght - marker_lenght + 1) (marker_lenght - 1))
      else
        return! read_data (count + cn) (Some right)
    } 
  and read_data count (ahead : BufferSegment option) = async {

    let a = connection.get_buffer ()
    let! bytes_read = connection.read a
    if bytes_read > 0 then 
      match ahead with 
      | Some data ->
        return! scan_data_x count data {buffer = a; offset = a.Offset ; lenght = bytes_read}
      | None -> 
        return! scan_data count {buffer = a; offset = a.Offset ; lenght = bytes_read}
    else
      return failwith "client disconnected."
    }
  match preread with
  | Some data ->
    if data.lenght > marker.Length then scan_data 0 data
    else read_data 0 preread
  | None -> read_data 0 preread

/// Alternative read_till_EOL
let _read_till_EOL(connection : Connection) (buff : byte[]) (preread : BufferSegment option) =
  read_until EOL (fun b c -> async { do Array.blit b.Array b.Offset buff 0 c }) connection preread

/// Convert the byte array of ASCII-encoded chars to a string, starting at 'index' for 'count' characters
/// (each character is necessarily one byte)
let inline to_string (buff : byte[]) (index : int) (count : int) =
  Encoding.ASCII.GetString(buff, index, count)

/// Read a line from the stream, calling to_string on the bytes before the EOL marker
let read_line (connection:Connection) ahead (buf:ArraySegment<byte>) = async {
  let offset = ref 0
  let! count, rem = read_till_EOL connection (fun a count -> Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count; offset := !offset + count) ahead
  let result = to_string buf.Array buf.Offset count
  return result , rem
}

/// Read all headers from the stream, returning a dictionary of the headers found
let read_headers connection read (headers: Dictionary<string,string>) (buf:ArraySegment<byte>)  =
  let rec loop (rem : BufferSegment option) = async {

    let offset = ref 0
    let! count,new_rem = read_till_EOL connection (fun a count -> Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count; offset := !offset + count) rem
    if count <> 0 then
      let line = to_string buf.Array buf.Offset count
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
let inline parse_url (line : string) (dict : Dictionary<string,string>) =
  let parts = line.Split(' ')
  if parts.Length < 2 || parts.Length > 3 then failwith (sprintf "invalid url: '%s'" line)
  let indexOfMark = parts.[1].IndexOf('?')

  if indexOfMark > 0 then
    let raw_query = parts.[1].Substring(indexOfMark + 1)
    (parts.[0], parts.[1].Substring(0,indexOfMark), parse_data raw_query dict, "?" + raw_query, parts.[2])
  else
    (parts.[0], parts.[1], dict, String.Empty, parts.[2])

/// Read the post data from the stream, given the number of bytes that makes up the post data.
let read_post_data (connection : Connection) (bytes : int) (read : BufferSegment option) =
    let read_bytes bytes_needed  missing read_offset = async{
       let counter = ref 0
       let rem = ref None
       let a = connection.get_buffer ()
       while !counter < bytes_needed do
        let! bytes_transmited = connection.read a
        if bytes_transmited > bytes_needed - !counter 
        then
          Array.blit a.Array a.Offset missing (read_offset + !counter) (bytes_needed - !counter)
          rem := Some { buffer = a; offset =  a.Offset + bytes_needed - !counter; lenght = bytes_needed - !counter}
        else
          Array.blit a.Array a.Offset missing (read_offset + !counter) bytes_transmited
        counter := !counter + bytes_transmited
       return (ArraySegment missing, !rem)
    }
    async {
    match read with
    | Some segment ->
      if segment.lenght >= bytes then
        return (ArraySegment(segment.buffer.Array, segment.offset, bytes), Some { buffer = segment.buffer; offset = segment.offset + bytes; lenght = segment.lenght - bytes })
      else 
        let missing = Array.zeroCreate bytes
        Array.blit segment.buffer.Array segment.offset missing 0 segment.lenght
        return! read_bytes (bytes - segment.lenght) missing segment.offset
    | None ->
      let missing = Array.zeroCreate bytes
      return! read_bytes bytes missing 0
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
let parse_multipart (connection : Connection) boundary (request : HttpRequest) (ahead : BufferSegment option) line_buffer : Async<BufferSegment option> =
  let rec loop boundary read = async {

    let! firstline, read = read_line connection read line_buffer

    if not(firstline.Equals("--")) then

      let part_headers = new Dictionary<string,string>()
      let! rem = read_headers connection read part_headers line_buffer

      let content_disposition = look_up part_headers "content-disposition"

      let fieldname = (header_params content_disposition) ? name |> opt

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
      Log.trace (fun () -> "parse_content -> read_until")
      let! a, b = read_until (bytes(eol + boundary)) (fun x y -> async { do! temp_file.AsyncWrite(x.Array, x.Offset, y) } ) connection rem
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
      let! a, b = read_until (bytes(eol + boundary)) (fun x y -> async { do! mem.AsyncWrite(x.Array, x.Offset, y) } ) connection rem
      let byts = mem.ToArray()
      request.form.Add(fieldname, (to_string byts 0 byts.Length)) 

      return! loop boundary b
    }
  loop boundary ahead 

/// Process the request, reading as it goes from the incoming 'stream', yielding a HttpRequest
/// when done
let process_request proxy_mode (request : HttpRequest) (bytes : BufferSegment option) line_buffer : Async<(HttpRequest * BufferSegment option) option> = async {

  let connection = request.connection
  try
    Log.tracef(fun fmt -> fmt "web:process_request proxy:%b bytes:%A -> read_line" proxy_mode bytes)
    let! (first_line : string), rem = read_line connection bytes line_buffer
    Log.tracef(fun fmt -> fmt "web:process_request proxy:%b <- read_line" proxy_mode)
    
    if first_line.Length = 0 then
      return None
    else
      let meth, url, _, raw_query, http_version = parse_url first_line request.query

      request.url      <- url
      request.``method``   <- meth
      request.raw_query <- raw_query
      request.http_version <- http_version

      let headers = request.headers
      let! rem = read_headers connection rem headers line_buffer

      // won't continue parsing if on proxyMode with the intention of forwarding the stream as it is
      if proxy_mode then return Some (request, rem)
      else
        request.headers
        |> Seq.filter (fun x -> x.Key.Equals("cookie"))
        |> Seq.iter (fun x ->
                      let cookie = parse_cookie x.Value
                      request.cookies.Add (fst(cookie.[0]),cookie))

        // TODO: can't assume only POST can have form data, PUT can also be done
        // from forms
        if meth.Equals("POST") then

          let content_encoding =
            match headers.TryGetValue("content-type") with
            | true, encoding -> Some encoding
            | false, _ -> None

          let content_length = Convert.ToInt32(headers.["content-length"])

          match content_encoding with
          | Some ce when ce.StartsWith("application/x-www-form-urlencoded") ->
            let! (rawdata : ArraySegment<_>), rem = read_post_data connection content_length rem
            let str = to_string rawdata.Array rawdata.Offset rawdata.Count
            let _  = parse_data str request.form
            // TODO: can't we instead of copying, do an LMAX and use the buffer as a circular
            // queue?
            let raw_form = Array.zeroCreate rawdata.Count
            Array.blit rawdata.Array rawdata.Offset raw_form 0 rawdata.Count
            request.raw_form <- raw_form
            return Some (request, rem)
          | Some ce when ce.StartsWith("multipart/form-data") ->
            let boundary = "--" + ce.Substring(ce.IndexOf('=')+1).TrimStart()
            let! rem = parse_multipart connection boundary request rem line_buffer
            return Some (request, rem)
          | Some _ | None -> 
            let! (rawdata : ArraySegment<_>),_ = read_post_data connection content_length rem
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
let inline load_connection proto (connection : Connection) = async{
  match proto with
  | HTTP       -> return connection
  | HTTPS cert -> 
    let ssl = authenticate_as_server cert
    do! accept connection ssl
    return { connection with read = ssl_receive connection ssl; write = ssl_send connection ssl }
  }

open System.Net.Sockets

/// A HttpProcessor takes a HttpRequest instance, returning asynchronously a HttpRequest that has been parsed
type HttpProcessor = HttpRequest -> BufferSegment option -> ArraySegment<byte>-> Async<(HttpRequest * (BufferSegment option)) option>
type RequestResult = Done

/// The request loop initialises a request with a processor to handle the
/// incoming stream and possibly pass the request to the web parts, a protocol,
/// a web part, an error handler, a timeout value for executing the web part
/// in milliseconds and a Connection to use for read-write
/// communication -- getting the initial request stream.
let request_loop
  (processor        : HttpProcessor)
  (proto            : Protocol)
  (web_part         : WebPart)
  (web_part_timeout : TimeSpan)
  (error_handler    : ErrorHandler)
  (connection       : Connection) =

  /// Check if the web part can perform its work on the current request. If it can't
  /// it will return None and the run method will return.
  let run request = async {
    match web_part request with // run the web part
    | Some x -> do! x
    | None -> return ()
  }

  let free (s : BufferSegment option) = 
    match s with 
    | None -> ()
    | Some x -> connection.free_buffer x.buffer

  let exit rem msg = async {
    free rem
    Log.trace(fun () -> msg)
    return ()
    }
   

  let rec loop (bytes : BufferSegment option) request = async {
    Log.trace(fun () -> "web:request_loop:loop -> processor")
    let! result = processor request bytes request.line_buffer
    Log.trace(fun () -> "web:request_loop:loop <- processor")

    match result with
    | Some (request : HttpRequest, rem) ->
      try
        Log.trace(fun () -> "web:request_loop:loop -> unblock")
        do! Async.WithTimeout (web_part_timeout, run request)
        Log.trace(fun () -> "web:request_loop:loop <- unblock")
      with
        | InternalFailure(_) as ex  -> free rem; raise ex
        | :? TimeoutException as ex -> free rem; raise ex
        | :? SocketIssue as ex      -> free rem; raise ex
        | ex -> do! error_handler ex "Routing request failed" request
      if connection.is_connected () then
        match request.headers?connection with
        | Some (x : string) when x.ToLower().Equals("keep-alive") ->
          clear request
          Log.tracef(fun fmt -> fmt "web:request_loop:loop 'Connection: keep-alive' recurse (!), rem: %A" rem)
          return! loop rem request
        | Some _ ->
          free rem;
          Log.trace(fun () -> "web:request_loop:loop  'Connection: close', exiting")
          return ()
        | None ->
          if request.http_version.Equals("HTTP/1.1") then
            clear request
            Log.trace(fun () -> "web:request_loop:loop  'Connection: keep-alive' recurse (!)")
            return! loop rem request
          else
            free rem;
            Log.trace(fun () -> "web:request_loop:loop  'Connection: close', exiting")
            return ()
      else
        free rem;
        Log.trace(fun () -> "web:request_loop:loop 'is_connected = false', exiting")
        return ()
    | None ->
      Log.trace(fun () -> "web:request_loop:loop 'result = None', exiting")
      return ()
  }
  async {
    let! connection = load_connection proto connection
    let request =
      { connection     = connection
      ; http_version   = null
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
      ; remote_address = connection.ipaddr
      ; is_secure      = match proto with HTTP -> false | HTTPS _ -> true
      ; line_buffer = connection.get_buffer() }
    try
      try
        do! loop None request
      with
      | InternalFailure(_)
      | :? EndOfStreamException
      | :? IOException as ex
        when ex.InnerException <> null && ex.InnerException.GetType() = typeof<SocketException> ->
        Log.trace(fun () -> "web:request_loop - client disconnected")
        return ()
      | ex ->
        Log.tracef(fun fmt -> fmt "web:request_loop - Request failed.\n%A" ex)
    finally
      connection.free_buffer request.line_buffer
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
  if IPAddress.IsLoopback request.remote_address then
    do! (response 500 "Internal Error" (bytes_utf8 (sprintf "<h1>%s</h1><br/>%A" ex.Message ex)) request)
  else do! (response 500 "Internal Error" (bytes_utf8 ("Internal Error")) request)
}

/// Starts a new web worker, given the configuration and a web part to serve.
let web_worker (proto, ip, port, error_handler, timeout, buffer_size, max_ops) (webpart : WebPart) =
  tcp_ip_server (ip, port, buffer_size, max_ops) (request_loop (process_request false) proto webpart timeout error_handler)

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
        web_worker (proto, ip, port, config.error_handler, config.web_part_timeout, config.buffer_size, config.max_ops) webpart)
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
  { bindings         = [ { scheme = HTTP; ip = IPAddress.Loopback; port = 8083us } ]
  ; error_handler    = default_error_handler
  ; web_part_timeout = TimeSpan.FromMinutes(1.)
  ; listen_timeout   = TimeSpan.FromSeconds(2.)
  ; ct               = Async.DefaultCancellationToken
  ; buffer_size      = 8192 // 8 Kilobytes
  ; max_ops          = 100 }
