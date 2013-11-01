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

/// Chunk sizes

/// When parsing headers and lines we will prefer reading from the stream in small chunk sizes
let SHORT_BUFFER_SIZE = 512

/// When parsing POST data and file uploads we will read from the stream in larger chunk sizes
let BIG_BUFFER_SIZE = 1048576

/// Returns the index of the first CRLF in the buffer
let scan_crlf (b : byte[]) =
  let rec loop i =
    if i > b.Length - 1 then None
    elif i > 0 && b.[i - 1] = EOL.[0] && b.[i] = EOL.[1] then Some (i - 1)
    else loop (i + 1)
  loop 0

/// Read the passed stream into buff until the EOL (CRLF) has been reached 
/// and returns an array containing excess data read past the marker
let read_till_EOL (stream : Stream) (buff : byte[]) (preread : byte[]) chunk_size =

  let rec loop count (ahead : byte[]) = async {
    if ahead.Length > 0 then
      return! scan_data count ahead
    else
      return! read_data count ahead
    }

  and scan_data count (ahead : byte[]) = async {
    match scan_crlf ahead with
    | Some 0 ->
      return (0, Array.sub ahead 2 (ahead.Length - 2))
    | Some index ->
      Array.blit ahead 0 buff count (index)
      return (count + index, Array.sub ahead (index + 2) (ahead.Length - index - 2))
    | None ->
      return! read_data count ahead
    }

  and read_data count (ahead : byte[])  = async {

    let inp = Array.zeroCreate SHORT_BUFFER_SIZE

    let! bytesread = stream.AsyncRead inp
    if bytesread <> 0 then

      let sub = Array.sub inp 0 bytesread
      let inp = Array.append ahead sub

      return! scan_data count inp
    else return (0, ahead)
    }
  (loop 0 preread)

/// Read the stream until the marker appears.
let read_until (marker : byte array) (f : byte[] -> int -> Async<unit>) (stream : Stream) preread chunk_size =

  let rec loop count (ahead : byte[]) = async {
      if ahead.Length > 0 then return! scan_data count ahead
      else return! read_data count ahead
    }
  and scan_data count (data : byte[]) = async {
    match kmp marker data  with
    | Some 0 ->
      return (0, Array.sub data marker.Length (data.Length - marker.Length))
    | Some index ->
      do! f data index
      return (count + index, Array.sub data (index + marker.Length) (data.Length - index - marker.Length))
    | None -> return! read_data count data
    }
  and read_data count (ahead : byte[]) = async {
      let inp = Array.zeroCreate chunk_size

      let! bytesread = stream.AsyncRead inp
      if bytesread <> 0 then

        let sub = Array.sub inp 0 bytesread
        let inp = Array.append ahead sub

        return! scan_data count inp
      else return (0, ahead)
    }
  (loop 0 preread)

/// Alternative read_till_EOL
let _read_till_EOL (stream : Stream) (buff : byte[]) (preread : byte[]) =
  read_until EOL (fun b c -> async { do Array.blit b 0 buff 0 c }) stream preread

/// Convert the byte array of ASCII-encoded chars to a string, starting at 'index' for 'count' characters
/// (each character is necessarily one byte)
let to_string (buff : byte[]) (index : int) (count : int) =
  Encoding.ASCII.GetString(buff, index, count)

/// The maximun line size we will read from the stream
let max_line_size = 1024

/// Read a line from the stream, calling to_string on the bytes before the EOL marker
let read_line stream ahead = async {
  let buf = Array.zeroCreate max_line_size
  let! count, rem = read_till_EOL stream buf ahead SHORT_BUFFER_SIZE
  return (to_string buf 0 count, rem)
}

/// Read all headers from the stream, returning a dictionary of the headers found
let read_headers stream read =
  let headers = new Dictionary<string,string>()
  let rec loop (rem: byte[]) = async {
    let buf = Array.zeroCreate max_line_size
    let! count,new_rem = read_till_EOL stream buf rem SHORT_BUFFER_SIZE
    if count <> 0 then
      let line = to_string buf 0 count
      let indexOfColon = line.IndexOf(':')
      headers.Add (line.Substring(0,indexOfColon).ToLower(),line.Substring(indexOfColon+1).TrimStart())
      return! loop new_rem
    else return new_rem
  }
  async {
    let! rem = loop read
    return headers, rem
  }

open Suave.Types

/// Gets the empty query string dictionary
let empty_query_string () = new Dictionary<string,string>()

/// Gets the query from the HttpRequest // TODO: Move to a module for HttpRequest
let query (x : HttpRequest) = x.Query
/// Gets the form from the HttpRequest // TODO: Move to a module for HttpRequest
let form  (x : HttpRequest) = x.Form

/// Parse the data in the string to a dictionary, assuming k/v pairs are separated
/// by the ampersand character.
let parse_data (s : string) =
  let param = empty_query_string ()
  s.Split('&')
  |> Array.iter (fun (k : string) ->
       k.Split('=')
       |> (fun d -> if d.Length = 2 then param.Add(d.[0], System.Web.HttpUtility.UrlDecode(d.[1]))))
  param

/// TO BE DONE
let parse_url (line : string) =
  let parts = line.Split(' ')
  if parts.Length < 2 || parts.Length > 3 then failwith "400"
  let indexOfMark = parts.[1].IndexOf('?')

  if indexOfMark > 0 then
    let raw_query = parts.[1].Substring(indexOfMark + 1)
    (parts.[0], parts.[1].Substring(0,indexOfMark), parse_data raw_query, "?" + raw_query)
  else
    (parts.[0],parts.[1],empty_query_string (), String.Empty)

/// Read the post data from the stream, given the number of bytes that makes up the post data.
let read_post_data (stream : Stream) (bytes : int) (read : byte[]) = async {
    if read.Length >= bytes then
      return (Array.sub read 0 bytes, Array.sub read bytes (read.Length - bytes))
    else 
      let! missing = stream.AsyncRead(bytes - read.Length)
      return (Array.append read missing, Array.zeroCreate(0))
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
  | _ -> failwith "did not find header: %s." header

/// Parses multipart data from the stream, feeding it into the HttpRequest's property Files.
let parse_multipart (stream : Stream) boundary (request : HttpRequest) (ahead : byte[]) : Async<byte[]> =
  let rec loop boundary read = async {

    let! firstline, read = read_line stream read

    if not(firstline.Equals("--")) then

      let! part_headers,rem = read_headers stream read

      let content_disposition = look_up part_headers "content-disposition"

      let fieldname = (header_params content_disposition) ? name |> opt

      let content_type = look_up part_headers "content-type"

      return! parse_content content_type content_disposition fieldname rem
    else return Array.zeroCreate(0)
    }
  and parse_content content_type content_disposition fieldname rem = async {
    match content_type with
    | Some(x) when x.StartsWith("multipart/mixed") ->
      let subboundary = "--" + x.Substring(x.IndexOf('=') + 1).TrimStart()
      return! loop subboundary rem
    | Some(x) ->
      let temp_file_name = Path.GetTempFileName()
      use temp_file = new FileStream(temp_file_name,FileMode.Truncate)
      let! a,b = read_until (bytes(eol + boundary)) (fun x y -> async { do! temp_file.AsyncWrite(x,0,y) } ) stream rem BIG_BUFFER_SIZE
      let file_leght = temp_file.Length
      temp_file.Close()
      if  file_leght > int64(0) then
        let filename =
          (header_params content_disposition) ? filename |> opt
        request.Files.Add(new HttpUpload(fieldname,filename,content_type |> opt,temp_file_name))
      else
        File.Delete temp_file_name
      return! loop boundary b
    | None ->
      use mem = new MemoryStream()
      let! a,b = read_until (bytes(eol + boundary)) (fun x y -> async { do! mem.AsyncWrite(x,0,y) } ) stream rem BIG_BUFFER_SIZE
      let byts = mem.ToArray()
      request.Form.Add(fieldname, (to_string byts 0 byts.Length)) 

      return! loop boundary b
    }
  loop boundary ahead 

/// Process the request, reading as it goes from the incoming 'stream', yielding a HttpRequest
/// when done
let process_request proxyMode (request: HttpRequest) = async {

  let stream = request.Stream

  let! (first_line : string), rem = read_line stream Array.empty

  if first_line.Length = 0 then return None
  else
    let (meth, url, query, raw_query) as q = parse_url (first_line)
    request.Url      <- url
    request.Method   <- meth
    request.Query    <- query
    request.RawQuery <- raw_query

    let! headers,rem = read_headers stream rem
    request.Headers <- headers

    //wont continue parsing if on proxyMode with the intention of forwarding the stream as it is
    if not proxyMode then
      request.Headers
      |> Seq.filter (fun x -> x.Key.Equals("cookie"))
      |> Seq.iter (fun x ->
                    let cookie = parse_cookie x.Value
                    request.Cookies.Add (fst(cookie.[0]),cookie))

      if meth.Equals("POST") then

        let content_enconding = headers.["content-type"]
        let content_length = Convert.ToInt32(headers.["content-length"])

        if content_enconding.StartsWith("application/x-www-form-urlencoded") then
          let! rawdata,_ = read_post_data stream content_length rem
          let form_data = parse_data (to_string rawdata 0 rawdata.Length)
          request.Form    <- form_data
          request.RawForm <- rawdata
        elif content_enconding.StartsWith("multipart/form-data") then
          let boundary = "--" + content_enconding.Substring(content_enconding.IndexOf('=')+1).TrimStart()
          let! (rem : byte[]) = parse_multipart stream boundary request rem
          assert (rem.Length = 0)
          return ()

    return Some request
}

open System.Net
open OpenSSL.SSL
open OpenSSL.X509

open Types

/// Load a readable plain-text stream, based on the protocol in use. If plain HTTP
/// is being used, the stream is returned as it, otherwise a new SslStream is created
/// to decipher the stream, without client certificates.
let load_stream proto (stream : Stream) =
  match proto with
  | HTTP       -> stream
  | HTTPS cert ->
    let sslStream = new SslStream(stream, true)
    sslStream.AuthenticateAsServer(cert)
    sslStream :> Stream

open System.Net.Sockets

/// A HttpProcessor takes a HttpRequest instance, returning asynchronously a HttpRequest that has been parsed
type HttpProcessor = HttpRequest -> Async<HttpRequest option>
type RequestResult = Done

/// The request loop initialises a request against a web part, with a protocol, a processor to handle the
/// incoming stream, an error handler, a timeout value (milliseconds) and a TcpClient to use for read-write
/// communication -- getting the initial request stream.
let request_loop webpart proto (processor : HttpProcessor) error_handler (timeout : TimeSpan) (client : TcpClient) =
  /// Evaluate the (web part) action as an async value, handling errors with error_handler should one
  /// occur.
  let eval_action x r = async {
    try
      do! x
    with
    | InternalFailure(_) as ex -> raise ex
    | ex -> do! error_handler ex "Action failed" r
  }
  /// Check if the web part can perform its work on the current request. If it can't
  /// it will return None and the run method will return.
  let run request = async {
    match webpart request with // routing
    | Some x -> do! eval_action x request
    | None -> return ()
  }
  let stream =
        client.GetStream()
        |> load_stream proto
  let remote_endpoint = (client.Client.RemoteEndPoint :?> IPEndPoint)
  let ipaddr = remote_endpoint.Address.ToString()
  

  let rec loop _ = async {
  
    use request = new HttpRequest()
    request.Stream <- stream
    request.RemoteAddress <- ipaddr
    request.IsSecure <- match proto with HTTP -> false | HTTPS _ -> true
    
    let! result = processor request
    match result with
    | Some (request : HttpRequest) ->
      try
        do! unblock (fun _ -> Async.RunSynchronously(run request, int (timeout.TotalMilliseconds)))
        do! stream.FlushAsync()
      with
        | InternalFailure(_) as ex  -> raise ex
        | :? TimeoutException as ex -> do! error_handler ex "script timeout" request
        | ex -> do! error_handler ex "Routing request failed" request
      match request.Headers?connection with
      | Some (x : string)  when x.ToLower().Equals("keep-alive") -> return! loop ()
      | _ -> return ()
    | None -> return ()
  }
  async {
    try
      do! loop ()
    with
    | InternalFailure(_)
    | :? EndOfStreamException
    | :? IOException as ex
      when ex.InnerException.GetType() = typeof<SocketException> ->
      Log.log "Client disconnected.\n"
    | ex -> Log.log "Request failed.\n%A" ex
  }

/// Parallelise the map of 'f' onto all items in the 'input' seq.
let parallelize input f = input |> Seq.map f |> Async.Parallel

open Suave.Tcp

/// Gets whether the passed ip is a local ip address
let is_local_address (ip : string) =
  // TODO: equal on IP address
  // TODO: equal IPv6 too
  ip.Equals("127.0.0.1")

/// The default error handler returns a 500 Internal Error in response to
/// thrown exceptions.
let default_error_handler (ex : Exception) msg (request : HttpRequest) = async {
  Log.log "%s.\n%A" msg ex
  if is_local_address request.RemoteAddress then
    do! (response 500 "Internal Error" (bytes_utf8 (sprintf "<h1>%s</h1><br/>%A" ex.Message ex)) request)
  else do! (response 500 "Internal Error" (bytes_utf8 (request.RemoteAddress)) request)
}

/// Starts a new web worker, given the configuration and a web part to serve.
let web_worker (proto, ip, port, error_handler, timeout) (webpart : WebPart) =
  tcp_ip_server (ip,port) (request_loop webpart proto (process_request false) error_handler timeout)

/// Returns the webserver as an asynchronous computation
let web_server_async (config : Config) (webpart : WebPart) =
  config.bindings
  |> List.map (fun { scheme = proto; ip = ip; port = port } ->
      web_worker (proto, ip, port, config.error_handler, config.timeout) webpart)
  |> Async.Parallel
  |> Async.Ignore

/// Runs the web server and blocks waiting for the asynchronous workflow to be cancelled or
/// it returning itself.
let web_server (config : Config) (webpart : WebPart) =
  Async.RunSynchronously(web_server_async config webpart, cancellationToken = config.ct)

/// The default configuration binds on IPv4, 127.0.0.1:8083 with a regular 500 Internal Error handler,
/// with a timeout of one minute for computations to run.
let default_config =
  { bindings      = [ { scheme = HTTP; ip = IPAddress.Parse("127.0.0.1"); port = 8083us } ]
  ; error_handler = default_error_handler
  ; timeout       = TimeSpan.FromMinutes(1.) // 1 minute
  ; ct            = Async.DefaultCancellationToken } 
