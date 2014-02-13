namespace Suave
module Http =

  open System
  open System.IO
  open System.Text
  open System.Text.RegularExpressions
  open System.Threading.Tasks

  open Utils
  open Types
  open Socket

  // literals

  let SUAVE_VERSION = Reflection.Assembly.GetExecutingAssembly().GetName().Version.ToString()

  let [<Literal>] HTTP_VERSION = "HTTP/1.1"

  /// Server header
  let server_header = String.Concat [| "Server: Suave/"; SUAVE_VERSION; " (http://suave.io)" |]

  // general response functions

  let response_f (status_code: int) reason_phrase (f_content : HttpRequest -> Async<unit>) (request : HttpRequest) = async {
    try
      let connection:Connection = request.connection
      do! async_writeln connection (String.concat " " [ HTTP_VERSION ; status_code.ToString() ; reason_phrase]) request.line_buffer
      do! async_writeln connection server_header request.line_buffer
      do! async_writeln connection (String.Concat( [|  "Date: "; DateTime.UtcNow.ToString("R") |])) request.line_buffer

      for (x,y) in request.response.Headers do
        if not (List.exists (fun y -> x.ToLower().Equals(y)) ["server";"date";"content-length"]) then
          do! async_writeln connection (String.Concat [| x; ": "; y |]) request.line_buffer

      if not(request.response.Headers.Exists(new Predicate<_>(fun (x,_) -> x.ToLower().Equals("content-type")))) then
        do! async_writeln connection "Content-Type: text/html" request.line_buffer

      do! f_content request

    with //the connection might drop while we are sending the response
    | :? ObjectDisposedException -> () // this happens without it being a problem
    | :? IOException as ex  -> raise (InternalFailure "Failure while writing to client stream")
    }

  let load_encoder s =
    match s with
    | "gzip"    -> Some (s, gzip_encode)
    | "deflate" -> Some (s, deflate_encode)
    | _         -> None

  let get_encoder request =
     let encondings = request.headers?``accept-encoding``
     match encondings with
     | Some (value : string) -> 
       value.Split ',' 
       |> Array.map (fun s -> s.Trim())
       |> Array.tryPick (fun s -> load_encoder s)
     | _ -> None

  let parse_encoder request =
    let encondings = request.headers?``accept-encoding``
    match encondings with
    | Some (value : string) -> 
      value.Split ','
      |> Array.map (fun s -> s.Trim())
      |> Array.tryPick 
        (fun s -> 
          match s with
          | "gzip"    -> Some (s)
          | "deflate" -> Some (s)
          | _         -> None)
    | _ -> None
  
  // You should only gzip files above a certain size threshold; we recommend a minimum range
  // between 150 and 1000 bytes. Gzipping files below 150 bytes can actually make them larger
  let MIN_BYTES_TO_COMPRESS =   500
  let MAX_BYTES_TO_COMPRESS = 10000

  let transform (content : byte []) (request : HttpRequest) : Async<byte []> =
    async {
      if content.Length > MIN_BYTES_TO_COMPRESS && content.Length < MAX_BYTES_TO_COMPRESS then
        let enconding = get_encoder request 
        match enconding with
        | Some (n,encoder) ->
          do! async_writeln request.connection (String.Concat [| "Content-Encoding: "; n |]) request.line_buffer
          return encoder content
        | None ->
          return content
      else
        return content
    }

  let response status_code reason_phrase (cnt : byte []) (request : HttpRequest) =
    response_f status_code reason_phrase (
      fun r -> async {

        let! (content : byte []) = transform cnt request

        // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
        do! async_writeln r.connection (String.Concat [|  "Content-Length: "; content.Length.ToString() |]) request.line_buffer

        do! async_writeln r.connection "" request.line_buffer

        if content.Length > 0 then
          do! r.connection.write (new ArraySegment<_>(content, 0, content.Length)) })
      request

  // modifiers

  let set_header key value (http_request : HttpRequest) =
    http_request.response.Headers.Add(key, value)
    http_request

  let cookie_to_string (x : HttpCookie) = 
    let attributes = new System.Collections.Generic.List<string>()
    attributes.Add(String.Format("{0}={1}", x.name ,x.value))
    match x.domain  with | Some n -> attributes.Add(String.Format("Domain={0}", n))  | _ -> ()
    match x.path    with | Some n -> attributes.Add(String.Format("Path={0}", n))    | _ -> ()
    match x.expires with | Some n -> attributes.Add(String.Format("Expires={0}", n.ToString("R"))) | _ -> ()
    if x.http_only then attributes.Add(String.Format("HttpOnly"))
    if x.secure    then attributes.Add(String.Format("Secure"))
    String.concat "; " attributes

  let set_cookie (cookie : HttpCookie) = set_header "Set-Cookie" (cookie_to_string cookie)

  // filters/applicatives

  let url s (x : HttpRequest) = if s = x.url then Some x else None

  let meth0d s (x : HttpRequest) = if s = x.``method`` then Some x else None

  let is_secure (x : HttpRequest) = if x.is_secure then Some x else None

  let url_regex s (x : HttpRequest) = if Regex.IsMatch(x.url,s) then Some x else None

  // see http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html
  // see Http.fsi for documentation

  let GET     (x : HttpRequest) = meth0d "GET" x
  let POST    (x : HttpRequest) = meth0d "POST" x
  let DELETE  (x : HttpRequest) = meth0d "DELETE" x
  let PUT     (x : HttpRequest) = meth0d "PUT" x
  let HEAD    (x : HttpRequest) = meth0d "HEAD" x
  let CONNECT (x : HttpRequest) = meth0d "CONNECT" x
  let PATCH   (x : HttpRequest) = meth0d "PATCH" x
  let TRACE   (x : HttpRequest) = meth0d "TRACE" x
  let OPTIONS (x : HttpRequest) = meth0d "OPTIONS" x

  // TODO: let continue ... ?
  // TODO: let switching_protocols ... ?

  // also see: http://www.vinaysahni.com/best-practices-for-a-pragmatic-restful-api

  let ok s = response 200 "OK" s >> succeed

  let OK a = ok (bytes_utf8 a)

  let created s = response 201 "Created" s >> succeed

  let CREATED s = created (bytes_utf8 s)

  let accepted s = response 202 "Accepted" s >> succeed

  let ACCEPTED s = accepted (bytes_utf8 s)

  let no_content : HttpRequest -> Async<unit> option =
    response 204 "No Content" (Array.zeroCreate 0) >> succeed

  let NO_CONTENT = no_content

  // 3xx Redirects

  let moved_permanently location =
    set_header "Location" location
    >> response 301 "Moved Permanently" (Array.zeroCreate 0)
    >> succeed

  let MOVED_PERMANENTLY location = moved_permanently location

  let found location =
    set_header "Location" location
    >> response 302 "Found" (Array.zeroCreate 0)
    >> succeed

  let FOUND location = found location

  let redirect url =
    set_header "Location" url
    >> set_header "Content-Type" "text/html; charset=utf-8"
    >> response 302 "Found" (bytes_utf8(sprintf "<html>
  <body>
    <a href=\"%s\">Content Moved</a>
  </body>
</html>
" url))
    >> succeed

  let not_modified : HttpRequest -> Async<unit> option =
    response 304 "Not Modified" (Array.zeroCreate 0) >> succeed

  let NOT_MODIFIED : HttpRequest -> Async<unit> option =
    not_modified

  let bad_request s = response 400 "Bad Request" s >> succeed

  let BAD_REQUEST s = bad_request (bytes_utf8 s)

  // 401: see http://stackoverflow.com/questions/3297048/403-forbidden-vs-401-unauthorized-http-responses/12675357

  let unauthorized s =
    set_header "WWW-Authenticate" "Basic realm=\"protected\""
    >> response 401 "Unauthorized" s
    >> succeed

  let UNAUTHORIZED s = unauthorized (bytes_utf8 s)

  let challenge = UNAUTHORIZED "401 Unauthorized."

  let forbidden s = response 403 "Forbidden" s >> succeed

  let FORBIDDEN s = forbidden (bytes_utf8 s)

  let not_found s = response 404 "Not Found" s >> succeed

  let NOT_FOUND message = not_found (bytes_utf8 message)

  let method_not_allowed s = response 405 "Method Not Allowed" s >> succeed

  let METHOD_NOT_ALLOWED s = method_not_allowed (bytes_utf8 s)

  let not_acceptable s = response 406 "Not Acceptable" s >> succeed

  let NOT_ACCEPTABLE message = not_acceptable (bytes_utf8 message)

  let request_timeout = response 408 "Request Timeout" (Array.zeroCreate 0) >> succeed
  // all-caps req.timeout elided intentionally

  let conflict s = response 409 "Conflict" s >> succeed

  let CONFLICT message = conflict (bytes_utf8 message)

  let gone s = response 410 "Gone" s >> succeed

  let GONE s = gone (bytes_utf8 s)

  let unsupported_media_type s = response 415 "Unsupported Media Type" s >> succeed

  let UNSUPPORTED_MEDIA_TYPE s = unsupported_media_type (bytes_utf8 s)

  let unprocessable_entity s = response 422 "Unprocessable Entity" s >> succeed

  let UNPROCESSABLE_ENTITY s = unprocessable_entity (bytes_utf8 s)

  let precondition_required body = response 428 "Precondition Required" body >> succeed

  let PRECONDITION_REQUIRED body = precondition_required (bytes_utf8 body)

  let too_many_requests s = response 429 "Too Many Requests" s >> succeed

  let TOO_MANY_REQUESTS s = too_many_requests (bytes_utf8 s)

  let internal_error message = response 500 "Internal Error" message >> succeed

  let INTERNAL_ERROR a = internal_error (bytes_utf8 a)

  let mk_mime_type a b = 
    { name = a
    ; compression  = b } |> Some

  let default_mime_types_map = function
    | ".bmp" -> mk_mime_type "image/bmp" false
    | ".css" -> mk_mime_type "text/css" true
    | ".gif" -> mk_mime_type "image/gif" false
    | ".png" -> mk_mime_type "image/png" false
    | ".ico" -> mk_mime_type "image/x-icon" false
    | ".htm"
    | ".html" -> mk_mime_type "text/html" true
    | ".jpe"
    | ".jpeg"
    | ".jpg" -> mk_mime_type "image/jpeg" false
    | ".js"  -> mk_mime_type "application/x-javascript" true
    | ".exe" -> mk_mime_type "application/exe" false
    | ".txt" -> mk_mime_type "text/plain" true
    | _      -> None

  let set_mime_type t = set_header "Content-Type" t

  open System.IO.Compression
  open System.Collections.Concurrent

  let compressed_files_map = new ConcurrentDictionary<string,string>()

  let compression_folder = "_temporary_compressed_files"

  let compress n path (fs : FileStream) = async {
    use new_fs = new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.Write)
    if n = "gzip" then
      use gzip = new GZipStream(new_fs, CompressionMode.Compress)
      do! fs.CopyToAsync gzip
      gzip.Close()
    elif n = "deflate" then
      use deflate = new DeflateStream(new_fs, CompressionMode.Compress)
      do! fs.CopyToAsync deflate
      deflate.Close()
    else
      return failwith "invalid case."
    new_fs.Close()
  }

  let transform_x (filename : string) compression r : Async<FileStream> = async {
    let fs = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
    if compression && fs.Length > int64(MIN_BYTES_TO_COMPRESS) && fs.Length < int64(MAX_BYTES_TO_COMPRESS) then
      let enconding = parse_encoder r 
      match enconding with
      | Some (n) ->
        do! async_writeln r.connection (String.Concat [| "Content-Encoding: "; n |]) r.line_buffer
        if not (compressed_files_map.ContainsKey filename) then
          let temp_file_name = Path.GetRandomFileName()
          if not (Directory.Exists compression_folder) then Directory.CreateDirectory compression_folder |> ignore
          let new_path = Path.Combine(compression_folder,temp_file_name)
          do! compress n new_path fs
          compressed_files_map.TryAdd(filename,new_path) |> ignore
        return new FileStream(compressed_files_map.[filename] ,FileMode.Open, FileAccess.Read, FileShare.Read)
      | None ->
        return fs
    else
      return fs
   }

  let send_file filename (compression : bool) r =
    let write_file file (r : HttpRequest) = async {

      use! fs = transform_x file compression r

      do! async_writeln r.connection (sprintf "Content-Length: %d" (fs : FileStream).Length) r.line_buffer
      do! async_writeln r.connection "" r.line_buffer

      if fs.Length > 0L then
        do! transfer_x r.connection fs }

    async { do! response_f 200 "OK" (write_file filename) r } |> succeed

  // If a response includes both an Expires header and a max-age directive,
  // the max-age directive overrides the Expires header, even if the Expires header is more restrictive 
  // 'Cache-Control' and 'Expires' headers should be left up to the user

  let file filename =
    fun (r : HttpRequest) ->
      if File.Exists filename then
        let file_info = new FileInfo(filename)
        let mimes = r.mime_types (file_info.Extension)
        match mimes with
        | Some value ->

          let send_it _ = 
            set_header "Last-Modified" (file_info.LastAccessTimeUtc.ToString("R"))
            >> set_mime_type value.name 
            >> send_file filename value.compression

          
          let modified_since = (r.headers ? ``if-modified-since`` )
          match modified_since with
          | Some v -> let date = DateTime.Parse v
                      if file_info.LastWriteTime > date then send_it () r
                      else NOT_MODIFIED r
          | None   -> send_it () r

        | None -> None
      else
        None

  // BUG: Concatenating strings here is a security risk; we need to make sure we don't serve unintended files
  let local_file fileName = sprintf "%s%s" Environment.CurrentDirectory fileName

  let browse_file filename = file (local_file filename)

  let browse : WebPart = warbler (fun req -> file (local_file req.url))

  type WebResult = Option<Async<unit>>

  let dir (req : HttpRequest) : WebResult =

    let url = req.url

    let dirname = local_file url
    let result = new StringBuilder()

    let filesize  (x : FileSystemInfo) =
      if (x.Attributes ||| FileAttributes.Directory = FileAttributes.Directory) then
        String.Format("{0,-14}",System.Web.HttpUtility.HtmlEncode("<DIR>"))
      else
        String.Format("{0,14}", (new FileInfo(x.FullName)).Length)

    let formatdate (t:DateTime) =
      t.ToString("MM-dd-yy") + "  " + t.ToString("hh:mmtt")

    let buildLine (x : FileSystemInfo) =
      result.Append(x.LastWriteTime.ToString() + "       " + filesize(x) + " " + x.Name + "<br/>\n")
      |> ignore

    if Directory.Exists dirname then
      let di = new DirectoryInfo(dirname)
      (di.GetFileSystemInfos()) |> Array.sortBy (fun x -> x.Name) |> Array.iter buildLine
      ok (bytes (result.ToString())) req
    else fail

  let parse_authentication_token (token : string) =
    let parts = token.Split (' ')
    let enc = parts.[1].Trim()
    let decoded = decode_base64 enc
    let indexOfColon = decoded.IndexOf(':')
    (parts.[0].ToLower(), decoded.Substring(0,indexOfColon), decoded.Substring(indexOfColon+1))

  let authenticate_basic f (p : HttpRequest) =
    let headers = p.headers
    if headers.ContainsKey("authorization") then
      let header = headers.["authorization"]
      let (typ,username,password) = parse_authentication_token header
      p.user_name <- username
      p.password <- password
      if (typ.Equals("basic")) && f p then
        fail
      else
        challenge p
    else
      challenge p

  let log_format (http_request : HttpRequest) =
    sprintf "%A\n" (http_request.``method``, http_request.remote_address, http_request.url, http_request.query, http_request.form, http_request.headers)

  let log (s : Stream) (http_request : HttpRequest) =
    let bytes = bytes (log_format http_request)
    s.Write(bytes, 0, bytes.Length)
    succeed http_request

  open Suave.Sscanf

  let url_scan (pf : PrintfFormat<_,_,_,_,'t>) (h : 't ->  WebPart) : WebPart =
    let t url = sscanf pf url

    let F (r:HttpRequest) =
      try
        let y = r.url |> t |> h
        try y r with ex -> r |> INTERNAL_ERROR (ex.ToString())
      with _ -> fail
    F
