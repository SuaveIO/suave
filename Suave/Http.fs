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

  let response_f (status_code: int) reason_phrase (f_content : HttpRequest -> Async<unit>) ({request = request; runtime = runtime; connection = connection } as context : HttpContext) = async {
    try
      //let connection:Connection = runtime.connection
      do! async_writeln connection (String.concat " " [ HTTP_VERSION ; status_code.ToString() ; reason_phrase])
      do! async_writeln connection server_header
      do! async_writeln connection (String.Concat( [|  "Date: "; DateTime.UtcNow.ToString("R") |]))

      for (x,y) in request.response.Headers do
        if not (List.exists (fun y -> x.ToLower().Equals(y)) ["server";"date";"content-length"]) then
          do! async_writeln connection (String.Concat [| x; ": "; y |])

      if not(request.response.Headers.Exists(new Predicate<_>(fun (x,_) -> x.ToLower().Equals("content-type")))) then
        do! async_writeln connection "Content-Type: text/html"

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

  let MIN_BYTES_TO_COMPRESS =       500 // 500 bytes
  let MAX_BYTES_TO_COMPRESS = 524288000 // 500 megabytes

  let transform (content : byte []) (ctx : HttpContext) : Async<byte []> =
    async {
      if content.Length > MIN_BYTES_TO_COMPRESS && content.Length < MAX_BYTES_TO_COMPRESS then
        let request = ctx.request
        let enconding = get_encoder request 
        match enconding with
        | Some (n,encoder) ->
          do! async_writeln ctx.connection (String.Concat [| "Content-Encoding: "; n |])
          return encoder content
        | None ->
          return content
      else
        return content
    }

  let response status_code reason_phrase (cnt : byte []) ({request = request; runtime = runtime; connection = connection } as context : HttpContext) =
    response_f status_code reason_phrase (
      fun r -> async {

        let! (content : byte []) = transform cnt context

        // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
        do! async_writeln connection (String.Concat [|  "Content-Length: "; content.Length.ToString() |])

        do! async_writeln connection ""

        if content.Length > 0 then
          do! connection.write (new ArraySegment<_>(content, 0, content.Length)) })
      context

  // modifiers

  let set_header key value ({request = http_request; runtime = _ } as ctx : HttpContext) =
    http_request.response.Headers.Add(key, value)
    ctx

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

  let url s (x : HttpContext) = if s = x.request.url then Some x else None

  let meth0d s (x : HttpContext) = if s = x.request.``method`` then Some x else None

  let is_secure (x : HttpContext) = if x.request.is_secure then Some x else None

  let url_regex s (x : HttpContext) = if Regex.IsMatch(x.request.url,s) then Some x else None

  // see http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html
  // see Http.fsi for documentation

  let GET     (x : HttpContext) = meth0d "GET" x
  let POST    (x : HttpContext) = meth0d "POST" x
  let DELETE  (x : HttpContext) = meth0d "DELETE" x
  let PUT     (x : HttpContext) = meth0d "PUT" x
  let HEAD    (x : HttpContext) = meth0d "HEAD" x
  let CONNECT (x : HttpContext) = meth0d "CONNECT" x
  let PATCH   (x : HttpContext) = meth0d "PATCH" x
  let TRACE   (x : HttpContext) = meth0d "TRACE" x
  let OPTIONS (x : HttpContext) = meth0d "OPTIONS" x

  // TODO: let continue ... ?
  // TODO: let switching_protocols ... ?

  // also see: http://www.vinaysahni.com/best-practices-for-a-pragmatic-restful-api

  let ok s = response 200 "OK" s >> succeed

  let OK a = ok (bytes_utf8 a)

  let created s = response 201 "Created" s >> succeed

  let CREATED s = created (bytes_utf8 s)

  let accepted s = response 202 "Accepted" s >> succeed

  let ACCEPTED s = accepted (bytes_utf8 s)

  let no_content : WebPart =
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

  let not_modified : WebPart =
    response 304 "Not Modified" (Array.zeroCreate 0) >> succeed

  let NOT_MODIFIED : WebPart =
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

  let compress encoding path (fs : FileStream) = async {
    use new_fs = new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.Write)
    if encoding = "gzip" then
      use gzip = new GZipStream(new_fs, CompressionMode.Compress)
      do! fs.CopyToAsync gzip
      gzip.Close()
    elif encoding = "deflate" then
      use deflate = new DeflateStream(new_fs, CompressionMode.Compress)
      do! fs.CopyToAsync deflate
      deflate.Close()
    else
      return failwith "invalid case."
    new_fs.Close()
  }

  let transform_x (path : string) compression compression_folder ({ request = q; runtime = r; connection = connection } as ctx) : Async<FileStream> = 
    let fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read)
    let compress_file n = async {
      let temp_file_name = Path.GetRandomFileName()
      if not (Directory.Exists compression_folder) then Directory.CreateDirectory compression_folder |> ignore
      let new_path = Path.Combine(compression_folder,temp_file_name)
      do! compress n new_path fs
      return new_path
    }
    async {
      if compression && fs.Length > int64(MIN_BYTES_TO_COMPRESS) && fs.Length < int64(MAX_BYTES_TO_COMPRESS) then
        let enconding = parse_encoder q 
        match enconding with
        | Some (n) ->
          do! async_writeln connection (String.Concat [| "Content-Encoding: "; n |])
          if compressed_files_map.ContainsKey path then
            let file_info = new FileInfo(path)
            let cmpr_info = new FileInfo(compressed_files_map.[path])
            if file_info.LastWriteTime > cmpr_info.CreationTime then
              let! new_path =  compress_file n
              compressed_files_map.[path] <- new_path
          else
            let! new_path =  compress_file n
            compressed_files_map.TryAdd(path,new_path) |> ignore
          return new FileStream(compressed_files_map.[path] , FileMode.Open, FileAccess.Read, FileShare.Read)
        | None ->
          return fs
      else
        return fs
   }

  let send_file filename (compression : bool) (r : HttpContext) =
    let write_file file (q : HttpRequest) = async {
      
      let connection = r.connection
      let runtime    = r.runtime

      use! fs = transform_x file compression runtime.compression_folder r

      do! async_writeln connection (sprintf "Content-Length: %d" (fs : FileStream).Length)
      do! async_writeln connection ""

      if fs.Length > 0L then
        do! transfer_x connection fs }

    async { do! response_f 200 "OK" (write_file filename) r } |> succeed

  // If a response includes both an Expires header and a max-age directive,
  // the max-age directive overrides the Expires header, even if the Expires header is more restrictive 
  // 'Cache-Control' and 'Expires' headers should be left up to the user

  let file filename =
    fun ({request = r; runtime = q} as ctx) ->
      if File.Exists filename then
        let file_info = new FileInfo(filename)
        let mimes = q.mime_types_map (file_info.Extension)
        match mimes with
        | Some value ->
          let send_it _ = 
            set_header "Last-Modified" (file_info.LastAccessTimeUtc.ToString("R"))
            >> set_mime_type value.name 
            >> send_file filename value.compression

          let modified_since = (r.headers ? ``if-modified-since`` )
          match modified_since with
          | Some v -> let date = DateTime.Parse v
                      if file_info.LastWriteTime > date then send_it () ctx
                      else NOT_MODIFIED ctx
          | None   -> send_it () ctx
        | None -> None
      else
        None

  let local_file (fileName : string) (root_path : string) =
    let fileName = 
      if Path.DirectorySeparatorChar.Equals('/') then fileName
      else fileName.Replace('/', Path.DirectorySeparatorChar)
    let calculated_path = Path.Combine(root_path, fileName.TrimStart([| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |]))
    if calculated_path = Path.GetFullPath(calculated_path) then
      if calculated_path.StartsWith root_path then
        calculated_path
      else raise <| Exception("File canonalization issue.")
    else raise <| Exception("File canonalization issue.")

  let browse_file filename = 
    fun ({request = r; runtime = q} as h) -> file (local_file filename q.home_directory) h

  let browse : WebPart = warbler (fun {request = r; runtime = q } -> file (local_file r.url q.home_directory))

  type WebResult = Option<Async<unit>>

  let dir (ctx : HttpContext) : WebResult =

    let req = ctx.request
    let runtime = ctx.runtime

    let url = req.url

    let dirname = local_file url runtime.home_directory
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
      ok (bytes (result.ToString())) ctx
    else fail

  let parse_authentication_token (token : string) =
    let parts = token.Split (' ')
    let enc = parts.[1].Trim()
    let decoded = decode_base64 enc
    let indexOfColon = decoded.IndexOf(':')
    (parts.[0].ToLower(), decoded.Substring(0,indexOfColon), decoded.Substring(indexOfColon+1))

  let authenticate_basic f (ctx : HttpContext) =
    let p = ctx.request
    let headers = p.headers
    if headers.ContainsKey("authorization") then
      let header = headers.["authorization"]
      let (typ,username,password) = parse_authentication_token header
      p.user_name <- username
      p.password <- password
      if (typ.Equals("basic")) && f p then
        fail
      else
        challenge ctx
    else
      challenge ctx

  let log_format (http_request : HttpRequest) =
    sprintf "%A\n" (http_request.``method``, http_request.remote_address, http_request.url, http_request.query, http_request.form, http_request.headers)

  let log (s : Stream) (ctx : HttpContext) =
    let http_request = ctx.request
    let bytes = bytes (log_format http_request)
    s.Write(bytes, 0, bytes.Length)
    succeed ctx

  open Suave.Sscanf

  let url_scan (pf : PrintfFormat<_,_,_,_,'t>) (h : 't ->  WebPart) : WebPart =
    let t url = sscanf pf url

    let F (r:HttpContext) =
      try
        let y = r.request.url |> t |> h
        try y r with ex -> r |> INTERNAL_ERROR (ex.ToString())
      with _ -> fail
    F
