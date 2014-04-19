namespace Suave

module Http =

  open Types

  type WebResult = Async<unit> option

  type WebPart = HttpContext -> WebResult

  type HttpMethod =
    | GET
    | POST
    | DELETE
    | PUT
    | HEAD
    | CONNECT
    | PATCH
    | TRACE
    | OPTIONS
    override x.ToString() =
      match x with
      | GET     -> "GET"
      | POST    -> "POST"
      | DELETE  -> "DELETE"
      | PUT     -> "PUT"
      | HEAD    -> "HEAD"
      | CONNECT -> "CONNECT"
      | PATCH   -> "PATCH"
      | TRACE   -> "TRACE"
      | OPTIONS -> "OPTIONS"

  type HttpCode =
    | HTTP_100 | HTTP_101
    | HTTP_200 | HTTP_201 | HTTP_202 | HTTP_203 | HTTP_204 | HTTP_205 | HTTP_206
    | HTTP_300 | HTTP_301 | HTTP_302 | HTTP_303 | HTTP_304 | HTTP_305 | HTTP_307
    | HTTP_400 | HTTP_401 | HTTP_402 | HTTP_403 | HTTP_404 | HTTP_405 | HTTP_406
    | HTTP_407 | HTTP_408 | HTTP_409 | HTTP_410 | HTTP_411 | HTTP_412 | HTTP_413
    | HTTP_422 | HTTP_428 | HTTP_429 | HTTP_414 | HTTP_415 | HTTP_416 | HTTP_417
    | HTTP_500 | HTTP_501 | HTTP_502 | HTTP_503 | HTTP_504 | HTTP_505

  let http_code = function
    | HTTP_100 -> 100 | HTTP_101 -> 101 | HTTP_200 -> 200 | HTTP_201 -> 201
    | HTTP_202 -> 202 | HTTP_203 -> 203 | HTTP_204 -> 204 | HTTP_205 -> 205
    | HTTP_206 -> 206 | HTTP_300 -> 300 | HTTP_301 -> 301 | HTTP_302 -> 302
    | HTTP_303 -> 303 | HTTP_304 -> 304 | HTTP_305 -> 305 | HTTP_307 -> 307
    | HTTP_400 -> 400 | HTTP_401 -> 401 | HTTP_402 -> 402 | HTTP_403 -> 403
    | HTTP_404 -> 404 | HTTP_405 -> 405 | HTTP_406 -> 406 | HTTP_407 -> 407
    | HTTP_408 -> 408 | HTTP_409 -> 409 | HTTP_410 -> 410 | HTTP_411 -> 411
    | HTTP_412 -> 412 | HTTP_413 -> 413 | HTTP_414 -> 414 | HTTP_415 -> 415
    | HTTP_416 -> 416 | HTTP_417 -> 417 | HTTP_422 -> 422 | HTTP_428 -> 428
    | HTTP_429 -> 429 | HTTP_500 -> 500 | HTTP_501 -> 501 | HTTP_502 -> 502
    | HTTP_503 -> 503 | HTTP_504 -> 504 | HTTP_505 -> 505

  let http_reason = function
    | HTTP_100 -> "Continue"
    | HTTP_101 -> "Switching Protocols"
    | HTTP_200 -> "OK"
    | HTTP_201 -> "Created"
    | HTTP_202 -> "Accepted"
    | HTTP_203 -> "Non-Authoritative Information"
    | HTTP_204 -> "No Content"
    | HTTP_205 -> "Reset Content"
    | HTTP_206 -> "Partial Content"
    | HTTP_300 -> "Multiple Choices"
    | HTTP_301 -> "Moved Permanently"
    | HTTP_302 -> "Found"
    | HTTP_303 -> "See Other"
    | HTTP_304 -> "Not Modified"
    | HTTP_305 -> "Use Proxy"
    | HTTP_307 -> "Temporary Redirect"
    | HTTP_400 -> "Bad Request"
    | HTTP_401 -> "Unauthorized"
    | HTTP_402 -> "Payment Required"
    | HTTP_403 -> "Forbidden"
    | HTTP_404 -> "Not Found"
    | HTTP_405 -> "Method Not Allowed"
    | HTTP_406 -> "Not Acceptable"
    | HTTP_407 -> "Proxy Authentication Required"
    | HTTP_408 -> "Request Timeout"
    | HTTP_409 -> "Conflict"
    | HTTP_410 -> "Gone"
    | HTTP_411 -> "Length Required"
    | HTTP_412 -> "Precondition Failed"
    | HTTP_413 -> "Request Entity Too Large"
    | HTTP_414 -> "Request-URI Too Long"
    | HTTP_415 -> "Unsupported Media Type"
    | HTTP_416 -> "Requested Range Not Satisfiable"
    | HTTP_417 -> "Expectation Failed"
    | HTTP_422 -> "Unprocessable Entity"
    | HTTP_428 -> "Precondition Required"
    | HTTP_429 -> "Too Many Requests"
    | HTTP_500 -> "Internal Server Error"
    | HTTP_501 -> "Not Implemented"
    | HTTP_502 -> "Bad Gateway"
    | HTTP_503 -> "Service Unavailable"
    | HTTP_504 -> "Gateway Timeout"
    | HTTP_505 -> "HTTP Version Not Supported"

  let http_message = function
    | HTTP_100 -> "Request received, please continue"
    | HTTP_101 -> "Switching to new protocol; obey Upgrade header"
    | HTTP_200 -> "Request fulfilled, document follows"
    | HTTP_201 -> "Document created, URL follows"
    | HTTP_202 -> "Request accepted, processing continues off-line"
    | HTTP_203 -> "Request fulfilled from cache"
    | HTTP_204 -> "Request fulfilled, nothing follows"
    | HTTP_205 -> "Clear input form for further input."
    | HTTP_206 -> "Partial content follows."
    | HTTP_300 -> "Object has several resources -- see URI list"
    | HTTP_301 -> "Object moved permanently -- see URI list"
    | HTTP_302 -> "Object moved temporarily -- see URI list"
    | HTTP_303 -> "Object moved -- see Method and URL list"
    | HTTP_304 -> "Document has not changed since given time"
    | HTTP_305 -> "You must use proxy specified in Location to access this resource."
    | HTTP_307 -> "Object moved temporarily -- see URI list"
    | HTTP_400 -> "Bad request syntax or unsupported method"
    | HTTP_401 -> "No permission -- see authorization schemes"
    | HTTP_402 -> "No payment -- see charging schemes"
    | HTTP_403 -> "Request forbidden -- authorization will not help"
    | HTTP_404 -> "Nothing matches the given URI"
    | HTTP_405 -> "Specified method is invalid for this resource."
    | HTTP_406 -> "URI not available in preferred format."
    | HTTP_407 -> "You must authenticate with this proxy before proceeding."
    | HTTP_408 -> "Request timed out; try again later."
    | HTTP_409 -> "Request conflict."
    | HTTP_410 -> "URI no longer exists and has been permanently removed."
    | HTTP_411 -> "Client must specify Content-Length."
    | HTTP_412 -> "Precondition in headers is false."
    | HTTP_413 -> "Entity is too large."
    | HTTP_414 -> "URI is too long."
    | HTTP_415 -> "Entity body in unsupported format."
    | HTTP_416 -> "Cannot satisfy request range."
    | HTTP_417 -> "Expect condition could not be satisfied."
    | HTTP_422 -> "The entity sent to the server was invalid."
    | HTTP_428 -> "You should verify the server accepts the request before sending it."
    | HTTP_429 -> "Request rate too high, chill out please."
    | HTTP_500 -> "Server got itself in trouble"
    | HTTP_501 -> "Server does not support this operation"
    | HTTP_502 -> "Invalid responses from another server/proxy."
    | HTTP_503 -> "The server cannot process the request due to a high load"
    | HTTP_504 -> "The gateway server did not receive a timely response"
    | HTTP_505 -> "Cannot fulfill request."

  type HttpCode with
    member x.Describe () =
      sprintf "%d %s: %s" (http_code x) (http_reason x) (http_message x)

  module internal Internals =

    open System
    open System.Reflection

    let SUAVE_VERSION = Assembly.GetExecutingAssembly().GetName().Version.ToString()

    let server_header = String.Concat [| "Server: Suave/"; SUAVE_VERSION; " (http://suave.io)" |]

  module internal Compression =

    open Socket

    open System
    open System.IO
    open System.IO.Compression
    open System.Collections.Concurrent

    type Algorithm =
      /// No compression
      | Plain
      /// GZIP compression
      | GZIP
      /// Deflate compression
      | Deflate
      /// Prints the algorithm as a string that can be put in a HTTP header
      override x.ToString() =
        match x with
        | Plain   -> "plain"
        | GZIP    -> "gzip"
        | Deflate -> "deflate"

    // You should only gzip files above a certain size threshold; we recommend a minimum range
    // between 150 and 1000 bytes. Gzipping files below 150 bytes can actually make them larger

    let MIN_BYTES_TO_COMPRESS =       500 // 500 bytes
    let MAX_BYTES_TO_COMPRESS = 524288000 // 500 megabytes

    let load_encoder s =
      match s with
      | "gzip"    -> Some (GZIP, Compression.gzip_encode)
      | "deflate" -> Some (Deflate, Compression.deflate_encode)
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
            | "gzip"    -> Some GZIP
            | "deflate" -> Some Deflate
            | _         -> None)
      | _ -> None

    let transform (content : byte []) (ctx : HttpContext) : Async<byte []> =
      async {
        if content.Length > MIN_BYTES_TO_COMPRESS && content.Length < MAX_BYTES_TO_COMPRESS then
          let request = ctx.request
          let enconding = get_encoder request
          match enconding with
          | Some (n,encoder) ->
            do! async_writeln ctx.connection (String.Concat [| "Content-Encoding: "; n.ToString() |])
            return encoder content
          | None ->
            return content
        else
          return content
      }

    let compress encoding path (fs : FileStream) = async {
      use new_fs = new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.Write)
      match encoding with
      | GZIP ->
        use gzip = new GZipStream(new_fs, CompressionMode.Compress)
        do! fs.CopyToAsync gzip
        gzip.Close()
      | Deflate ->
        use deflate = new DeflateStream(new_fs, CompressionMode.Compress)
        do! fs.CopyToAsync deflate
        deflate.Close()
      | _ ->
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
            do! async_writeln connection (String.Concat [| "Content-Encoding: "; n.ToString() |])
            if Globals.compressed_files_map.ContainsKey path then
              let file_info = new FileInfo(path)
              let cmpr_info = new FileInfo(Globals.compressed_files_map.[path])
              if file_info.LastWriteTime > cmpr_info.CreationTime then
                let! new_path =  compress_file n
                Globals.compressed_files_map.[path] <- new_path
            else
              let! new_path =  compress_file n
              Globals.compressed_files_map.TryAdd(path,new_path) |> ignore
            return new FileStream(Globals.compressed_files_map.[path] , FileMode.Open, FileAccess.Read, FileShare.Read)
          | None ->
            return fs
        else
          return fs
     }

  module Response =

    open Socket

    open System
    open System.IO

    let response_f (status_code : HttpCode)
                   (f_content : HttpRequest -> Async<unit>)
                   ({request = request; runtime = runtime; connection = connection } as context : HttpContext)
                   = async {
      try
        //let connection:Connection = runtime.connection
        do! async_writeln connection (String.concat " " [ "HTTP/1.1"
                                                        ; (http_code status_code).ToString()
                                                        ; http_reason status_code ])
        do! async_writeln connection Internals.server_header
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

    let response status_code
                 (cnt : byte [])
                 ({request = request; runtime = runtime; connection = connection } as context : HttpContext) =
      response_f status_code (
        fun r -> async {

          let! (content : byte []) = Compression.transform cnt context

          // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
          do! async_writeln connection (String.Concat [|  "Content-Length: "; content.Length.ToString() |])

          do! async_writeln connection ""

          if content.Length > 0 then
            do! connection.write (new ArraySegment<_>(content, 0, content.Length))
        })
        context

  module Writers =

    open System

    let set_header key value ({request = http_request; runtime = _ } as ctx : HttpContext) =
      http_request.response.Headers.Add(key, value)
      ctx

    let private cookie_to_string (x : HttpCookie) =
      let attributes = new System.Collections.Generic.List<string>()
      attributes.Add(String.Format("{0}={1}", x.name ,x.value))
      match x.domain  with | Some n -> attributes.Add(String.Format("Domain={0}", n))  | _ -> ()
      match x.path    with | Some n -> attributes.Add(String.Format("Path={0}", n))    | _ -> ()
      match x.expires with | Some n -> attributes.Add(String.Format("Expires={0}", n.ToString("R"))) | _ -> ()
      if x.http_only then attributes.Add(String.Format("HttpOnly"))
      if x.secure    then attributes.Add(String.Format("Secure"))
      String.concat "; " attributes

    let set_cookie (cookie : HttpCookie) =
      set_header "Set-Cookie" (cookie_to_string cookie)

    let mk_mime_type a b =
      { name = a
      ; compression = b } |> Some

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

  // http://www.web-cache.com/Writings/http-status-codes.html

  /// Intermediate responses - SUAVE TODO
  /// Functions have signature f :: TODO
  module Intermediate =

    open System

    let CONTINUE () = raise <| NotImplementedException("TODO")
    let SWITCHING_PROTO () = raise <| NotImplementedException("TODO")

  /// 2xx successful responses
  /// Functions have signature f :: params... -> HttpContext -> Async<unit> option.
  /// Functions from here are 'end routes' in that they don't require you to keep
  /// returning applicatives, but can end up in an async monad/workflow that writes
  /// the data to the client in the end.
  module Successful =

    open Response

    let ok s = response HTTP_200 s >> succeed

    let OK a = ok (UTF8.bytes a)

    let created s = response HTTP_201 s >> succeed

    let CREATED s = created (UTF8.bytes s)

    let accepted s = response HTTP_202 s >> succeed

    let ACCEPTED s = accepted (UTF8.bytes s)

    let no_content : WebPart =
      response HTTP_204 [||] >> succeed

    let NO_CONTENT = no_content

  /// 3xx Redirects
  /// Functions have signature f :: params... -> HttpContext -> Async<unit> option.
  /// Functions from here are 'end routes' in that they don't require you to keep
  /// returning applicatives, but can end up in an async monad/workflow that writes
  /// the data to the client in the end.
  module Redirect =

    open Response
    open Writers

    let moved_permanently location =
      set_header "Location" location
      >> response HTTP_301 [||]
      >> succeed

    let MOVED_PERMANENTLY location = moved_permanently location

    let found location =
      set_header "Location" location
      >> response HTTP_302 [||]
      >> succeed

    let FOUND location = found location

    let redirect url =
      set_header "Location" url
      >> set_header "Content-Type" "text/html; charset=utf-8"
      >> response HTTP_302 (UTF8.bytes(sprintf "<html>
    <body>
      <a href=\"%s\">%s</a>
    </body>
  </html>
  " url (http_message HTTP_302)))
      >> succeed

    let not_modified : WebPart =
      response HTTP_304 [||] >> succeed

    let NOT_MODIFIED : WebPart =
      not_modified

  module RequestErrors =

    open Response
    open Writers

    let bad_request s = response HTTP_400 s >> succeed

    let BAD_REQUEST s = bad_request (UTF8.bytes s)

    /// 401: see http://stackoverflow.com/questions/3297048/403-forbidden-vs-401-unauthorized-http-responses/12675357
    let unauthorized s =
      set_header "WWW-Authenticate" "Basic realm=\"protected\""
      >> response HTTP_401 s
      >> succeed

    let UNAUTHORIZED s = unauthorized (UTF8.bytes s)

    let challenge = UNAUTHORIZED (http_message HTTP_401)

    let forbidden s = response HTTP_403 s >> succeed

    let FORBIDDEN s = forbidden (UTF8.bytes s)

    let not_found s = response HTTP_404 s >> succeed

    let NOT_FOUND message = not_found (UTF8.bytes message)

    let method_not_allowed s = response HTTP_405 s >> succeed

    let METHOD_NOT_ALLOWED s = method_not_allowed (UTF8.bytes s)

    let not_acceptable s = response HTTP_406 s >> succeed

    let NOT_ACCEPTABLE message = not_acceptable (UTF8.bytes message)

    let request_timeout = response HTTP_408 [||] >> succeed

    // all-caps req.timeout elided intentionally, as nothing can be passed to
    // a writing client

    let conflict s = response HTTP_409 s >> succeed

    let CONFLICT message = conflict (UTF8.bytes message)

    let gone s = response HTTP_410 s >> succeed

    let GONE s = gone (UTF8.bytes s)

    let unsupported_media_type s = response HTTP_415 s >> succeed

    let UNSUPPORTED_MEDIA_TYPE s = unsupported_media_type (UTF8.bytes s)

    let unprocessable_entity s = response HTTP_422 s >> succeed

    let UNPROCESSABLE_ENTITY s = unprocessable_entity (UTF8.bytes s)

    let precondition_required body = response HTTP_428 body >> succeed

    let PRECONDITION_REQUIRED body = precondition_required (UTF8.bytes body)

    let too_many_requests s = response HTTP_429 s >> succeed

    let TOO_MANY_REQUESTS s = too_many_requests (UTF8.bytes s)

  module ServerErrors =

    open Response

    let internal_error arr = response HTTP_500 arr >> succeed

    let INTERNAL_ERROR message = internal_error (UTF8.bytes message)

    let not_implemented arr = response HTTP_501 arr >> succeed

    let NOT_IMPLEMENTED message = not_implemented (UTF8.bytes message)

    let bad_gateway arr = response HTTP_502 arr >> succeed

    let BAD_GATEWAY message = bad_gateway (UTF8.bytes message)

    let service_unavailable arr = response HTTP_503 arr >> succeed

    let SERVICE_UNAVAILABLE message = service_unavailable (UTF8.bytes message)

    let gateway_timeout arr = response HTTP_504 arr >> succeed

    let GATEWAY_TIMEOUT message = gateway_timeout (UTF8.bytes message)

    let invalid_http_version arr = response HTTP_505 arr >> succeed

    let INVALID_HTTP_VERSION = invalid_http_version (UTF8.bytes (http_message HTTP_505))

  /// Module that deals with the applicatives of suave - use functions from this module
  /// to filter what requests a given route responds to.
  /// Functions have signature f :: params... -> HttpContext -> HttpContext option.
  module Applicatives =

    open System
    open System.Text.RegularExpressions

    let url s (x : HttpContext) = if s = x.request.url then Some x else None

    let ``method`` (s : HttpMethod) (x : HttpContext) =
      if s.ToString() = x.request.``method`` then Some x else None

    let is_secure (x : HttpContext) = if x.request.is_secure then Some x else None

    let url_regex s (x : HttpContext) = if Regex.IsMatch(x.request.url,s) then Some x else None

    // see http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html

    let GET     (x : HttpContext) = ``method`` GET x
    let POST    (x : HttpContext) = ``method`` POST x
    let DELETE  (x : HttpContext) = ``method`` DELETE x
    let PUT     (x : HttpContext) = ``method`` PUT x
    let HEAD    (x : HttpContext) = ``method`` HEAD x
    let CONNECT (x : HttpContext) = ``method`` CONNECT x
    let PATCH   (x : HttpContext) = ``method`` PATCH x
    let TRACE   (x : HttpContext) = ``method`` TRACE x
    let OPTIONS (x : HttpContext) = ``method`` OPTIONS x

    /// The default log format for <see cref="log" />.
    let log_format (ctx : HttpContext) =
      let r = ctx.request
      sprintf "%A\n" (r.``method``, ctx.connection.ipaddr, r.url, r.query, r.form, r.headers)

    let log (logger : Log.Logger) (formatter : HttpContext -> string) (ctx : HttpContext) =
      logger.Log Log.LogLevel.Debug <| fun _ ->
        { trace         = ctx.request.trace
        ; message       = formatter ctx
        ; level         = Log.LogLevel.Debug
        ; path          = "suave/web-requests"
        ; ``exception`` = None
        ; ts_utc_ticks  = DateTime.UtcNow.Ticks }

      succeed ctx

    open Suave.Sscanf
    open ServerErrors

    let url_scan (pf : PrintfFormat<_,_,_,_,'t>) (h : 't ->  WebPart) : WebPart =
      let t url = sscanf pf url

      let F (r:HttpContext) =
        try
          let y = r.request.url |> t |> h
          try y r with ex -> r |> INTERNAL_ERROR (ex.ToString())
        with _ -> fail
      F

  module Files =

    open System
    open System.IO
    open System.Text

    open Suave.Socket

    open Response
    open Writers
    open Successful
    open Redirect

    let send_file file_name (compression : bool) ({ connection = conn; runtime = runtime } as ctx : HttpContext) =
      let write_file file (q : HttpRequest) = async {
        use! fs = Compression.transform_x file compression runtime.compression_folder ctx

        do! async_writeln conn (sprintf "Content-Length: %d" (fs : FileStream).Length)
        do! async_writeln conn ""

        if fs.Length > 0L then
          do! transfer_x conn fs
      }
      async { do! response_f HTTP_200 (write_file file_name) ctx } |> succeed

    // If a response includes both an Expires header and a max-age directive,
    // the max-age directive overrides the Expires header, even if the Expires header is more restrictive
    // 'Cache-Control' and 'Expires' headers should be left up to the user

    let file file_name =
      fun ({request = r; runtime = q} as ctx) ->
        if File.Exists file_name then
          let file_info = new FileInfo(file_name)
          let mimes = q.mime_types_map (file_info.Extension)
          match mimes with
          | Some value ->
            let send_it _ =
              set_header "Last-Modified" (file_info.LastAccessTimeUtc.ToString("R"))
              >> set_mime_type value.name
              >> send_file file_name value.compression

            let modified_since = (r.headers ? ``if-modified-since`` )
            match modified_since with
            | Some v -> let date = DateTime.Parse v
                        if file_info.LastWriteTime > date then send_it () ctx
                        else NOT_MODIFIED ctx
            | None   -> send_it () ctx
          | None -> None
        else
          None

    let local_file (file_name : string) (root_path : string) =
      let file_name =
        if Path.DirectorySeparatorChar.Equals('/') then file_name
        else file_name.Replace('/', Path.DirectorySeparatorChar)
      let calculated_path = Path.Combine(root_path, file_name.TrimStart([| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |]))
      if calculated_path = Path.GetFullPath(calculated_path) then
        if calculated_path.StartsWith root_path then
          calculated_path
        else raise <| Exception("File canonalization issue.")
      else raise <| Exception("File canonalization issue.")

    let browse_file file_name =
      fun ({request = r; runtime = q} as h) -> file (local_file file_name q.home_directory) h

    let browse : WebPart = warbler (fun {request = r; runtime = q } -> file (local_file r.url q.home_directory))

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
        OK (result.ToString()) ctx
      else fail

  module Authentication =

    open RequestErrors

    let internal parse_authentication_token (token : string) =
      let parts = token.Split (' ')
      let enc = parts.[1].Trim()
      let decoded = ASCII.base64_decode enc
      let indexOfColon = decoded.IndexOf(':')
      (parts.[0].ToLower(), decoded.Substring(0,indexOfColon), decoded.Substring(indexOfColon+1))

    let authenticate_basic f (ctx : HttpContext) =
      let p = ctx.request
      let headers = p.headers
      if headers.ContainsKey("authorization") then
        let header = headers.["authorization"]
        let (typ, username, password) = parse_authentication_token header
        p.user_name <- username
        p.password <- password
        if (typ.Equals("basic")) && f p then
          fail
        else
          challenge ctx
      else
        challenge ctx
