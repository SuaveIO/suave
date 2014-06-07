namespace Suave

module Http =

  open Socket
  open Types

  let inline succeed x = Some x

  let fail = None

  let inline never _ = None

  let bind = Option.bind

  let inline delay f = f()

  let inline (>>=) a b = fun x -> Option.bind b (a x)

  let inline (>=>) a b = fun x ->
    match a x with
    | None   -> b x
    | r      -> r

  let rec choose options arg =
    match options with
    | []        -> None
    | p :: tail ->
      match p arg with
      | Some x -> Some x
      | None   -> choose tail arg

  let inline warbler f a = f a a //which bird? A Warbler!

  let inline (>>==) a b = a >>= warbler (fun r -> b r)

  let inline cnst x = fun _ -> x

  let cond d f g a =
    match d with
    | Some x -> f x a
    | None   -> g a

  module Response =

    open Socket
    open Types
    open Types.Codes

    open System
    open System.IO

    let response status_code (cnt : byte []) =
      fun (ctx : HttpContext) -> 
        let response = 
          { ctx.response with status = status_code; content = Bytes cnt }
        { ctx with response = response }

  module Writers =

    open System

    let set_header key value (ctx : HttpContext) =
      let new_response =
          { ctx.response with headers = (key,value) :: ctx.response.headers }
      { ctx with response = new_response }

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

    // TODO: I'm not sure about having MIME types in the Writers module
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

  // 1xx
  module Intermediate =

    open System

    let CONTINUE (ctx : HttpContext) : HttpContext option =
      raise <| NotImplementedException("TODO")

    let SWITCHING_PROTO (ctx : HttpContext) : HttpContext option =
      raise <| NotImplementedException("TODO")

  // 2xx
  module Successful =

    open Response
    open Types.Codes

    let ok s : WebPart = 
      fun ctx -> { ctx with response = { ctx.response with status = HTTP_200; content = Bytes s }} |> succeed

    let OK a = ok (UTF8.bytes a)

    let created s = response HTTP_201 s >> succeed

    let CREATED s = created (UTF8.bytes s)

    let accepted s = response HTTP_202 s >> succeed

    let ACCEPTED s = accepted (UTF8.bytes s)

    let no_content : WebPart =
      fun ctx -> { ctx with response = { status = HTTP_204; headers = ctx.response.headers; content = Bytes [||] }} |> succeed

    let NO_CONTENT = no_content

  // 3xx
  module Redirection =

    open Response
    open Writers
    open Types.Codes

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
      >> response HTTP_302 (
        UTF8.bytes(sprintf "<html>
    <body>
      <a href=\"%s\">%s</a>
    </body>
  </html>"
        url (http_message HTTP_302)))
      >> succeed

    let not_modified : WebPart =
      fun ctx -> { ctx with response = {status = HTTP_304; headers = []; content = Bytes [||] }} |> succeed

    let NOT_MODIFIED : WebPart =
      not_modified

  // 4xx
  module RequestErrors =

    open Response
    open Writers
    open Types.Codes

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
    open Types.Codes

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

  module Applicatives =

    open System
    open System.Text.RegularExpressions

    open Types.Methods

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

    /// The default log format for <see cref="log" />.  NCSA Common log format
    /// 
    /// 127.0.0.1 user-identifier frank [10/Oct/2000:13:55:36 -0700] "GET /apache_pb.gif HTTP/1.0" 200 2326
    /// 
    /// A "-" in a field indicates missing data.
    /// 
    /// 127.0.0.1 is the IP address of the client (remote host) which made the request to the server.
    /// user-identifier is the RFC 1413 identity of the client.
    /// frank is the userid of the person requesting the document.
    /// [10/Oct/2000:13:55:36 -0700] is the date, time, and time zone when the server finished processing the request, by default in strftime format %d/%b/%Y:%H:%M:%S %z.
    /// "GET /apache_pb.gif HTTP/1.0" is the request line from the client. The method GET, /apache_pb.gif the resource requested, and HTTP/1.0 the HTTP protocol.
    /// 200 is the HTTP status code returned to the client. 2xx is a successful response, 3xx a redirection, 4xx a client error, and 5xx a server error.
    /// 2326 is the size of the object returned to the client, measured in bytes.
    let log_format (ctx : HttpContext) =
      
      let dash = function | "" | null -> "-" | x -> x
      let ci = Globalization.CultureInfo("en-US")
      let process_id = System.Diagnostics.Process.GetCurrentProcess().Id.ToString()
      sprintf "%O %s %s [%s] \"%s %s %s\" %d %s"
        ctx.request.ipaddr
        process_id //TODO: obtain connection owner via Ident protocol
        (dash ctx.request.user_name)
        (DateTime.UtcNow.ToString("dd/MMM/yyyy:hh:mm:ss %K", ci))
        ctx.request.``method``
        ctx.request.url
        ctx.request.http_version
        (Codes.http_code ctx.response.status)
        "0"

    let log (logger : Log.Logger) (formatter : HttpContext -> string) (ctx : HttpContext) =
      logger.Log Log.LogLevel.Debug <| fun _ ->
        { trace         = ctx.request.trace
        ; message       = formatter ctx
        ; level         = Log.LogLevel.Debug
        ; path          = "suave/web-requests"
        ; ``exception`` = None
        ; ts_utc_ticks  = Globals.utc_now().Ticks }

      succeed ctx

    open Suave.Sscanf
    open ServerErrors

    let url_scan (pf : PrintfFormat<_,_,_,_,'t>) (h : 't ->  WebPart) : WebPart =
      
      let scan url =
        try 
          let r = sscanf pf url
          Some r
        with _ -> None

      let F (r:HttpContext) =
        match scan r.request.url with
        | Some p ->
          let part = h p
          part r 
        | None -> 
          fail
      F

    let resolve (part: WebPart) = fun (ctx: HttpContext) ->
      async {
        return part ctx
      }
    
    let timeout_webpart (time_span : TimeSpan) (web_part : WebPart) : WebPart =
      fun (ctx : HttpContext) ->
        try
          Async.RunSynchronously (resolve web_part ctx, int time_span.TotalMilliseconds)
        with
          | :? TimeoutException ->
            Response.response Codes.HTTP_408 (UTF8.bytes "Request Timeout") ctx |> Some

  module ServeResource =
    open System

    open Writers
    open Redirection

    // If a response includes both an Expires header and a max-age directive,
    // the max-age directive overrides the Expires header, even if the Expires header is more restrictive
    // 'Cache-Control' and 'Expires' headers should be left up to the user
    let resource key exists get_last get_extension (send : string -> bool -> HttpContext -> HttpContext option) ({request = r } as ctx) =

      let send_it name compression =
        set_header "Last-Modified" ((get_last key : DateTime).ToString("R"))
        >> set_mime_type name
        >> send key compression

      if exists key then
          let mimes = ctx.runtime.mime_types_map <| get_extension key
          match mimes with
          | Some value ->
            let modified_since = (r.headers ? ``if-modified-since`` )
            match modified_since with
            | Some v -> let date = DateTime.Parse v
                        if get_last key > date then send_it value.name value.compression ctx
                        else NOT_MODIFIED ctx 
            | None   -> send_it  value.name value.compression ctx
          | None -> None
      else
        None

  module Files =

    open System
    open System.IO
    open System.Text

    open Suave.Socket
    open Types.Codes

    open Response
    open Writers
    open Successful
    open Redirection
    open ServeResource

    let send_file file_name (compression : bool) (ctx : HttpContext) =
      let write_file file conn = socket {
        let get_fs = fun path -> new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read) :> Stream
        let get_lm = fun path -> FileInfo(path).LastWriteTime
        use! fs = Compression.transform_x file get_fs get_lm compression ctx.runtime.compression_folder ctx conn

        do! async_writeln conn (sprintf "Content-Length: %d" (fs: Stream).Length)
        do! async_writeln conn ""

        if fs.Length > 0L then
          do! transfer_x conn fs
      }
      { ctx with response = { ctx.response with status = HTTP_200; content = SocketTask (write_file file_name)}} |> Some

    let file file_name =
      resource
        file_name
        (File.Exists)
        (fun name -> FileInfo(name).LastAccessTime)
        (Path.GetExtension)
        send_file

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

    let dir (ctx : HttpContext) =
      let req = ctx.request

      let url = req.url

      let dirname = local_file url (ctx.runtime.home_directory)
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

  module Embedded =
    
    open System
    open System.IO
    open System.Reflection

    open Suave.Socket
    open Types.Codes

    open Response
    open ServeResource
    
    let assembly = 
      if Assembly.GetEntryAssembly() = null
      then Assembly.GetCallingAssembly()
      else Assembly.GetEntryAssembly()

    let resources = assembly.GetManifestResourceNames()
    let last_modified = FileInfo(Assembly.GetExecutingAssembly().Location).CreationTime
    
    let send_resource resource_name (compression : bool) (ctx : HttpContext) =
      let write_resource name conn = socket {
        let get_fs = fun name -> assembly.GetManifestResourceStream(name)
        let get_lm = fun _ -> last_modified
        use! fs = Compression.transform_x name get_fs get_lm compression ctx.runtime.compression_folder ctx conn

        do! async_writeln conn (sprintf "Content-Length: %d" (fs: Stream).Length)
        do! async_writeln conn ""

        if fs.Length > 0L then
          do! transfer_x conn fs
      }
      { ctx with response = { ctx.response with status = HTTP_200; content = SocketTask (write_resource resource_name)}} |> Some

    let resource name =
      resource
        name
        (fun name -> resources |> Array.exists ((=) name))
        (fun _ -> last_modified)
        (Path.GetExtension)
        send_resource

    let browse : WebPart =
      warbler (fun ctx -> resource (ctx.request.url.TrimStart [|'/'|]))
      

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
