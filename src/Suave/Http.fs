﻿namespace Suave

module Http =

  open Suave.Sockets
  open Suave.Types

  let inline succeed x = async.Return (Some x)

  let fail = async.Return None

  let never : WebPart = fun x -> fail

  let inline bind (second : 'b -> Async<'c option>) (first : 'a -> Async<'b option>) : 'a -> Async<'c option> =
    fun x -> 
      async {
        let! e = first x
        match e with
        | None ->
          return None
        | Some t ->
          return! second t }

  let inline (>>=) (first : 'a -> Async<'b option>)  (second : 'b -> Async<'c option>) : 'a -> Async<'c option> =
    fun x ->
      bind second first x

  let inline (>=>) a b = fun x ->
    match a x with
    | None   -> b x
    | r      -> r

  let inline (<|>) (a : WebPart) (b : WebPart) : WebPart =
    fun x ->
      async{
        let! e = a x
        match e with
        | None   ->
          let! result = b x
          match result with
          | None -> return None
          | r -> return r
        | r -> return r
      }

  let rec choose (options : WebPart list): WebPart =
    fun arg -> async {
    match options with
    | []        -> return None
    | p :: tail ->
      let! res = p arg 
      match res with
      | Some x -> return Some x
      | None   -> return! choose tail arg
    }

  /// Inject a web_part
  ///
  /// +------------+                                            +--------------+
  /// | url "/a"   +----------+                       +---------+   cont1      |
  /// +------------+          |                       |         +--------------+
  ///                         |                       |                         
  /// +-------------+         |       +----------+    |         +--------------+
  /// |  url "/b"   +---------+-------+ injected +----+---------+  cont2       |
  /// +-------------+         |       +----------+    |         +--------------+
  ///                         |                       |                         
  /// +-------------+         |                       |         +--------------+
  /// | url "/b"    +---------+                       +---------+  cont3       |
  /// +-------------+                                           +--------------+

  let rec inject (post_op : WebPart) (pairs : (WebPart*WebPart) list) : WebPart =
    fun arg -> async {
      match pairs with
      | []        -> return None
      | (p,q) :: tail ->
        let! res = p arg
        match res with
        | Some x ->
          return! (post_op >>= q) x
        | None   -> return! inject post_op tail arg
      }

  let inline warbler f a = f a a //which bird? A Warbler!

  let inline cnst x = fun _ -> x

  let cond d f g a =
    match d with
    | Some x -> f x a
    | None   -> g a

  module Response =

    open Types
    open Types.Codes

    open System
    open System.IO

    let response status_code (cnt : byte []) =
      fun (ctx : HttpContext) ->
        let response = 
          { ctx.response with status = status_code; content = Bytes cnt }
        { ctx with response = response } |> succeed

  module Writers =
    // TODO: transform into a set of lenses with Aether
    // @ https://github.com/xyncro/aether and move closer to HttpContext.

    open System

    let set_header key value (ctx : HttpContext) =
      { ctx with response = { ctx.response with headers = (key, value) :: ctx.response.headers } }
      |> succeed

    let set_user_data key value (ctx : HttpContext) =
      { ctx with user_state = ctx.user_state |> Map.add key (box value) }
      |> succeed

    let unset_user_data key (ctx : HttpContext) =
      { ctx with user_state = ctx.user_state |> Map.remove key }
      |> succeed

    // TODO: I'm not sure about having MIME types in the Writers module
    let mk_mime_type a b =
      MimeType.mk a b |> Some

    let default_mime_types_map =
      let types = [
        ".bmp" , MimeType.mk "image/bmp" false
        ".css" , MimeType.mk "text/css" true
        ".gif" , MimeType.mk "image/gif" false
        ".png" , MimeType.mk "image/png" false
        ".svg" , MimeType.mk "image/svg+xml" false
        ".ico" , MimeType.mk "image/x-icon" false
        ".htm" , MimeType.mk "text/html" true
        ".html", MimeType.mk "text/html" true
        ".jpe" , MimeType.mk "image/jpeg" false
        ".jpeg", MimeType.mk "image/jpeg" false
        ".jpg" , MimeType.mk "image/jpeg" false
        ".js"  , MimeType.mk "application/x-javascript" true
        ".exe" , MimeType.mk "application/exe" false
        ".txt" , MimeType.mk "text/plain" true
      ]
      types |> Map.ofList

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
    
    open Suave.Utils
    open Response
    open Types.Codes
    

    let ok s : WebPart = 
      fun ctx -> { ctx with response = { ctx.response with status = HTTP_200; content = Bytes s }} |> succeed

    let OK a = ok (UTF8.bytes a)

    let created s = response HTTP_201 s

    let CREATED s = created (UTF8.bytes s)

    let accepted s = response HTTP_202 s

    let ACCEPTED s = accepted (UTF8.bytes s)

    let no_content : WebPart =
      fun ctx -> { ctx with response = { status = HTTP_204; headers = ctx.response.headers; content = Bytes [||] }} |> succeed

    let NO_CONTENT = no_content

  // 3xx
  module Redirection =

    open Suave.Utils
    open Response
    open Writers
    open Types.Codes

    let moved_permanently location =
      set_header "Location" location
      >>= response HTTP_301 [||]

    let MOVED_PERMANENTLY location = moved_permanently location

    let found location =
      set_header "Location" location
      >>= response HTTP_302 [||]

    let FOUND location = found location

    let redirect url =
      set_header "Location" url
      >>= set_header "Content-Type" "text/html; charset=utf-8"
      >>= response HTTP_302 (
        UTF8.bytes(sprintf "<html>
    <body>
      <a href=\"%s\">%s</a>
    </body>
  </html>"
        url (http_message HTTP_302)))
     

    let not_modified : WebPart =
      fun ctx -> { ctx with response = {status = HTTP_304; headers = []; content = Bytes [||] }} |> succeed

    let NOT_MODIFIED : WebPart =
      not_modified

  // 4xx
  module RequestErrors =

    open Suave.Utils
    open Response
    open Writers
    open Types.Codes

    let bad_request s = response HTTP_400 s

    let BAD_REQUEST s = bad_request (UTF8.bytes s)

    /// 401: see http://stackoverflow.com/questions/3297048/403-forbidden-vs-401-unauthorized-http-responses/12675357
    let unauthorized s =
      set_header "WWW-Authenticate" "Basic realm=\"protected\""
      >>= response HTTP_401 s

    let UNAUTHORIZED s = unauthorized (UTF8.bytes s)

    let challenge = UNAUTHORIZED (http_message HTTP_401)

    let forbidden s = response HTTP_403 s

    let FORBIDDEN s = forbidden (UTF8.bytes s)

    let not_found s = response HTTP_404 s

    let NOT_FOUND message = not_found (UTF8.bytes message)

    let method_not_allowed s = response HTTP_405 s

    let METHOD_NOT_ALLOWED s = method_not_allowed (UTF8.bytes s)

    let not_acceptable s = response HTTP_406 s

    let NOT_ACCEPTABLE message = not_acceptable (UTF8.bytes message)

    let request_timeout = response HTTP_408 [||]

    // all-caps req.timeout elided intentionally, as nothing can be passed to
    // a writing client

    let conflict s = response HTTP_409 s

    let CONFLICT message = conflict (UTF8.bytes message)

    let gone s = response HTTP_410 s

    let GONE s = gone (UTF8.bytes s)

    let unsupported_media_type s = response HTTP_415 s

    let UNSUPPORTED_MEDIA_TYPE s = unsupported_media_type (UTF8.bytes s)

    let unprocessable_entity s = response HTTP_422 s

    let UNPROCESSABLE_ENTITY s = unprocessable_entity (UTF8.bytes s)

    let precondition_required body = response HTTP_428 body

    let PRECONDITION_REQUIRED body = precondition_required (UTF8.bytes body)

    let too_many_requests s = response HTTP_429 s

    let TOO_MANY_REQUESTS s = too_many_requests (UTF8.bytes s)

  module ServerErrors =
  
    open Suave.Utils
    open Response
    open Types.Codes

    let internal_error arr = response HTTP_500 arr

    let INTERNAL_ERROR message = internal_error (UTF8.bytes message)

    let not_implemented arr = response HTTP_501 arr

    let NOT_IMPLEMENTED message = not_implemented (UTF8.bytes message)

    let bad_gateway arr = response HTTP_502 arr

    let BAD_GATEWAY message = bad_gateway (UTF8.bytes message)

    let service_unavailable arr = response HTTP_503 arr

    let SERVICE_UNAVAILABLE message = service_unavailable (UTF8.bytes message)

    let gateway_timeout arr = response HTTP_504 arr

    let GATEWAY_TIMEOUT message = gateway_timeout (UTF8.bytes message)

    let invalid_http_version arr = response HTTP_505 arr

    let INVALID_HTTP_VERSION = invalid_http_version (UTF8.bytes (http_message HTTP_505))

  module Applicatives =
  

    open Suave.Utils
    open Suave.Logging
    open System
    open System.Text.RegularExpressions

    open Types

    module private Option =
      let iff b x =
        if b then Some x else None

    let url s (x : HttpContext) =
      async.Return (Option.iff (s = x.request.url.AbsolutePath) x)

    let ``method`` (m : HttpMethod) (x : HttpContext) =
      async.Return (Option.iff (m = x.request.``method``) x)

    let is_secure (x : HttpContext) =
      async.Return (Option.iff x.request.is_secure x)

    let url_regex regex (x : HttpContext) =
      async.Return (Option.iff (Regex.IsMatch(x.request.url.AbsolutePath, regex)) x)

    let host hostname (x : HttpContext) = async {
      if x.request.host.value = hostname then
        return Some { x with request = { x.request with host = ServerClient hostname } }
      else
        return None
      }

    // see http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html

    let GET     (x : HttpContext) = ``method`` HttpMethod.GET x
    let POST    (x : HttpContext) = ``method`` HttpMethod.POST x
    let DELETE  (x : HttpContext) = ``method`` HttpMethod.DELETE x
    let PUT     (x : HttpContext) = ``method`` HttpMethod.PUT x
    let HEAD    (x : HttpContext) = ``method`` HttpMethod.HEAD x
    let CONNECT (x : HttpContext) = ``method`` HttpMethod.CONNECT x
    let PATCH   (x : HttpContext) = ``method`` HttpMethod.PATCH x
    let TRACE   (x : HttpContext) = ``method`` HttpMethod.TRACE x
    let OPTIONS (x : HttpContext) = ``method`` HttpMethod.OPTIONS x

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
        (match Map.tryFind "user_name" ctx.user_state with Some x -> x :?> string | None -> "-")
        (DateTime.UtcNow.ToString("dd/MMM/yyyy:hh:mm:ss %K", ci))
        (string ctx.request.``method``)
        ctx.request.url.AbsolutePath
        ctx.request.http_version
        (Codes.http_code ctx.response.status)
        "0"

    let log (logger : Logger) (formatter : HttpContext -> string) (ctx : HttpContext) =
      logger.Log LogLevel.Debug <| fun _ ->
        { trace         = ctx.request.trace
          message       = formatter ctx
          level         = LogLevel.Debug
          path          = "Suave.Http.web-requests"
          ``exception`` = None
          ts_utc_ticks  = Globals.utc_now().Ticks }

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
        match scan r.request.url.AbsolutePath with
        | Some p ->
          let part = h p
          part r 
        | None -> 
          fail
      F
          
    let timeout_webpart (time_span : TimeSpan) (web_part : WebPart) : WebPart =
      fun (ctx : HttpContext) -> async {
        try
          return! Async.WithTimeout (time_span, web_part ctx)
        with
          | :? TimeoutException ->
            return! Response.response Codes.HTTP_408 (UTF8.bytes "Request Timeout") ctx
            }

  module ServeResource =
    open System

    open Writers
    open Redirection
    open RequestErrors
    open Model
    open Suave.Utils
    open Suave.Logging

    // If a response includes both an Expires header and a max-age directive,
    // the max-age directive overrides the Expires header, even if the Expires header is more restrictive
    // 'Cache-Control' and 'Expires' headers should be left up to the user
    let resource key exists get_last get_extension
                 (send : string -> bool -> WebPart)
                 ({ request = r; runtime = rt } as ctx) =
      let log =
        Log.verbose rt.logger "Suave.Http.ServeResource.resource" TraceHeader.empty

      let send_it name compression =
        set_header "Last-Modified" ((get_last key : DateTime).ToString("R"))
        >>= set_mime_type name
        >>= send key compression

      if exists key then
        let mimes = ctx.runtime.mime_types_map <| get_extension key
        match mimes with
        | Some value ->
          let modified_since = r.headers %% "if-modified-since"
          match modified_since with
          | Some v ->
            match Parse.date_time v with
            | Choice1Of2 date ->
              if get_last key > date then send_it value.name value.compression ctx
              else NOT_MODIFIED ctx
            | Choice2Of2 parse_error -> bad_request [||] ctx
          | None -> send_it value.name value.compression ctx
        | None ->
          let ext = get_extension key
          log (sprintf "failed to find matching mime for ext '%s'" ext)
          fail
      else
        log (sprintf "failed to find resource by key '%s'" key)
        fail

  module Files =

    open System
    open System.IO
    open System.Text

    open Suave.Utils
    open Suave.Logging
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

        do! async_writeln conn (sprintf "Content-Length: %d" (fs : Stream).Length)
        do! async_writeln conn ""

        if fs.Length > 0L then
          do! transfer_x conn fs
      }
      { ctx with
          response =
            { ctx.response with
                status = HTTP_200
                content = SocketTask (write_file file_name) } }
      |> succeed

    let file file_name =
      resource
        file_name
        (File.Exists)
        (fun name -> FileInfo(name).LastAccessTime)
        (Path.GetExtension)
        send_file

    let resolve_path (root_path : string) (file_name : string) =
      let file_name =
        if Path.DirectorySeparatorChar.Equals('/') then file_name
        else file_name.Replace('/', Path.DirectorySeparatorChar)
      let calculated_path = Path.Combine(root_path, file_name.TrimStart([| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |]))
      if calculated_path = Path.GetFullPath(calculated_path) then
        if calculated_path.StartsWith root_path then
          calculated_path
        else raise <| Exception("File canonalization issue.")
      else raise <| Exception("File canonalization issue.")

    [<Obsolete("Use resolve_path")>]
    let local_file file_name root_path = resolve_path root_path file_name

    let browse_file root_path file_name =
      fun ({request = r; runtime = q} as h) ->
        file (resolve_path root_path file_name) h

    let browse_file' file_name =
      fun ({request = r; runtime = q} as h) ->
        browse_file q.home_directory file_name h
    
    let browse root_path : WebPart =
      warbler (fun { request = r; runtime = { logger = l } } ->
        Log.verbose l
          "Suave.Http.Files.browse"
          TraceHeader.empty
          (sprintf "Files.browse trying file (local_file url:'%s' root:'%s')"
            r.url.AbsolutePath root_path)
        file (resolve_path root_path r.url.AbsolutePath))

    let browse' : WebPart =
      warbler (fun { runtime = q } -> browse q.home_directory)

    let dir root_path (ctx : HttpContext) =
      let req = ctx.request

      let url = req.url

      let dirname = resolve_path root_path url.AbsolutePath
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

    let dir' ctx =
      dir ctx.runtime.home_directory ctx

  module Embedded =
    
    open System
    open System.IO
    open System.Reflection

    open Suave.Utils
    open Types.Codes

    open Response
    open ServeResource
    
    let default_source_assembly =
      if Assembly.GetEntryAssembly() = null
      then Assembly.GetCallingAssembly()
      else Assembly.GetEntryAssembly()

    let resources (assembly : Assembly) =
      assembly.GetManifestResourceNames()

    let last_modified (assembly : Assembly) =
      FileInfo(assembly.Location).CreationTime
    
    let send_resource (assembly : Assembly)
                      resource_name
                      (compression : bool)
                      (ctx : HttpContext) =
      let write_resource name conn = socket {
        let get_fs = fun name -> assembly.GetManifestResourceStream(name)
        let get_lm = fun _ -> last_modified assembly
        use! fs = Compression.transform_x name get_fs get_lm compression ctx.runtime.compression_folder ctx conn

        do! async_writeln conn (sprintf "Content-Length: %d" (fs: Stream).Length)
        do! async_writeln conn ""

        if fs.Length > 0L then
          do! transfer_x conn fs
      }
      { ctx with
          response =
            { ctx.response with
                status = HTTP_200
                content = SocketTask (write_resource resource_name) }}
      |> succeed

    let send_resource' resource_name compression =
      send_resource default_source_assembly resource_name compression

    let resource assembly name =
      resource
        name
        (fun name -> resources assembly |> Array.exists ((=) name))
        (fun _ -> last_modified assembly)
        (Path.GetExtension)
        (send_resource assembly)

    let resource' name =
      resource default_source_assembly name

    let browse assembly : WebPart =
      warbler (fun ctx -> resource assembly (ctx.request.url.AbsolutePath.TrimStart [|'/'|]))

    let browse' : WebPart =
      browse default_source_assembly

  // See www.w3.org/TR/eventsource/#event-stream-interpretation
  module EventSource =
    open System
    

    open Suave
    open Suave.Sockets
    open Suave.Sockets.Connection
    open Suave.Types
    open Suave.Utils

    [<Literal>]
    let private ES_EOL = "\n"

    let private ES_EOL_S = ArraySegment<_>(UTF8.bytes ES_EOL, 0, 1)

    let async_write (out : Connection) (data : string) =
      async_writebytes out (UTF8.bytes data)

    let (<<.) (out : Connection) (data : string) =
      async_writebytes out (UTF8.bytes data)

    let dispatch (out : Connection) =
      send out ES_EOL_S

    let comment (out : Connection) (cmt : string) =
      out <<. ": " + cmt + ES_EOL

    let event_type (out : Connection) (event_type : string) =
      out <<. "event: " + event_type + ES_EOL

    let data (out : Connection) (data : string) =
      out <<. "data: " + data + ES_EOL

    let es_id (out : Connection) (last_event_id : string) =
      out <<. "id: " + last_event_id + ES_EOL

    let retry (out : Connection) (retry : uint32) =
      out <<. "retry: " + (string retry) + ES_EOL

    type Message =
      { id       : string
        data     : string
        ``type`` : string option }

    let mk_message id data =
      { id = id; data = data; ``type`` = None }

    let mk_message' id data typ =
      { id = id; data = data; ``type`` = Some typ }

    let send (out : Connection) (msg : Message) =
      socket {
        do! msg.id |> es_id out
        match msg.``type`` with
        | Some x -> do! x |> event_type out
        | None   -> ()
        do! msg.data |> data out
        return! dispatch out }

    let private hand_shake' f (out : Connection) =
      socket {
        // resp.SendChunked       <- false
        // Buggy Internet Explorer; 2kB of comment padding for IE
        do! String.replicate 2000 " " |> comment out
        do! 2000u |> retry out
        return! f out }

    let hand_shake f ({ request = req } as ctx : HttpContext) =
      { ctx with
          response =
            { ctx.response with
                status = Codes.HTTP_200
                headers =
                     ("Content-Type",                "text/event-stream; charset=utf-8")
                  :: ("Cache-Control",               "no-cache")
                  :: ("Access-Control-Allow-Origin", "*")
                  :: []
                content = SocketTask (hand_shake' f)
                //chunked = false
            }
      }
      |> succeed

  module Authentication =

    open RequestErrors
    open Suave.Utils

    let internal parse_authentication_token (token : string) =
      let parts = token.Split (' ')
      let enc = parts.[1].Trim()
      let decoded = ASCII.base64_decode enc
      let indexOfColon = decoded.IndexOf(':')
      (parts.[0].ToLower(), decoded.Substring(0,indexOfColon), decoded.Substring(indexOfColon+1))

    let authenticate_basic f (ctx : HttpContext) =
      let p = ctx.request
      let headers = p.headers
      match headers %% "authorization" with
      | Some header ->
        let (typ, username, password) = parse_authentication_token header
        if (typ.Equals("basic")) && f (username, password) then
          fail
        else
          challenge { ctx with user_state = ctx.user_state.Add("user_name",username) }
      | None ->
        challenge ctx