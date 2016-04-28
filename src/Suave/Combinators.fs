namespace Suave

open Suave.Operators
open Suave.Sockets

module Response =

  open System
  open System.IO

  let response (statusCode : HttpCode) (cnt : byte []) =
    fun (ctx : HttpContext) ->
      let response = 
        { ctx.response with status = statusCode.status; content = Bytes cnt }
      { ctx with response = response } |> succeed

module Writers =
  // TODO: transform into a set of lenses with Aether
  // @ https://github.com/xyncro/aether and move closer to HttpContext.

  open System
  open Suave.Utils

  let setStatus (s : HttpCode) : WebPart = 
    fun ctx ->
      { ctx with response = { ctx.response with status = s.status }}
      |> succeed

  let setStatusCode (code : int) : WebPart = 
    fun ctx ->
      { ctx with response = { ctx.response with status = { ctx.response.status with code = code }}}
      |> succeed

  let setStatusReason (reason : string) : WebPart = 
    fun ctx ->
      { ctx with response = { ctx.response with status = { ctx.response.status with reason = reason }}}
      |> succeed

  let setHeader key value (ctx : HttpContext) =
    let headers' =
      (key, value)
      :: (ctx.response.headers |> List.filter (fst >> String.equalsCaseInsensitve key >> not))

    { ctx with response = { ctx.response with headers = headers' } }
    |> succeed

  let setHeaderValue key value (ctx : HttpContext) =
    let headers' =
      let rec iter = function
        | [] ->
          [key, value]

        | (existingKey, existingValue) :: hs
          when String.equalsCaseInsensitve key existingKey ->
          // Deliberately side-stepping lowercase-uppercase and just doing a F# (=)
          // compare.
          // This can be changed via PR/discussion.
          if Array.exists ((=) value) (String.splita ',' existingValue) then
            ctx.response.headers
          else
            (key, String.Concat [| existingValue; ","; value |]) :: hs

        | h :: hs ->
          h :: iter hs

      iter ctx.response.headers

    { ctx with response = { ctx.response with headers = headers' } }
    |> succeed

  let addHeader key value (ctx : HttpContext) =
    let headers' =
       List.append ctx.response.headers [key, value]

    { ctx with response = { ctx.response with headers = headers' } }
    |> succeed

  let setUserData key value (ctx : HttpContext) =
    { ctx with userState = ctx.userState |> Map.add key (box value) }
    |> succeed

  let unsetUserData key (ctx : HttpContext) =
    { ctx with userState = ctx.userState |> Map.remove key }
    |> succeed

  // TODO: I'm not sure about having MIME types in the Writers module
  let mkMimeType name compression =
    { name=name; compression=compression } |> Some

  let defaultMimeTypesMap = function
    | ".bmp" -> mkMimeType "image/bmp" false
    | ".css" -> mkMimeType "text/css" true
    | ".gif" -> mkMimeType "image/gif" false
    | ".png" -> mkMimeType "image/png" false
    | ".svg" -> mkMimeType "image/svg+xml" true
    | ".ico" -> mkMimeType "image/x-icon" false
    | ".xml" -> mkMimeType "application/xml" true
    | ".js"  -> mkMimeType "application/javascript" true
    | ".json" -> mkMimeType "application/json" true
    | ".map"  -> mkMimeType "application/json" true
    | ".htm"
    | ".html" -> mkMimeType "text/html" true
    | ".jpe"
    | ".jpeg"
    | ".jpg" -> mkMimeType "image/jpeg" false
    | ".exe" -> mkMimeType "application/exe" false
    | ".txt" -> mkMimeType "text/plain" true
    | ".ttf" -> mkMimeType "application/x-font-ttf" true
    | ".otf" -> mkMimeType "application/font-sfnt" true
    | ".woff" -> mkMimeType "application/font-woff" false
    | ".eot" -> mkMimeType "application/vnd.ms-fontobject" false
    | _      -> None

  let setMimeType t = setHeader "Content-Type" t

// 1xx
module Intermediate =

  open System
  open Response

  let CONTINUE : WebPart =
    response HTTP_100 [||]

  let SWITCHING_PROTO : WebPart =
    response HTTP_101 [||]

// 2xx
module Successful =
    
  open Suave.Utils
  open Response
    
  let ok s : WebPart = 
    fun ctx -> { ctx with response = { ctx.response with status = HTTP_200.status; content = Bytes s }} |> succeed

  let OK a = ok (UTF8.bytes a)

  let created s = response HTTP_201 s

  let CREATED s = created (UTF8.bytes s)

  let accepted s = response HTTP_202 s

  let ACCEPTED s = accepted (UTF8.bytes s)

  let no_content : WebPart =
    fun ctx -> { ctx with response = { status = HTTP_204.status; headers = ctx.response.headers; content = Bytes [||]; writePreamble = true }} |> succeed

  let NO_CONTENT = no_content

// 3xx
module Redirection =

  open Suave.Utils
  open Response
  open Writers

  let moved_permanently location =
    setHeader "Location" location
    >=> response HTTP_301 [||]

  let MOVED_PERMANENTLY location = moved_permanently location

  let found location =
    setHeader "Location" location
    >=> response HTTP_302 [||]

  let FOUND location = found location

  let redirect url =
    setHeader "Location" url
    >=> setHeader "Content-Type" "text/html; charset=utf-8"
    >=> response HTTP_302 (
      UTF8.bytes(sprintf "<html>
  <body>
    <a href=\"%s\">%s</a>
  </body>
</html>"
      url HTTP_302.message))

  let not_modified : WebPart =
    fun ctx -> { ctx with response = {status = HTTP_304.status; headers = []; content = Bytes [||]; writePreamble = true }} |> succeed

  let NOT_MODIFIED : WebPart =
    not_modified

// 4xx
module RequestErrors =

  open Suave.Utils
  open Response
  open Writers

  let bad_request s = response HTTP_400 s

  let BAD_REQUEST s = bad_request (UTF8.bytes s)

  /// 401: see http://stackoverflow.com/questions/3297048/403-forbidden-vs-401-unauthorized-http-responses/12675357
  let unauthorized s =
    setHeader "WWW-Authenticate" "Basic realm=\"protected\""
    >=> response HTTP_401 s

  let UNAUTHORIZED s = unauthorized (UTF8.bytes s)

  let challenge = UNAUTHORIZED HTTP_401.message

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

  let INVALID_HTTP_VERSION = invalid_http_version (UTF8.bytes HTTP_505.message)

module Filters =
  open Suave.Utils
  open Suave.Utils.AsyncExtensions
  open Suave.Logging
  open System
  open System.Text.RegularExpressions

  module private Option =
    let iff b x =
      if b then Some x else None

  let path s (x : HttpContext) =
    async.Return (Option.iff (s = x.request.path) x)

  let pathStarts s (x : HttpContext) =
    async.Return (Option.iff (x.request.path.StartsWith s) x)

  let url x = path x

  let ``method`` (m : HttpMethod) (x : HttpContext) =
    async.Return (Option.iff (m = x.request.``method``) x)

  let isSecure (x : HttpContext) =
    async.Return (Option.iff x.runtime.matchedBinding.scheme.secure x)

  let pathRegex regex (x : HttpContext) =
    async.Return (Option.iff (Regex.IsMatch(x.request.path, regex)) x)

  let urlRegex x = pathRegex x

  let host hostname (x : HttpContext) =
    async.Return (Option.iff (String.equalsOrdinalCI x.request.clientHostTrustProxy hostname) x)

  let serverHost hostname (x : HttpContext) =
    async.Return (Option.iff (String.equalsOrdinalCI x.request.host hostname) x)

  let clientHost hostname x = host hostname x

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

  let logFormat (ctx : HttpContext) =

    let dash = function | "" | null -> "-" | x -> x
    let ci = Globalization.CultureInfo("en-US")
    let processId = System.Diagnostics.Process.GetCurrentProcess().Id.ToString()
    sprintf "%O %s %s [%s] \"%s %s %s\" %d %d"
      ctx.clientIpTrustProxy
      processId //TODO: obtain connection owner via Ident protocol
                        // Authentication.UserNameKey
      (match Map.tryFind "userName" ctx.userState with Some x -> x :?> string | None -> "-")
      (DateTime.UtcNow.ToString("dd/MMM/yyyy:hh:mm:ss %K", ci))
      (string ctx.request.``method``)
      ctx.request.url.AbsolutePath
      ctx.request.httpVersion
      ctx.response.status.code
      (match ctx.response.content with
        | Bytes bs -> bs.Length
        | _ -> 0)

  let log (logger : Logger) (formatter : HttpContext -> string) (ctx : HttpContext) =
    logger.Log LogLevel.Debug <| fun _ ->
      { trace         = ctx.request.trace
        message       = formatter ctx
        level         = LogLevel.Debug
        path          = "Suave.Http.web-requests"
        ``exception`` = None
        tsUTCTicks    = Suave.Globals.utcNow().Ticks }

    succeed ctx

  open Suave.Sscanf
  open ServerErrors

  let pathScan (pf : PrintfFormat<_,_,_,_,'t>) (h : 't ->  WebPart) : WebPart =
      
    let scan url =
      try 
        let r = sscanf pf url
        Some r
      with _ -> None

    let F (r:HttpContext) =
      match scan r.request.path with
      | Some p ->
        let part = h p
        part r 
      | None -> 
        fail
    F

  let urlScan s x = pathScan s x
          
  let timeoutWebPart (timeSpan : TimeSpan) (webPart : WebPart) : WebPart =
    fun (ctx : HttpContext) -> async {
      try
        return! Async.WithTimeout (timeSpan, webPart ctx)
      with
        | :? TimeoutException ->
          return! Response.response HttpCode.HTTP_408 (UTF8.bytes "Request Timeout") ctx
          }

/// not part of the public API at this point
module ServeResource =
  open System

  open Writers
  open Redirection
  open RequestErrors
  open Suave.Utils
  open Suave.Logging

  // If a response includes both an Expires header and a max-age directive,
  // the max-age directive overrides the Expires header, even if the Expires header is more restrictive
  // 'Cache-Control' and 'Expires' headers should be left up to the user
  let resource key exists getLast getExtension
                (send : string -> bool -> WebPart)
                ctx =
    let log =
      Log.verbose ctx.runtime.logger "Suave.Http.ServeResource.resource" TraceHeader.empty

    let sendIt name compression =
      setHeader "Last-Modified" ((getLast key : DateTime).ToString("R"))
      >=> setHeader "Vary" "Accept-Encoding"
      >=> setMimeType name
      >=> send key compression

    if exists key then
      let mimes = ctx.runtime.mimeTypesMap (getExtension key)
      match mimes with
      | Some value ->
        match ctx.request.header "if-modified-since" with
        | Choice1Of2 v ->
          match Parse.dateTime v with
          | Choice1Of2 date ->
            if getLast key > date then sendIt value.name value.compression ctx
            else NOT_MODIFIED ctx
          | Choice2Of2 parse_error -> bad_request [||] ctx
        | Choice2Of2 _ ->
          sendIt value.name value.compression ctx
      | None ->
        let ext = getExtension key
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
  open Suave.Sockets.Control

  open Response
  open Writers
  open Successful
  open Redirection
  open ServeResource

  let sendFile fileName (compression : bool) (ctx : HttpContext) =
    let writeFile file (conn, _) = socket {
      let getFs = fun path -> new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite) :> Stream
      let getLm = fun path -> FileInfo(path).LastWriteTime
      let! (encoding,fs) = Compression.transformStream file getFs getLm compression ctx.runtime.compressionFolder ctx

      match encoding with
      | Some n ->
        let! (_,conn) = asyncWriteLn (String.Concat [| "Content-Encoding: "; n.ToString() |]) conn
        let! (_,conn) = asyncWriteLn (sprintf "Content-Length: %d\r\n" (fs : Stream).Length) conn
        let! conn = flush conn
        if  ctx.request.``method`` <> HttpMethod.HEAD && fs.Length > 0L then
         do! transferStream conn fs
        return conn
      | None ->
        let! (_,conn) = asyncWriteLn (sprintf "Content-Length: %d\r\n" (fs : Stream).Length) conn
        let! conn = flush conn
        if  ctx.request.``method`` <> HttpMethod.HEAD && fs.Length > 0L then
         do! transferStream conn fs
        return conn
    }
    { ctx with
        response =
          { ctx.response with
              status = HTTP_200.status
              content = SocketTask (writeFile fileName) } }
    |> succeed

  let file fileName : WebPart =
    resource
      fileName
      (File.Exists)
      (fun name -> FileInfo(name).LastAccessTime)
      (Path.GetExtension)
      sendFile

  let resolvePath (rootPath : string) (fileName : string) =
    let fileName =
      if Path.DirectorySeparatorChar.Equals('/') then fileName
      else fileName.Replace('/', Path.DirectorySeparatorChar)
    let calculatedPath =
      Path.Combine(rootPath, fileName.TrimStart([| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |]))
      |> Path.GetFullPath
    if calculatedPath.StartsWith rootPath then
      calculatedPath
    else raise <| Exception("File canonalization issue.")

  let browseFile rootPath fileName =
    fun ({request = r; runtime = q} as h) ->
      file (resolvePath rootPath fileName) h

  let browseFileHome fileName =
    fun ({request = r; runtime = q} as h) ->
      browseFile q.homeDirectory fileName h
    
  let browse rootPath : WebPart =
    warbler (fun ctx ->
      Log.verbose ctx.runtime.logger
        "Suave.Http.Files.browse"
        TraceHeader.empty
        (sprintf "Files.browse trying file (local file url:'%s' root:'%s')"
          ctx.request.url.AbsolutePath rootPath)
      file (resolvePath rootPath ctx.request.path))

  let browseHome : WebPart =
    warbler (fun ctx -> browse ctx.runtime.homeDirectory)

  let dir rootPath (ctx : HttpContext) =
    let req = ctx.request

    let dirname = resolvePath rootPath req.path
    let result = new StringBuilder()

    let filesize  (x : FileSystemInfo) =
      if (x.Attributes ||| FileAttributes.Directory = FileAttributes.Directory) then
        String.Format("{0,-14}",System.Net.WebUtility.HtmlEncode("<DIR>"))
      else
        String.Format("{0,14}", (new FileInfo(x.FullName)).Length)

    let formatdate (t : DateTime) =
      t.ToString("MM-dd-yy") + "  " + t.ToString("hh:mmtt")

    let buildLine (x : FileSystemInfo) =
      result.Append(x.LastWriteTime.ToString() + "       " + filesize(x) + " " + x.Name + "<br/>\n")
      |> ignore

    if Directory.Exists dirname then
      let di = new DirectoryInfo(dirname)
      (di.GetFileSystemInfos()) |> Array.sortBy (fun x -> x.Name) |> Array.iter buildLine
      OK (result.ToString()) ctx
    else fail

  let dirHome ctx =
    dir ctx.runtime.homeDirectory ctx

module Embedded =
    
  open System
  open System.IO
  open System.Reflection

  open Suave.Utils
  open Suave.Sockets.Control

  open Response
  open ServeResource
  
  #if !NETSTANDARD1_5
  let defaultSourceAssembly =
    if Assembly.GetEntryAssembly() = null
    then Assembly.GetCallingAssembly()
    else Assembly.GetEntryAssembly()
  #endif

  let resources (assembly : Assembly) =
    assembly.GetManifestResourceNames()

  let lastModified (assembly : Assembly) =
    FileInfo(assembly.Location).CreationTime
    
  let sendResource (assembly : Assembly)
                    resourceName
                    (compression : bool)
                    (ctx : HttpContext) =
    let writeResource name (conn, _) = socket {
      let getFs = fun name -> assembly.GetManifestResourceStream(name)
      let getLm = fun _ -> lastModified assembly
      let! encoding,fs = Compression.transformStream name getFs getLm compression ctx.runtime.compressionFolder ctx

      match encoding with
      | Some n ->
        let! (_,conn) =  asyncWriteLn (String.Concat [| "Content-Encoding: "; n.ToString() |]) conn
        let! (_,conn) = asyncWriteLn (sprintf "Content-Length: %d\r\n" (fs: Stream).Length) conn
        let! conn = flush conn
        if ctx.request.``method`` <> HttpMethod.HEAD && fs.Length > 0L then
          do! transferStream conn fs
        fs.Dispose()
        return conn
      | None ->
        let! (_,conn) = asyncWriteLn (sprintf "Content-Length: %d\r\n" (fs: Stream).Length) conn
        let! conn = flush conn
        if ctx.request.``method`` <> HttpMethod.HEAD && fs.Length > 0L then
          do! transferStream conn fs
        fs.Dispose()
        return conn
    }
    { ctx with
        response =
          { ctx.response with
              status = HTTP_200.status
              content = SocketTask (writeResource resourceName) }}
    |> succeed

  #if !NETSTANDARD1_5
  let sendResourceFromDefaultAssembly resourceName compression =
    sendResource defaultSourceAssembly resourceName compression
  #endif

  let resource assembly name =
    resource
      name
      (fun name -> resources assembly |> Array.exists ((=) name))
      (fun _ -> lastModified assembly)
      (Path.GetExtension)
      (sendResource assembly)
  
  #if !NETSTANDARD1_5
  let resourceFromDefaultAssembly name =
    resource defaultSourceAssembly name
  #endif

  let browse assembly =
    warbler (fun ctx -> resource assembly (ctx.request.path.TrimStart [|'/'|]))

  #if !NETSTANDARD1_5
  let browseDefaultAsssembly =
    browse defaultSourceAssembly
  #endif

// See www.w3.org/TR/eventsource/#event-stream-interpretation
module EventSource =
  open System
  open Suave
  open Suave.Sockets
  open Suave.Sockets.Control
  open Suave.Sockets.Connection
  open Suave.Utils

  [<Literal>]
  let private ES_EOL = "\n"

  let private ES_EOL_S = ArraySegment<_>(UTF8.bytes ES_EOL, 0, 1)

  let asyncWrite (out : Connection) (data : string) =
    asyncWriteBytes out (UTF8.bytes data)

  let (<<.) (out : Connection) (data : string) =
    asyncWriteBytes out (UTF8.bytes data)

  let dispatch (out : Connection) =
    send out ES_EOL_S

  let comment (out : Connection) (cmt : string) =
    out <<. ": " + cmt + ES_EOL

  let eventType (out : Connection) (evType : string) =
    out <<. "event: " + evType + ES_EOL

  let data (out : Connection) (text : string) =
    out <<. "data: " + text + ES_EOL

  let esId (out : Connection) (lastEventId : string) =
    out <<. "id: " + lastEventId + ES_EOL

  let retry (out : Connection) (retry : uint32) =
    out <<. "retry: " + (string retry) + ES_EOL

  type Message =
    { /// The event ID to set the EventSource object's last event ID value.
      id       : string
      /// The data field for the message. When the EventSource receives multiple consecutive lines that begin with data:, it will concatenate them, inserting a newline character between each one. Trailing newlines are removed.
      data     : string
      /// The event's type. If this is specified, an event will be dispatched on the browser to the listener for the specified event name; the web site source code should use addEventListener() to listen for named events. The onmessage handler is called if no event name is specified for a message.
      ``type`` : string option }

  let mkMessage id data =
    { id = id; data = data; ``type`` = None }

  let mkMessageType id data typ =
    { id = id; data = data; ``type`` = Some typ }

  let send (out : Connection) (msg : Message) =
    socket {
      do! msg.id |> esId out
      match msg.``type`` with
      | Some x -> do! x |> eventType out
      | None   -> ()
      do! msg.data |> data out
      return! dispatch out
    }

  let private handShakeAux f (out : Connection, _) =
    socket {
      let! (_,out) = asyncWriteLn "" out// newline after headers

      // Buggy Internet Explorer; 2kB of comment padding for IE
      do! String.replicate 2000 " " |> comment out
      do! 2000u |> retry out
      return! f out
    }

  let handShake f (ctx : HttpContext) =
    { ctx with
        response =
          { ctx.response with
              status = HTTP_200.status
              headers =
                ("Content-Type",                "text/event-stream; charset=utf-8")
                :: ("Cache-Control",               "no-cache")
                :: ("Access-Control-Allow-Origin", "*")
                // http://wiki.nginx.org/X-accel#X-Accel-Buffering – hard to find
                // also see http://wiki.nginx.org/HttpProxyModule#proxy_buffering
                :: ("X-Accel-Buffering",           "no")
                :: []
              content = SocketTask (handShakeAux f)
          }
    }
    |> succeed

module Control =

  let CLOSE (ctx : HttpContext) =
    { ctx with
        response =
          { ctx.response with
              content = NullContent
              writePreamble = false
          }
        request =
          { ctx.request with
              headers = [ "connection", "close" ]
          }
    }
    |> succeed

module CORS =
  
  open System
  open Successful
  open Utils

  [<Literal>]
  let Origin = "Origin"

  [<Literal>]
  let AccessControlRequestMethod = "Access-Control-Request-Method"

  [<Literal>]
  let AccessControlRequestHeaders = "Access-Control-Request-Headers"
  [<Literal>]
  let AccessControlAllowOrigin = "Access-Control-Allow-Origin"

  [<Literal>]
  let AccessControlAllowMethods = "Access-Control-Allow-Methods"

  [<Literal>]
  let AccessControlAllowHeaders = "Access-Control-Allow-Headers"
  [<Literal>]
  let AccessControlAllowCredentials = "Access-Control-Allow-Credentials"

  [<Literal>]
  let AccessControlExposeHeaders = "Access-Control-Expose-Headers"
  [<Literal>]
  let AccessControlMaxAge = "Access-Control-Max-Age"

  [<RequireQualifiedAccess>]
  type InclusiveOption<'T> =
    | None
    | Some of 'T
    | All

  /// The configuration values for CORS
  type CORSConfig =
    { /// The list of allowed Uri(s) for requests.
      allowedUris             : InclusiveOption<string list>

      /// The list of allowed HttpMethods for the request.
      allowedMethods          : InclusiveOption<HttpMethod list>
      
      /// Allow cookies? This is sent in the AccessControlAllowCredentials header.
      allowCookies            : bool

      /// Should response headers be exposed to the client? This is sent in AccessControlExposeHeaders header. 
      exposeHeaders           : bool
      
      /// Max age in seconds the user agent is allowed to cache the result of the request.
      maxAge                  : int option }
    
    static member allowedUris_           = Property<CORSConfig,_> (fun x -> x.allowedUris)           (fun v x -> { x with allowedUris = v })
    static member allowedMethods_        = Property<CORSConfig,_> (fun x -> x.allowedMethods)        (fun v x -> { x with allowedMethods = v })
    static member allowCookies_          = Property<CORSConfig,_> (fun x -> x.allowCookies)          (fun v x -> { x with allowCookies = v })
    static member exposeHeaders_         = Property<CORSConfig,_> (fun x -> x.exposeHeaders)         (fun v x -> { x with exposeHeaders = v })
    static member maxAge_                = Property<CORSConfig,_> (fun x -> x.maxAge)                (fun v x -> { x with maxAge = v })

  let private isAllowedOrigin config (value : string) = 
    match config.allowedUris with
    | InclusiveOption.All ->
      true

    | InclusiveOption.None ->
      false

    | InclusiveOption.Some uris ->
      uris
      |> List.exists (String.equalsCaseInsensitve value)

  let private setMaxAgeHeader config =
    match config.maxAge with
    | None ->
      succeed

    | Some age ->
      Writers.setHeader AccessControlMaxAge (age.ToString())

  let private setAllowCredentialsHeader config = 
    Writers.setHeader AccessControlAllowCredentials (config.allowCookies.ToString())

  let private setAllowMethodsHeader config value =
    match config.allowedMethods with
    | InclusiveOption.None ->
      succeed

    | InclusiveOption.All ->
      Writers.setHeader AccessControlAllowMethods "*"

    | InclusiveOption.Some (m :: ms) -> 
      let exists = m.ToString() = value || List.exists (fun m -> m.ToString() = value) ms
      if exists then
        let header = sprintf "%s,%s" (m.ToString()) (ms |> Seq.map (fun i -> i.ToString()) |> String.concat( ", "))
        Writers.setHeader AccessControlAllowMethods header
      else
        succeed

    | InclusiveOption.Some ([]) ->
      succeed

  let private setAllowOriginHeader value = 
    Writers.setHeader AccessControlAllowOrigin value

  let private setExposeHeadersHeader config =
    Writers.setHeader AccessControlExposeHeaders (config.exposeHeaders.ToString())

  let cors (config : CORSConfig) : WebPart =
    fun (ctx : HttpContext) ->
      let req = ctx.request
      match req.header (Origin.ToLowerInvariant()) with
      | Choice1Of2 originValue -> // CORS request
        let allowedOrigin = isAllowedOrigin config originValue
        match req.``method`` with
        | HttpMethod.OPTIONS ->
          match req.header (AccessControlRequestMethod.ToLowerInvariant()) with
          | Choice1Of2 requestMethodHeaderValue -> // Preflight request
            // Does the request have an Access-Control-Request-Headers header? If so, validate. If not, proceed.
            let setAccessControlRequestHeaders =
              match Headers.getAll req.headers (AccessControlRequestHeaders.ToLowerInvariant()) with
              | Choice1Of2 list ->
                Writers.setHeader AccessControlAllowHeaders (list |> String.concat ", ")
              | Choice2Of2 _ ->
                succeed

            if allowedOrigin then
              let composed =
                setAllowMethodsHeader config requestMethodHeaderValue
                >=> setAccessControlRequestHeaders
                >=> setMaxAgeHeader config
                >=> setAllowCredentialsHeader config
                >=> setAllowOriginHeader originValue
                >=> NO_CONTENT
              composed ctx
            else
              succeed ctx

          | Choice2Of2 _ ->
            succeed ctx

        | _ ->
          if allowedOrigin then
            let composed =
              setExposeHeadersHeader config
              >=> setAllowCredentialsHeader config
              >=> setAllowOriginHeader originValue
              >=> setAllowMethodsHeader config "*"
            composed ctx
          else
            succeed ctx // No headers will be sent. Browser will deny.

      | Choice2Of2 _ ->
        succeed ctx // Not a CORS request


  let defaultCORSConfig =
    { allowedUris           = InclusiveOption.All
      allowedMethods        = InclusiveOption.All
      allowCookies          = true
      exposeHeaders         = true
      maxAge                = None }
