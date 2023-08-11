namespace Suave

open Suave.Operators
open Suave.Sockets

module Response =

  let response (statusCode : HttpCode) (content : byte []) =
    fun (ctx : HttpContext) ->
      let response =
        { ctx.response with status = statusCode.status; content = Bytes content }
      { ctx with response = response } |> succeed

module Writers =

  open System

  let setStatus (status : HttpCode) : WebPart =
    fun ctx ->
      { ctx with response = { ctx.response with status = status.status }}
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
      :: (ctx.response.headers |> List.filter (fst >> String.equalsCaseInsensitive key >> not))

    { ctx with response = { ctx.response with headers = headers' } }
    |> succeed

  let setHeaderValue key value (ctx : HttpContext) =
    let headers' =
      let rec iter = function
        | [] ->
          [key, value]

        | (existingKey, existingValue) :: hs
          when String.equalsCaseInsensitive key existingKey ->
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
    if ctx.userState.ContainsKey key then
      ctx.userState.[key] <- box value
    else
      ctx.userState.Add(key, box value)
    succeed ctx

  let unsetUserData key (ctx : HttpContext) =
    ctx.userState.Remove(key) |> ignore
    succeed ctx

  // TODO: I'm not sure about having MIME types in the Writers module
  let createMimeType name compression =
    { name=name; compression=compression } |> Some

  let defaultMimeTypesMap (ext:string) =
    match ext with
    | ".bmp" -> createMimeType "image/bmp" false
    | ".css" -> createMimeType "text/css" true
    | ".gif" -> createMimeType "image/gif" false
    | ".png" -> createMimeType "image/png" false
    | ".svg" -> createMimeType "image/svg+xml" true
    | ".ico" -> createMimeType "image/x-icon" false
    | ".xml" -> createMimeType "application/xml" true
    | ".js"  -> createMimeType "application/javascript" true
    | ".json" -> createMimeType "application/json" true
    | ".map"  -> createMimeType "application/json" true
    | ".htm"
    | ".html" -> createMimeType "text/html" true
    | ".jpe"
    | ".jpeg"
    | ".jpg" -> createMimeType "image/jpeg" false
    | ".exe" -> createMimeType "application/exe" false
    | ".pdf" -> createMimeType "application/pdf" false
    | ".txt" -> createMimeType "text/plain" true
    | ".ttf" -> createMimeType "application/x-font-ttf" true
    | ".otf" -> createMimeType "application/font-sfnt" true
    | ".woff" -> createMimeType "application/font-woff" false
    | ".woff2" -> createMimeType "application/font-woff2" false
    | ".eot" -> createMimeType "application/vnd.ms-fontobject" false
    | _      -> None

  let setMimeType mimeType = setHeader "Content-Type" mimeType

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
  open System.Text
  open Suave.Utils
  open Response

  let ok bytes : WebPart =
    fun ctx -> { ctx with response = { ctx.response with status = HTTP_200.status; content = Bytes bytes }} |> succeed

  let OK (body:string) = ok (Encoding.UTF8.GetBytes body)

  let created bytes = response HTTP_201 bytes

  let CREATED (body:string) = created (Encoding.UTF8.GetBytes body)

  let accepted bytes = response HTTP_202 bytes

  let ACCEPTED (body:string) = accepted (Encoding.UTF8.GetBytes body)

  let no_content : WebPart =
    fun ctx -> { ctx with response = { status = HTTP_204.status; headers = ctx.response.headers; content = Bytes [||]; writePreamble = true }} |> succeed

  let NO_CONTENT = no_content

// 3xx
module Redirection =
  open System.Text
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

  let redirect location =
    setHeader "Location" location
    >=> setHeader "Content-Type" "text/html; charset=utf-8"
    >=> response HTTP_302 (
      Encoding.UTF8.GetBytes("<html>
  <body>
    <a href=\"" + location  + "\">" + HTTP_302.message + "</a>
  </body>
</html>"))

  let see_other location =
    setHeader "Location" location
    >=> setHeader "Content-Type" "text/html; charset=utf-8"
    >=> response HTTP_303 (
      Encoding.UTF8.GetBytes("<html>
  <body>
    <a href=\"" + location + "\">" + HTTP_303.message + "</a>
  </body>
</html>"))

  let not_modified : WebPart =
    fun ctx -> { ctx with response = {status = HTTP_304.status; headers = []; content = Bytes [||]; writePreamble = true }} |> succeed

  let NOT_MODIFIED : WebPart =
    not_modified

// 4xx
module RequestErrors =
  open System.Text
  open Response
  open Writers

  let bad_request bytes = response HTTP_400 bytes

  let BAD_REQUEST (body:string) = bad_request (Encoding.UTF8.GetBytes body)

  /// 401: see http://stackoverflow.com/questions/3297048/403-forbidden-vs-401-unauthorized-http-responses/12675357
  let unauthorized bytes =
    setHeader "WWW-Authenticate" "Basic realm=\"protected\""
    >=> response HTTP_401 bytes

  let UNAUTHORIZED (body:string) = unauthorized (Encoding.UTF8.GetBytes body)

  let challenge = UNAUTHORIZED HTTP_401.message

  let forbidden bytes = response HTTP_403 bytes

  let FORBIDDEN (body:string) = forbidden (Encoding.UTF8.GetBytes body)

  let not_found bytes = response HTTP_404 bytes

  let NOT_FOUND (body:string) = not_found (Encoding.UTF8.GetBytes body)

  let method_not_allowed bytes = response HTTP_405 bytes

  let METHOD_NOT_ALLOWED (body:string) = method_not_allowed (Encoding.UTF8.GetBytes body)

  let not_acceptable bytes = response HTTP_406 bytes

  let NOT_ACCEPTABLE (body:string) = not_acceptable (Encoding.UTF8.GetBytes body)

  let request_timeout = response HTTP_408 [||]

  // all-caps req.timeout elided intentionally, as nothing can be passed to
  // a writing client

  let conflict bytes = response HTTP_409 bytes

  let CONFLICT (body:string) = conflict (Encoding.UTF8.GetBytes body)

  let gone bytes = response HTTP_410 bytes

  let GONE (body:string) = gone (Encoding.UTF8.GetBytes body)

  let unsupported_media_type bytes = response HTTP_415 bytes

  let UNSUPPORTED_MEDIA_TYPE (body:string) = unsupported_media_type (Encoding.UTF8.GetBytes body)

  let unprocessable_entity bytes = response HTTP_422 bytes

  let UNPROCESSABLE_ENTITY (body:string) = unprocessable_entity (Encoding.UTF8.GetBytes body)

  let precondition_required bytes = response HTTP_428 bytes

  let PRECONDITION_REQUIRED (body:string) = precondition_required (Encoding.UTF8.GetBytes body)

  let too_many_requests bytes = response HTTP_429 bytes

  let TOO_MANY_REQUESTS (body:string) = too_many_requests (Encoding.UTF8.GetBytes body)

module ServerErrors =

  open System.Text
  open Response

  let internal_error bytes = response HTTP_500 bytes

  let INTERNAL_ERROR (body:string) = internal_error (Encoding.UTF8.GetBytes body)

  let not_implemented bytes = response HTTP_501 bytes

  let NOT_IMPLEMENTED (body:string) = not_implemented (Encoding.UTF8.GetBytes body)

  let bad_gateway bytes = response HTTP_502 bytes

  let BAD_GATEWAY (body:string) = bad_gateway (Encoding.UTF8.GetBytes body)

  let service_unavailable bytes = response HTTP_503 bytes

  let SERVICE_UNAVAILABLE (body:string) = service_unavailable (Encoding.UTF8.GetBytes body)

  let gateway_timeout bytes = response HTTP_504 bytes

  let GATEWAY_TIMEOUT (body:string) = gateway_timeout (Encoding.UTF8.GetBytes body)

  let invalid_http_version bytes = response HTTP_505 bytes

  let INVALID_HTTP_VERSION = invalid_http_version (Encoding.UTF8.GetBytes HTTP_505.message)

module Filters =
  open Suave.Utils.AsyncExtensions
  open Suave.Logging
  open System
  open System.Text
  open System.Text.RegularExpressions

  module private Option =
    let iff b x =
      if b then Some x else None

  let path pathAfterDomain (x : HttpContext) =
    async.Return (Option.iff (pathAfterDomain = x.request.path) x)

  let pathCi pathAfterDomain (x : HttpContext) =
    async.Return (Option.iff (String.Equals(pathAfterDomain, x.request.path, StringComparison.CurrentCultureIgnoreCase)) x)

  let pathStarts (pathAfterDomainSubstr:string) (x : HttpContext) =
    async.Return (Option.iff (x.request.path.StartsWith pathAfterDomainSubstr) x)

  let pathStartsCi pathAfterDomainSubstr (x : HttpContext) =
    async.Return (Option.iff (x.request.path.StartsWith (pathAfterDomainSubstr, StringComparison.CurrentCultureIgnoreCase)) x)

  let url x = path x

  let ``method`` (method : HttpMethod) (x : HttpContext) =
    async.Return (Option.iff (method = x.request.``method``) x)

  let isSecure (x : HttpContext) =
    async.Return (Option.iff x.runtime.matchedBinding.scheme.secure x)

  let hasFlag flag (ctx : HttpContext) =
    if ctx.request.queryFlag flag then succeed ctx else fail

  let pathRegex pathAfterDomainRegex (x : HttpContext) =
    async.Return (Option.iff (Regex.IsMatch(x.request.path, pathAfterDomainRegex)) x)

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

  let getUserName (ctx : HttpContext) =
    match ctx.userState.TryGetValue "userName"  with true, x -> x :?> string | false, _ -> "-"

  let logFormat (ctx : HttpContext) =

    let dash = function | "" | null -> "-" | x -> x
    let ci = Globalization.CultureInfo("en-US")
    let processId = System.Diagnostics.Process.GetCurrentProcess().Id.ToString()
    ctx.clientIpTrustProxy.ToString() + " " +
    processId + " " + //TODO: obtain connection owner via Ident protocol
                       // Authentication.UserNameKey
    (getUserName ctx) + " [" +
    (DateTime.UtcNow.ToString("dd/MMM/yyyy:hh:mm:ss %K", ci)) + "] \"" +
    (string ctx.request.``method``) + " " +
    ctx.request.url.AbsolutePath + " " +
    ctx.request.httpVersion + "\" " +
    ctx.response.status.code.ToString() + " " +
    (match ctx.response.content with
    | Bytes bs -> bs.Length.ToString()
    | _ -> "0")

  let logFormatStructured (ctx : HttpContext) =
    let fieldList : (string*obj) list = [
      "clientIp", box ctx.clientIpTrustProxy
      "processId", box (System.Diagnostics.Process.GetCurrentProcess().Id.ToString())
      "userName", box (getUserName ctx)
      "utcNow", box DateTime.UtcNow
      "requestMethod", box (ctx.request.``method``)
      "requestUrlPath", box (ctx.request.url.AbsolutePath)
      "httpVersion", box ctx.request.httpVersion
      "httpStatusCode", box ctx.response.status.code
      "responseContentLength", box (match ctx.response.content with | Bytes bs -> bs.Length | _ -> 0)
    ]
    "{clientIp} {processId} {userName} [{utcNow:dd/MMM/yyyy:hh:mm:ss %K}] \"{requestMethod} {requestUrlPath} {httpVersion}\" {httpStatusCode} {responseContentLength}", fieldList |> Map

  let logWithLevel (level : LogLevel) (logger : Logger) (messageFun : HttpContext -> string) (ctx : HttpContext) =
    async{
      logger.log level (fun _ ->
        { value         = Event (messageFun ctx)
          level         = level
          name          = [| "Suave"; "Http"; "requests" |]
          fields        = Map.empty
          timestamp     = Suave.Logging.Global.timestamp() })
      return Some ctx }

  let logWithLevelStructured (level : LogLevel) (logger : Logger) (messageFun : HttpContext -> (string * Map<string,obj>)) (ctx : HttpContext) =
    async{
      logger.log level (fun _ ->
        let template, fields = messageFun ctx
        { value         = Event template
          level         = level
          name          = [| "Suave"; "Http"; "requests" |]
          fields        = fields
          timestamp     = Suave.Logging.Global.timestamp() })
      return Some ctx }

  let logStructured (logger : Logger) (messageFun : HttpContext -> (string * Map<string,obj>)) =
    logWithLevelStructured LogLevel.Info logger messageFun

  let log (logger : Logger) (messageFun : HttpContext -> string) =
    logWithLevel LogLevel.Info logger messageFun

  open Suave.Sscanf

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

  let pathScanCi (format : PrintfFormat<_,_,_,_,'t>) (handler : 't ->  WebPart) : WebPart =
    let scan path =
      try
        let extract = sscanfci format path
        Some extract
      with _ ->
        None

    let part (context:HttpContext) =
      match scan context.request.path with
      | Some extract ->
        handler extract context
      | None ->
        fail
    part

  let urlScan s x = pathScan s x
  let urlScanCi s x = pathScanCi s x

  let timeoutWebPart (timeout : TimeSpan) (child : WebPart) : WebPart =
    fun (ctx : HttpContext) -> async {
      try
        return! Async.WithTimeout (timeout, child ctx)
      with
        | :? TimeoutException ->
          return! Response.response HttpCode.HTTP_408 (Encoding.UTF8.GetBytes "Request Timeout") ctx
          }

/// not part of the public API at this point
module ServeResource =

  open System

  open Writers
  open Redirection
  open RequestErrors
  open Suave.Utils
  open Suave.Logging
  open Suave.Logging.Message

  // If a response includes both an Expires header and a max-age directive,
  // the max-age directive overrides the Expires header, even if the Expires header is more restrictive
  // 'Cache-Control' and 'Expires' headers should be left up to the user
  let resource key exists getLast getExtension
                (send : string -> bool -> WebPart)
                ctx =
    let log =
      event Verbose
      >> setSingleName "Suave.Http.ServeResource.resource"
      >> ctx.runtime.logger.logSimple

    let sendIt name compression =
      setHeader "Last-Modified" ((getLast key : DateTimeOffset).ToString("R"))
      >=> setHeader "Vary" "Accept-Encoding"
      >=> setMimeType name
      >=> send key compression

    if exists key then
      let mimes = ctx.runtime.mimeTypesMap (getExtension key)
      match mimes with
      | Some value ->
        match ctx.request.header "if-modified-since" with
        | Choice1Of2 v ->
          match Parse.dateTimeOffset v with
          | Choice1Of2 date ->
            let lm = getLast key
            // RFC1123 is only precise to the second so the comparison must be done with this precision
            let lmSeconds = new DateTimeOffset(lm.Year, lm.Month, lm.Day, lm.Hour, lm.Minute, lm.Second, lm.Offset)
            if lmSeconds > date then sendIt value.name value.compression ctx
            else NOT_MODIFIED ctx
          | Choice2Of2 _parse_error -> bad_request [||] ctx
        | Choice2Of2 _ ->
          sendIt value.name value.compression ctx
      | None ->
        let ext = getExtension key
        log ("failed to find matching mime for ext '" + ext + "'")
        fail
    else
      log ("failed to find resource by key '" + key + "'")
      fail

module ContentRange =
  open System
  open System.IO
  open Suave.Utils

  let parseContentRange (input:string) =
    let contentUnit = input.Split([|' '; '='|], 2)
    let rangeArray = contentUnit.[1].Split([|'-'|])
    let start = int64 rangeArray.[0]
    let finish = if Int64.TryParse (rangeArray.[1], ref 0L) then Some <| int64 rangeArray.[1] else None
    start, finish

  let (|ContentRange|_|) (context:HttpContext) =
    match context.request.header "range" with
    | Choice1Of2 rangeValue -> Some <| parseContentRange rangeValue
    | Choice2Of2 _ -> None

  let getFileStream (ctx:HttpContext) path =
    let fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite) :> Stream
    match ctx with
    | ContentRange (start, finish) ->
      let length = finish |> Option.bind (fun finish -> Some (finish - start))
      new RangedStream(fs, start, length, true) :> Stream, start, fs.Length, HTTP_206.status
    | _ -> fs, 0L, fs.Length, HTTP_200.status

module Files =

  open System
  open System.IO
  open System.Text

  open Suave.Utils
  open Suave.Logging
  open Suave.Logging.Message
  open Suave.Sockets.Control

  open Response
  open Writers
  open Successful
  open Redirection
  open ServeResource
  open ContentRange

  let sendFile fileName (compression : bool) (ctx : HttpContext) =
    let writeFile file =
      let fs, start, total, status = getFileStream ctx file
      fun (conn:Connection, _) -> socket {
        let getLm = fun path -> FileInfo(path).LastWriteTime
        let! (encoding,fs) = Compression.transformStream file fs getLm compression ctx.runtime.compressionFolder ctx
        let finish = start + fs.Length - 1L
        try
          try
            match encoding with
            | Some n ->
              do! conn.asyncWriteLn ("Content-Range: bytes " + start.ToString() + "-" + finish.ToString() + "/*")
              do! conn.asyncWriteLn (String.Concat [| "Content-Encoding: "; n.ToString() |])
              do! conn.asyncWriteLn ("Content-Length: " + (fs : Stream).Length.ToString() + "\r\n")
              do! conn.flush ()
              if ctx.request.``method`` <> HttpMethod.HEAD && fs.Length > 0L then
                do! transferStream conn fs
            | None ->
              do! conn.asyncWriteLn ("Content-Range: bytes " + start.ToString() + "-" + finish.ToString() + "/" + total.ToString())
              do! conn.asyncWriteLn ("Content-Length: " + (fs : Stream).Length.ToString() + "\r\n")
              do! conn.flush()
              if ctx.request.``method`` <> HttpMethod.HEAD && fs.Length > 0L then
                do! transferStream conn fs
          with ex ->
            raise ex
        finally
          fs.Dispose()
      }, status
    let task, status = writeFile fileName
    { ctx with
        response =
          { ctx.response with
              status = status
              content = SocketTask task } }
    |> succeed

  let file fileName : WebPart =
    resource
      fileName
      (File.Exists)
      (fun name -> new DateTimeOffset(FileInfo(name).LastWriteTime))
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
      ctx.runtime.logger.verbose (
        eventX "Files.browser trying {localFileUrl} at {rootPath}"
        >> setFieldValue "localFileUrl" ctx.request.url.AbsolutePath
        >> setFieldValue "rootPath" rootPath
        >> setSingleName "Suave.Http.Files.browse")
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
  open System.Xml
  open System.Xml.Linq

  open Suave.Utils
  open Suave.Utils.Option.Operators
  open Suave.Sockets.Control

  open Response
  open ServeResource

  let defaultSourceAssembly =
    if Assembly.GetEntryAssembly() = null
    then Assembly.GetCallingAssembly()
    else Assembly.GetEntryAssembly()

  let resources (assembly : Assembly) =
    assembly.GetManifestResourceNames()

  let lastModified (assembly : Assembly) =
    // Assembly.Location might not be available, such as in single-file deployments.
    if String.IsNullOrEmpty assembly.Location then
      Diagnostics.Process.GetCurrentProcess().StartTime
    else
      FileInfo(assembly.Location).CreationTime

  let sendResource (source : Assembly)
                    resourceName
                    (compression : bool)
                    (ctx : HttpContext) =
    let writeResource name (conn:Connection, _) = socket {
      let fs = source.GetManifestResourceStream(name)
      let getLm = fun _ -> lastModified source
      let! encoding,fs = Compression.transformStream name fs getLm compression ctx.runtime.compressionFolder ctx
      match encoding with
      | Some n ->
        do! conn.asyncWriteLn (String.Concat [| "Content-Encoding: "; n.ToString() |])
        do! conn.asyncWriteLn ("Content-Length: " + (fs: Stream).Length.ToString() + "\r\n")
        do! conn.flush()
        if ctx.request.``method`` <> HttpMethod.HEAD && fs.Length > 0L then
          do! transferStream conn fs
        fs.Dispose()
        return ()
      | None ->
        do! conn.asyncWriteLn ("Content-Length: " + (fs: Stream).Length.ToString() + "\r\n")
        do! conn.flush ()
        if ctx.request.``method`` <> HttpMethod.HEAD && fs.Length > 0L then
          do! transferStream conn fs
        fs.Dispose()
        return ()
    }
    { ctx with
        response =
          { ctx.response with
              status = HTTP_200.status
              content = SocketTask (writeResource resourceName) }}
    |> succeed

  let sendResourceFromDefaultAssembly resourceName compression =
    sendResource defaultSourceAssembly resourceName compression

  let resource source name =
    resource
      name
      (fun name -> resources source |> Array.exists ((=) name))
      (fun _ -> new DateTimeOffset(lastModified source))
      (Path.GetExtension)
      (sendResource source)

  let resourceFromDefaultAssembly name =
    resource defaultSourceAssembly name

  let browse source =
    warbler (fun ctx -> resource source (ctx.request.path.TrimStart [|'/'|]))

  let browseDefaultAsssembly =
    browse defaultSourceAssembly

  [<Literal>]
  let defaultManifestFile = "Microsoft.Extensions.FileProviders.Embedded.Manifest.xml"

  // Reference: https://github.com/dotnet/aspnetcore/blob/HEAD/src/FileProviders/Embedded/src/Manifest/ManifestParser.cs
  let browseManifest (source: Assembly) manifestFile root =
    let getElements (container: XContainer) = container.Elements()

    let ensureElement (name: string) (container: XContainer) =
      match container.Element name with
      | null -> invalidOp $"Invalid manifest format. Missing '{name}' element name"
      | elem -> elem

    let ensureText (element: XElement) =
      if Seq.isEmpty(getElements element)
        && not element.IsEmpty
        && element.Nodes() |> Seq.length |> (=) 1
        && element.FirstNode.NodeType = XmlNodeType.Text
      then
        element.Value
      else
        invalidOp $"Invalid manifest format. '{element.Name.LocalName}' must contain a text value. '{element.Value}'"

    let ensureName (element: XElement) =
      let value = element.Attribute "Name" |> Option.ofObj >>= (fun a -> Option.ofObj a.Value)
      match value with
      | None -> invalidOp $"Invalid manifest format. '{element.Name}' must contain a 'Name' attribute."
      | Some name -> name

    let getResourcePath = ensureElement "ResourcePath" >> ensureText

    let manifestFile = defaultArg manifestFile defaultManifestFile

    let manifest =
      manifestFile
      |> source.GetManifestResourceStream
      |> Option.ofObj
      |> Option.map XDocument.Load
      |> Option.defaultWith (fun () ->
           invalidOp
             $"Could not load the embedded file manifest '{manifestFile}' for assembly '{source.GetName().Name}'.")
      |> ensureElement "Manifest"

    let version =
      manifest
      |> ensureElement "ManifestVersion"
      |> ensureText

    if not (String.equalsConstantTime "1.0" version) then
      invalidOp $"The embedded file manifest '{manifestFile}' for assembly '{source.GetName().Name}'" +
        $"specifies an unsupported file format version: '{version}'."
      |> ignore

    let filesystem = ensureElement "FileSystem" manifest

    let rec buildMapping prefix mapping (element: XElement) =
      let path = prefix + (ensureName element)
      match element.Name.LocalName with
      | "File" -> Map.add path (getResourcePath element) mapping
      | "Directory" -> mapElement element (path + "/") mapping
      | invalidNode ->
        invalidOp $"Invalid manifest format. Expected a 'File' or a 'Directory' node. Got '{invalidNode}' instead."

    and mapElement element prefix mapping =
      element |> getElements |> Seq.fold (buildMapping prefix) mapping

    let resourceMapping = mapElement filesystem "" Map.empty

    let resolveResource reqPath =
      let path = [root; String.trimc '/' <!> Some reqPath] |> Seq.choose id |> String.concat "/"
      Map.tryFind path resourceMapping

    request (fun r ->
      match resolveResource r.path with
      | Some name -> resource source name
      | None -> never)

  let browseDefaultAsssemblyManifest manifestFile root = browseManifest defaultSourceAssembly manifestFile root

// See www.w3.org/TR/eventsource/#event-stream-interpretation
module EventSource =
  open System
  open Suave
  open Suave.Sockets.Control
  open Suave.Sockets.Connection
  open System.Text

  [<Literal>]
  let private ES_EOL = "\n"

  let private ES_EOL_S = Memory<_>(Encoding.UTF8.GetBytes ES_EOL, 0, 1)

  let asyncWrite (out : Connection) (data : string) =
    out.asyncWriteBytes (Encoding.UTF8.GetBytes data)

  let (<<.) (out : Connection) (data : string) =
    out.asyncWriteBytes (Encoding.UTF8.GetBytes data)

  let dispatch (out : Connection) : SocketOp<unit> =
    send out ES_EOL_S

  let comment (out : Connection) (cmt : string) =
    out <<. ": " + cmt + ES_EOL

  let eventType (out : Connection) (eventType : string) =
    out <<. "event: " + eventType + ES_EOL

  let data (out : Connection) (data : string) =
    out <<. "data: " + data + ES_EOL

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

    /// Create a new EventSource Message
    static member create id data =
      { id = id; data = data; ``type`` = None }

    static member createType id data typ =
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
      do! out.asyncWriteLn "" // newline after headers
      do! out.flush() // must flush lines buffer before using asyncWriteBytes

      // Buggy Internet Explorer; 2kB of comment padding for IE
      do! String.replicate 2000 " " |> comment out
      do! 2000u |> retry out
      return! f out
    }

  let handShake (fCont: Connection -> SocketOp<unit>) (ctx : HttpContext) =
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
              content = SocketTask (handShakeAux fCont)
          }
    }
    |> succeed

module TransferEncoding =
  open Suave
  open Suave.Sockets.Control

  let chunked (asyncWriteChunks: Connection -> SocketOp<unit>) (ctx : HttpContext) =
    let task (conn:Connection,_) = socket {
      do! conn.asyncWriteLn ""
      do! asyncWriteChunks conn
    }
    { ctx with
        response =
          {
            ctx.response with
              status = ctx.response.status
              headers = ("Transfer-Encoding", "chunked") :: ctx.response.headers
              writePreamble = true
              content = SocketTask task
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

  open Successful

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

      /// The list of response headers exposed to client. This is sent in AccessControlExposeHeaders header.
      exposeHeaders           : InclusiveOption<string list>

      /// Max age in seconds the user agent is allowed to cache the result of the request.
      maxAge                  : int option }

  let private isAllowedOrigin config (value : string) =
    match config.allowedUris with
    | InclusiveOption.All ->
      true

    | InclusiveOption.None ->
      false

    | InclusiveOption.Some uris ->
      uris
      |> List.exists (String.equalsCaseInsensitive value)

  let private setMaxAgeHeader config =
    match config.maxAge with
    | None ->
      succeed

    | Some age ->
      Writers.setHeader AccessControlMaxAge (age.ToString())

  let private setAllowCredentialsHeader config =
    if config.allowCookies then
        Writers.setHeader AccessControlAllowCredentials "true"
    else
        succeed

  let private setAllowMethodsHeader config value =
    match config.allowedMethods with
    | InclusiveOption.None ->
      succeed

    | InclusiveOption.All ->
      Writers.setHeader AccessControlAllowMethods "*"

    | InclusiveOption.Some (m :: ms) ->
      let exists = m.ToString() = value || List.exists (fun m -> m.ToString() = value) ms
      if exists then
        let header = (m.ToString()) + "," + (ms |> Seq.map (fun i -> i.ToString()) |> String.concat( ", "))
        Writers.setHeader AccessControlAllowMethods header
      else
        succeed

    | InclusiveOption.Some ([]) ->
      succeed

  let private setAllowOriginHeader value =
    Writers.setHeader AccessControlAllowOrigin value

  let private setExposeHeadersHeader config =
    match config.exposeHeaders with
    | InclusiveOption.None
    | InclusiveOption.Some [] ->
      succeed
    | InclusiveOption.All ->
      Writers.setHeader AccessControlExposeHeaders "*"
    | InclusiveOption.Some hs ->
      let header = hs |> String.concat(", ")
      Writers.setHeader AccessControlExposeHeaders header

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
      exposeHeaders         = InclusiveOption.None
      maxAge                = None }
