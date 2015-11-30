namespace Suave

[<AutoOpen>]
module Http =

  open System
  open System.IO
  open System.Collections.Generic
  open System.Net.Sockets
  open System.Net
  open System.Text
  open Suave.Utils
  open Suave.Utils.Aether
  open Suave.Utils.Aether.Operators
  open Suave.Utils.Collections
  open Suave.Sockets
  open Suave.Tcp
  open Suave.Utils
  open Suave.Log
  open Suave.Logging

  /// <summary>
  /// These are the known HTTP methods. See http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html
  /// </summary>
  [<RequireQualifiedAccess>]
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
    | OTHER of string // This represents a method string that isn't one of the standard methods.

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
      | OTHER s -> s

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpMethod =
    let parse (str:string) =
      match str.ToUpperInvariant() with
        | "GET"     -> HttpMethod.GET
        | "POST"    -> HttpMethod.POST
        | "DELETE"  -> HttpMethod.DELETE
        | "PUT"     -> HttpMethod.PUT
        | "HEAD"    -> HttpMethod.HEAD
        | "CONNECT" -> HttpMethod.CONNECT
        | "PATCH"   -> HttpMethod.PATCH
        | "TRACE"   -> HttpMethod.TRACE
        | "OPTIONS" -> HttpMethod.OPTIONS
        | s         -> HttpMethod.OTHER s

  type HttpCode =
    | HTTP_100 | HTTP_101
    | HTTP_200 | HTTP_201 | HTTP_202 | HTTP_203 | HTTP_204 | HTTP_205 | HTTP_206
    | HTTP_300 | HTTP_301 | HTTP_302 | HTTP_303 | HTTP_304 | HTTP_305 | HTTP_307
    | HTTP_400 | HTTP_401 | HTTP_402 | HTTP_403 | HTTP_404 | HTTP_405 | HTTP_406
    | HTTP_407 | HTTP_408 | HTTP_409 | HTTP_410 | HTTP_411 | HTTP_412 | HTTP_413
    | HTTP_422 | HTTP_428 | HTTP_429 | HTTP_414 | HTTP_415 | HTTP_416 | HTTP_417
    | HTTP_500 | HTTP_501 | HTTP_502 | HTTP_503 | HTTP_504 | HTTP_505

    member x.code = 
      match x with 
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

    member x.reason = 
      match x with
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

    member x.message = 
      match x with 
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

    member x.Describe () =
      sprintf "%d %s: %s" x.code x.reason x.message

    static member TryParse (code: int) =
      HttpCodeStatics.mapCases.Force() |> Map.tryFind ("HTTP_" + string code)

  and private HttpCodeStatics() =
    static member val mapCases : Lazy<Map<string,HttpCode>> =
      lazy
        Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<HttpCode>)
        |> Array.map (fun case -> case.Name, Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(case, [||]) :?> HttpCode)
        |> Map.ofArray
    
  module Codes =
    type X = HttpCode
    [<System.Obsolete("Use Suave.Types.HttpCode")>]
    type HttpCode = X

  /// HTTP cookie
  type HttpCookie =
    { name     : string
      value    : string
      expires  : DateTimeOffset option
      path     : string option
      /// This cookies is only valid for the given domain
      domain   : string option
      /// This cookie is not forwarded over plaintext transports
      secure   : bool
      /// This cookie is not readable from JavaScript
      httpOnly : bool }

    static member name_     = (fun x -> x.name),    fun v x -> { x with name = v }
    static member value_    = (fun x -> x.value), fun v x -> { x with value = v }
    static member expires_  = (fun x -> x.expires), fun v x -> { x with expires = v }
    static member path_     = (fun x -> x.path), fun v (x : HttpCookie) -> { x with path = v }
    static member domain_   = (fun x -> x.domain), fun v x -> { x with domain = v }
    static member secure_   = (fun x -> x.secure), fun v x -> { x with secure = v }
    static member httpOnly_ = (fun x -> x.httpOnly), fun v x -> { x with httpOnly = v }

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpCookie =

    /// Create a new HttpCookie with all the given values.
    let mk name value expires path domain secure httpOnly =
      { name      = name
        value     = value
        expires   = expires
        path      = path
        domain    = domain
        secure    = secure
        httpOnly = httpOnly }

    /// Create a new cookie with the given name, value, and defaults:
    ///
    /// - no explicit expiry time
    /// - path at "/", so that it's global to the domain that it's created under.
    /// - no specific domain (defaults to the current domain plus its subdomains)
    /// - secure = false (you can set it over plain text HTTP - change to true in SSL terminator)
    /// - http_only = true - the cookie can be read from JS - change this to
    ///   false if you want to only be able to read the cookie from JS, but
    ///   Good default if you're implementing session handling.
    /// - version: an optional version field
    ///
    /// More reading:
    /// - http://www.nczonline.net/blog/2009/05/05/http-cookies-explained/
    /// - https://developer.mozilla.org/en-US/docs/Web/API/document.cookie
    ///
    let mkKV name value =
      { name      = name
        value     = value
        expires   = None
        path      = Some "/"
        domain    = None
        secure    = false
        httpOnly = true }

    /// An empty cookie value
    let empty = mkKV "" ""

    /// Assumes only valid characters go in, see http://tools.ietf.org/html/rfc6265#section-4.1.1
    let toHeader (x : HttpCookie) =
      let app (sb : StringBuilder) (value : string) = sb.Append value |> ignore
      let sb = new StringBuilder(String.Concat [ x.name; "="; x.value ])
      let app value = app sb (String.Concat [";"; value])
      let appkv k fMap v = v |> Option.iter (fun v -> app (String.Concat [ k; "="; fMap v ]))
      x.domain  |> appkv "Domain" id
      x.path    |> appkv "Path" id
      x.expires |> appkv "Expires" (fun (i : DateTimeOffset) -> i.ToString("R"))
      if x.httpOnly then app "HttpOnly"
      if x.secure    then app "Secure"
      sb.ToString ()

  /// A file's mime type and if compression is enabled or not
  type MimeType =
    { name         : string
      compression  : bool }

    static member name_ = Property (fun x -> x.name) (fun v (x : MimeType) -> { x with name = v })
    static member compression_ = Property (fun x -> x.compression) (fun v (x : MimeType) -> { x with compression = v })

  type MimeTypesMap = string -> MimeType option

  /// A holder for uploaded file meta-data
  type HttpUpload =
    { fieldName     : string
      fileName      : string
      mimeType      : string
      tempFilePath : string }

    static member fieldName_ = Property<HttpUpload,_> (fun x -> x.fieldName) (fun v x -> { x with fieldName = v })
    static member fileName_ = Property<HttpUpload,_> (fun x -> x.fileName) (fun v x -> { x with fileName = v })
    static member mimeType_ = Property<HttpUpload,_> (fun x -> x.mimeType) (fun v x -> { x with mimeType = v })
    static member tempFilePath_ = Property<HttpUpload,_> (fun x -> x.tempFilePath) (fun v x -> { x with tempFilePath = v })

  [<AllowNullLiteral>]
  type ITlsProvider =
    abstract member Wrap : Connection * obj -> SocketOp<Connection>

  /// Gets the supported protocols, HTTP and HTTPS with a certificate
  type Protocol = 
    /// The HTTP protocol is the core protocol
    | HTTP
    /// The HTTP protocol tunneled in a TLS tunnel
    | HTTPS of obj
      
    member x.secure = 
      match x with
      | HTTP    -> false
      | HTTPS _ -> true
    
    override x.ToString() = 
      match x with
      | HTTP    -> "http"
      | HTTPS _ -> "https"

  /// Type alias for string. This is the host as seen from the server; not
  /// necessarily as seen from the client.
  type Host = string

  /// A holder for the data extracted from the request.
  type HttpRequest =
    { httpVersion     : string
      url             : Uri
      /// The Host that the web server responds to; not necessarily the host called
      /// by the client, as the request may have traversed proxies. As Suave
      /// binds to an IP rather than IP+Hostname, this can be anything the client
      /// has passed as the Host header. If you're behind a proxy, it may be the
      /// DNS name of the node that the reverse proxy forwards to, or if you're
      /// exposing Suave publically, it should match the public DNS name of the
      /// node.
      ///
      /// To ensure the correct host-name is being called, you can use `Http.host`
      /// in your web app.
      host            : Host
      ``method``      : HttpMethod
      headers         : (string * string) list
      rawForm         : byte []
      rawQuery        : string
      files           : HttpUpload list
      multiPartFields : (string * string) list
      trace           : TraceHeader }

    static member httpVersion_     = Property<HttpRequest,_> (fun x -> x.httpVersion) (fun v (x : HttpRequest) -> { x with httpVersion = v })
    static member url_             = Property<HttpRequest,_> (fun x -> x.url) (fun v x -> { x with url = v })
    static member host_            = Property<HttpRequest,_> (fun x -> x.host) (fun v x -> { x with host = v })
    static member method_          = Property<HttpRequest,_> (fun x -> x.``method``) (fun v x -> { x with ``method`` = v })
    static member headers_         = Property<HttpRequest,_> (fun x -> x.headers) (fun v x -> { x with headers = v })
    static member rawForm_         = Property<HttpRequest,_> (fun x -> x.rawForm) (fun v x -> { x with rawForm = v })
    static member rawQuery_        = Property<HttpRequest,_> (fun x -> x.rawQuery) (fun v x -> { x with rawQuery = v })
    static member files_           = Property<HttpRequest,_> (fun x -> x.files) (fun v x -> { x with files = v })
    static member multipartFields_ = Property<HttpRequest,_> (fun x -> x.multiPartFields) (fun v x -> { x with multiPartFields = v })
    static member trace_           = Property<HttpRequest,_> (fun x -> x.trace) (fun v x -> { x with trace = v })

    /// Gets the query string from the HttpRequest. Use queryParam to try to fetch
    /// data for individual items.
    member x.query =
      Parsing.parseData x.rawQuery

    /// Finds the key k from the query string in the HttpRequest. To access form
    /// data, use either `formData` to access normal form data, or `fieldData` to
    /// access the multipart-fields.
    member x.queryParam (k : string) =
      getFirstOpt x.query k

    /// Gets the header for the given key in the HttpRequest
    member x.header k =
      getFirst x.headers k

    /// Gets the form as a ((string * string option) list) from the HttpRequest.
    /// Use formData to get the data for a particular key or use the indexed
    /// property in the HttpRequest.
    member x.form =
      Parsing.parseData (ASCII.toString x.rawForm)

    /// Finds the key k from the form of the HttpRequest. To access query string
    /// parameters, use `queryParam` or to access multipart form fields, use
    /// `fieldData`.
    member x.formData (k : string) =
      getFirstOpt x.form k

    /// Finds the key k from the multipart-form of the HttpRequest. To access
    /// query string parameters, use `queryParam` or to access regular form data,
    /// use `formData`.
    member x.fieldData (k : string) =
      getFirst x.multiPartFields k

    /// Syntactic Sugar to retrieve query string, form or multi-field values from HttpRequest 
    member this.Item
      with get(x) =

        let inline (>>=) f1 f2 x =
          match f1 x with
          | Some x' -> Some x'
          | None -> f2 x

        let params' =
          (tryGetChoice1 this.queryParam)
            >>= (tryGetChoice1 this.formData) 
            >>= (tryGetChoice1 <| getFirst this.multiPartFields)

        params' x

    /// Get the client's view of what host is being called. If you trust your
    /// proxy the value will be fetched from X-Forwarded-Host, then the Host
    /// headers. If you don't explicitly overwrite these headers in the proxy
    /// you may be open to clients spoofing the headers. Hence the explicit
    /// interfaces which force you as a developer to think abou the problem.
    member x.clientHost trustProxy sources : string =
      if trustProxy then
        sources
        |> List.fold (fun state source ->
          state |> Choice.bindSnd (fun _ -> x.header source))
          (Choice2Of2 "")
        |> Choice.orDefault x.host
      else
        x.host

    member x.clientHostTrustProxy =
      x.clientHost true [ "x-forwarded-host" ]

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpRequest =

    open Suave.Utils

    let empty =
      { httpVersion     = "HTTP/1.1"
        url             = Uri "http://localhost/"
        host            = "localhost"
        ``method``      = HttpMethod.OTHER ""
        headers         = []
        rawForm         = Array.empty
        rawQuery        = ""
        files           = []
        multiPartFields = []
        trace           = TraceHeader.empty }

  /// A HTTP binding is a protocol is the product of HTTP or HTTP, a DNS or IP
  /// binding and a port number.
  type HttpBinding =
    { scheme        : Protocol
      socketBinding : SocketBinding }

    member x.uri (path : string) query =
      let path' = if path.StartsWith "/" then path else "/" + path
      String.Concat [
        x.scheme.ToString(); "://"; x.socketBinding.ToString()
        path'
        (match query with | "" -> "" | qs -> "?" + qs)
      ]
      |> fun x -> Uri x

    /// Overrides the default ToString() method to provide an implementation that
    /// is assignable to a BaseUri for a RestClient/HttpClient.
    override x.ToString() =
      String.Concat [ x.scheme.ToString(); "://"; x.socketBinding.ToString() ]

    static member scheme_ = Property<HttpBinding,_> (fun x -> x.scheme) (fun v x -> { x with scheme = v })
    static member socketBinding_ = Property<HttpBinding,_> (fun x -> x.socketBinding) (fun v x -> { x with socketBinding=v })

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpBinding =

    let DefaultBindingPort = 8083us

    let defaults =
      { scheme        = HTTP
        socketBinding = SocketBinding.mk IPAddress.Loopback DefaultBindingPort }

    /// Create a HttpBinding for the given protocol, an IP address to bind to and
    /// a port to listen on – this is the strongly typed overload.
    let mk scheme (ip : IPAddress) (port : Port) = 
      { scheme        = scheme
        socketBinding = SocketBinding.mk ip port }

    /// Create a HttpBinding for the given protocol, an IP address to bind to and
    /// a port to listen on – this is the "stringly typed" overload.
    let mkSimple scheme ip (port : int) = 
      { scheme        = scheme
        socketBinding = SocketBinding.mk (IPAddress.Parse ip) (uint16 port) } 

  type HttpContent =
    /// This is the default HttpContent. If you place this is a HttpResult the web
    /// server won't be that happy. It's assumed by Suave that you place a proper
    /// Bytes or SocketTask content as the value – all built-in Http applicates
    /// do this properly.
    | NullContent
    /// This tells Suave to respond with an array of bytes. Since most responses
    /// are small enough to fit into memory, this is the most used HttpContent
    /// used as results. If you want a streaming result, use SocketTask instead;
    /// useful when you're serving large files through Suave.
    | Bytes of byte []
    /// This task, especially with the `writePreamble`-flag lets your WebPart
    /// control the flow of bytes by using a SocketOp. Contrasting with Bytes,
    /// setting the HttpContent as this discriminated union type lets you stream
    /// data back to the client through Suave.
    | SocketTask of (Connection * HttpResult -> SocketOp<unit>)

    static member NullContentPIso =
      (function | NullContent -> Some ()
                | _ -> None), fun _ -> NullContent

    static member BytesPIso =
      (function | Bytes bs -> Some bs
                | _ -> None), Bytes

    static member SocketTaskPIso =
      (function | SocketTask cb -> Some cb
                | _ -> None),
      (fun cb -> SocketTask cb)

    static member NullContentPLens : PLens<HttpContent, unit> =
      Aether.idLens <-?> HttpContent.NullContentPIso

    static member BytesPLens  : PLens<HttpContent, byte[]> =
      Aether.idLens <-?> HttpContent.BytesPIso

    static member SocketTaskPLens : PLens<HttpContent, Connection * HttpResult -> SocketOp<unit>> =
      Aether.idLens <-?> HttpContent.SocketTaskPIso

  /// The HttpResult is the structure that you work with to tell Suave how to
  /// send the response. Have a look at the docs for HttpContent for further
  /// details on what is possible.
  and HttpResult =
    { status        : HttpCode
      headers       : (string * string) list
      content       : HttpContent
      writePreamble : bool }

    /// The empty HttpResult, with a 404 and a HttpContent.NullContent content
    static member empty =
      { status        = HTTP_404
        headers       = []
        content       = HttpContent.NullContent
        writePreamble = true }

    static member status_ = Property<HttpResult,_> (fun x -> x.status) (fun v x -> { x with status = v })
    static member headers_ = Property<HttpResult,_> (fun x -> x.headers) (fun v x -> { x with headers = v })
    static member content_ = Property<HttpResult,_> (fun x -> x.content) (fun v x -> { x with content = v })
    static member writePreamble_ = Property<HttpResult,_> (fun x -> x.writePreamble) (fun v x -> { x with writePreamble = v })

  /// A server-key is a 256 bit key with high entropy
  type ServerKey = byte []

  type IPAddress with
    static member TryParseC str =
      match IPAddress.TryParse str with
      | false, _ -> Choice2Of2 ()
      | _, ip    -> Choice1Of2 ip

  /// The HttpRuntime is created from the SuaveConfig structure when the web
  /// server starts. You can also use the `HttpRuntime` module to create a new
  /// value yourself, or use the `empty` one.
  type HttpRuntime =
    { serverKey         : ServerKey
      errorHandler      : ErrorHandler
      mimeTypesMap      : MimeTypesMap
      homeDirectory     : string
      compressionFolder : string
      logger            : Logger
      matchedBinding    : HttpBinding
      parsePostData     : bool
      cookieSerialiser  : Suave.Utils.CookieSerialiser
      tlsProvider       : ITlsProvider }

    static member serverKey_ = Property (fun x -> x.serverKey) (fun v x -> { x with serverKey = v })
    static member errorHandler_ = Property (fun x -> x.errorHandler) (fun v x -> { x with errorHandler = v })
    static member mimeTypesMap_ = Property (fun x -> x.mimeTypesMap) (fun v x -> { x with mimeTypesMap = v })
    static member homeDirectory_ = Property (fun x -> x.homeDirectory) (fun v x -> { x with homeDirectory = v })
    static member compressionFolder_ = Property (fun x -> x.compressionFolder) (fun v x -> { x with compressionFolder = v })
    static member logger_ = Property (fun x -> x.logger) (fun v x -> { x with logger = v })
    static member matchedBinding_ = Property (fun x -> x.matchedBinding) (fun v x -> { x with matchedBinding = v })
    static member parsePostData_ = Property (fun x -> x.parsePostData) (fun v x -> { x with parsePostData = v })

  /// The HttpContext is the container of the request, runtime, user-state and
  /// response.
  and HttpContext =
    { /// The HTTP request being processed
      request    : HttpRequest

      /// The HttpRuntime for the request being processed
      runtime    : HttpRuntime

      /// The connection for the request being processed
      connection : Connection

      /// The user state for the request being processed
      userState  : Map<string, obj>

      /// The response for the request being processed
      response   : HttpResult }

    /// Get the IP of the client from the HttpContext.
    member x.clientIp trustProxy sources =
      if trustProxy then
        sources
        |> List.fold (fun state source ->
          state |> Choice.bindSnd (fun _ ->
            x.request.header source |> Choice.bindUnit IPAddress.TryParseC))
          (Choice2Of2 ())
        |> Choice.orDefault x.connection.ipAddr
      else
        x.connection.ipAddr

    /// Warning; if you don't write these headers in your rev.proxy, the client will
    /// be able to spoof them. Related headers:
    /// - client-ip
    /// - x-forwarded-for: the "X-Forwarded-For" client request header field with
    ///   the $remote_addr variable appended to it, separated by a comma. If the
    ///   "X-Forwarded-For" field is not present in the client request header, the
    ///   $proxy_add_x_forwarded_for variable is equal to the $remote_addr variable.
    ///   from http://nginx.org/en/docs/http/ngx_http_proxy_module.html
    ///
    /// Related blog entry, with suggestion on nginx module to use to recursively
    /// tell all upstream proxies to overwrite X-Real-IP:
    /// http://distinctplace.com/infrastructure/2014/04/23/story-behind-x-forwarded-for-and-x-real-ip-headers/
    member x.clientIpTrustProxy =
      x.clientIp true [ "x-real-ip" ]

    member x.isLocal =
      IPAddress.IsLoopback (x.clientIp false [])

    member x.clientPort trustProxy sources : Port =
      if trustProxy then
        sources
        |> List.fold (fun state source ->
          state |> Choice.bindSnd (fun _ ->
            x.request.header source
            |> Choice.bind (
              Choice.parser UInt16.TryParse "failed to parse X-Forwarded-Port")))
          (Choice2Of2 "")
        |> Choice.orDefault x.connection.port
      else
        x.connection.port

    member x.clientPortTrustProxy =
      x.clientPort true [ "x-forwarded-port" ]

    member x.clientProto trustProxy sources : string =
      if trustProxy then
        sources
        |> List.fold (fun state source ->
          state |> Choice.bindSnd (fun _ ->
            x.request.header source))
          (Choice2Of2 "")
        |> Choice.orDefault (x.runtime.matchedBinding.scheme.ToString())
      else
        x.runtime.matchedBinding.scheme.ToString()

    member x.clientProtoTrustProxy =
      x.clientProto true [ "x-forwarded-proto" ]

    static member request_     = Property (fun x -> x.request) (fun v x -> { x with request = v })
    static member userState_   = Property (fun x -> x.userState) (fun v x -> { x with userState = v })
    static member runtime_     = Property (fun x -> x.runtime) (fun v x -> { x with runtime = v })
    static member response_    = Property (fun x -> x.response) (fun v x -> { x with response = v })
    static member clientIp_    = Property (fun (x : HttpContext) -> x.clientIpTrustProxy) (fun v x -> x)
    static member isLocal_     = Property (fun (x : HttpContext) -> x.isLocal) (fun v x -> x)
    static member clientPort_  = Property (fun (x : HttpContext) -> x.clientPortTrustProxy) (fun v x -> x)
    static member clientProto_ = Property (fun (x : HttpContext) -> x.clientProtoTrustProxy) (fun v x -> x)

  /// A WebPart is an asynchronous function that transforms the HttpContext.  An asynchronous return
  /// value of None indicates 'did not handle'. 

  /// An error handler takes the exception, a programmer-provided message, a
  /// request (that failed) and returns an asynchronous workflow for the handling
  /// of the error.
  and ErrorHandler = Exception -> String -> WebPart<HttpContext>

  type WebPart = WebPart<HttpContext>

  /// a module that gives you the `empty` (beware) and `mk` functions for creating
  /// a HttpRuntime
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpRuntime =

    /// The key length in bytes, references Crypto.KeyLength which is appropriate
    /// for the underlying AES-256 bit symmetric crypto in use.
    let ServerKeyLength = Crypto.KeyLength

    /// warn: this is not to be played around with; prefer using the config
    /// defaults instead, from Web.fs, as they contain the logic for printing to
    /// the output stream correctly.
    let empty =
      { serverKey         = Crypto.generateKey ServerKeyLength
        errorHandler      = fun _ _ -> fun _ -> async.Return None
        mimeTypesMap      = fun _ -> None
        homeDirectory     = "."
        compressionFolder = "."
        logger            = Loggers.saneDefaultsFor LogLevel.Debug
        matchedBinding    = HttpBinding.defaults
        parsePostData     = false
        cookieSerialiser  = new BinaryFormatterSerialiser()
        tlsProvider       = null }

    /// make a new HttpRuntime from the given parameters
    let mk serverKey errorHandler mimeTypes homeDirectory compressionFolder logger parsePostData cookieSerialiser tlsProvider binding =
      { serverKey         = serverKey
        errorHandler      = errorHandler
        mimeTypesMap      = mimeTypes
        homeDirectory     = homeDirectory
        compressionFolder = compressionFolder
        logger            = logger
        matchedBinding    = binding
        parsePostData     = parsePostData
        cookieSerialiser  = cookieSerialiser
        tlsProvider       = tlsProvider }

  /// A module that provides functions to create a new HttpContext.
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpContext =
    /// The empty HttpContext is fairly useless for doing real work; you'd be well
    /// adviced to write some of the properties. However, it can be quite useful
    /// in unit tests.
    let empty =
      { request    = HttpRequest.empty
        userState  = Map.empty
        runtime    = HttpRuntime.empty
        connection = Connection.empty
        response   = HttpResult.empty }

    let mk request runtime connection writePreamble =
      { request    = request
        userState  = Map.empty
        runtime    = runtime
        connection = connection
        response   = { status = HTTP_404
                       headers = []
                       content = NullContent
                       writePreamble = writePreamble } }

    let request x = x.request
    let userState x = x.userState
    let runtime x = x.runtime
    let response x = x.response

  let request f (a : HttpContext) = f a.request a

  let context f (a : HttpContext) = f a a

  /////////////////////////
  /////// DEPRECATED //////
  /////////////////////////

  [<Obsolete>]
  let inline succeed x = async.Return (Some x)

  [<Obsolete>]
  let fail = async.Return None

  [<Obsolete>]
  let never : WebPart = fun x -> fail

  [<Obsolete>]
  let inline bind (second : 'b -> Async<'c option>) (first : 'a -> Async<'b option>) : 'a -> Async<'c option> =
    fun x -> 
      async {
        let! e = first x
        match e with
        | None ->
          return None
        | Some t ->
          return! second t }

  [<Obsolete>]
  let inline (>>=) (first : 'a -> Async<'b option>)  (second : 'b -> Async<'c option>) : 'a -> Async<'c option> =
    fun x ->
      bind second first x

  [<Obsolete>]
  let inline (>=>) a b = fun x ->
    match a x with
    | None   -> b x
    | r      -> r

  [<Obsolete>]
  let inline (<|>) (a : WebPart) (b : WebPart) : WebPart =
    fun x ->
      async {
        let! e = a x
        match e with
        | None ->
          let! result = b x
          match result with
          | None -> return None
          | r -> return r
        | r -> return r
      }
  
  [<Obsolete>]
  let rec choose (options : WebPart list) : WebPart =
    fun arg -> async {
    match options with
    | []        -> return None
    | p :: tail ->
      let! res = p arg 
      match res with
      | Some x -> return Some x
      | None   -> return! choose tail arg
    }

  /// Inject a webPart
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

  [<Obsolete>]
  let rec inject (postOp : WebPart) (pairs : (WebPart*WebPart) list) : WebPart =
    fun arg -> async {
      match pairs with
      | []        -> return None
      | (p,q) :: tail ->
        let! res = p arg
        match res with
        | Some x ->
          return! (postOp >>= q) x
        | None   -> return! inject postOp tail arg
      }
  
  [<Obsolete>]
  let inline warbler f a = f a a //which bird? A Warbler!

  [<Obsolete>]
  let inline cnst x = fun _ -> x

  [<Obsolete>]
  let cond d f g a =
    match d with
    | Choice1Of2 x -> f x a
    | Choice2Of2 _ -> g a

  [<Obsolete("open Suave.Response")>]
  module Response =

    open System
    open System.IO

    [<Obsolete>]
    let response statusCode (cnt : byte []) =
      fun (ctx : HttpContext) ->
        let response = 
          { ctx.response with status = statusCode; content = Bytes cnt }
        { ctx with response = response } |> succeed

  [<Obsolete("open Suave.Writers")>]
  module Writers =
    // TODO: transform into a set of lenses with Aether
    // @ https://github.com/xyncro/aether and move closer to HttpContext.

    open System

    [<Obsolete>]
    let setHeader key value (ctx : HttpContext) =
      { ctx with response = { ctx.response with headers = (key, value) :: (ctx.response.headers |> List.filter (fun (k,_) -> k <> key))  } }
      |> succeed
    
    [<Obsolete>]
    let addHeader key value (ctx : HttpContext) =
      { ctx with response = { ctx.response with headers = List.append ctx.response.headers [key, value] } }
      |> succeed
    
    [<Obsolete>]
    let setUserData key value (ctx : HttpContext) =
      { ctx with userState = ctx.userState |> Map.add key (box value) }
      |> succeed
    
    [<Obsolete>]
    let unsetUserData key (ctx : HttpContext) =
      { ctx with userState = ctx.userState |> Map.remove key }
      |> succeed

    // TODO: I'm not sure about having MIME types in the Writers module
    [<Obsolete>]
    let mkMimeType name compression =
      { name=name; compression=compression } |> Some
    
    [<Obsolete>]
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
  [<Obsolete("open Suave.Intermediate")>]
  module Intermediate =

    open System
    open Response

    [<Obsolete>]
    let CONTINUE : WebPart =
      response HTTP_100 [||]
    
    [<Obsolete>]
    let SWITCHING_PROTO : WebPart =
      response HTTP_101 [||]

  // 2xx
  [<Obsolete("open Suave.Successful")>]
  module Successful =
    
    open Suave.Utils
    open Response

    [<Obsolete>]
    let ok s : WebPart = 
      fun ctx -> { ctx with response = { ctx.response with status = HTTP_200; content = Bytes s }} |> succeed
    
    [<Obsolete>]
    let OK a = ok (UTF8.bytes a)

    [<Obsolete>]
    let created s = response HTTP_201 s

    [<Obsolete>]
    let CREATED s = created (UTF8.bytes s)

    [<Obsolete>]
    let accepted s = response HTTP_202 s

    [<Obsolete>]
    let ACCEPTED s = accepted (UTF8.bytes s)

    [<Obsolete>]
    let no_content : WebPart =
      fun ctx -> { ctx with response = { status = HTTP_204; headers = ctx.response.headers; content = Bytes [||]; writePreamble = true }} |> succeed
    
    [<Obsolete>]
    let NO_CONTENT = no_content

  // 3xx
  [<Obsolete("open Suave.Redirection")>]
  module Redirection =

    open Suave.Utils
    open Response
    open Writers

    [<Obsolete>]
    let moved_permanently location =
      setHeader "Location" location
      >>= response HTTP_301 [||]
    
    [<Obsolete>]
    let MOVED_PERMANENTLY location = moved_permanently location

    [<Obsolete>]
    let found location =
      setHeader "Location" location
      >>= response HTTP_302 [||]
    
    [<Obsolete>]
    let FOUND location = found location

    [<Obsolete>]
    let redirect url =
      setHeader "Location" url
      >>= setHeader "Content-Type" "text/html; charset=utf-8"
      >>= response HTTP_302 (
        UTF8.bytes(sprintf "<html>
    <body>
      <a href=\"%s\">%s</a>
    </body>
  </html>"
        url HTTP_302.message))

    [<Obsolete>]
    let not_modified : WebPart =
      fun ctx -> { ctx with response = {status = HTTP_304; headers = []; content = Bytes [||]; writePreamble = true }} |> succeed
    
    [<Obsolete>]
    let NOT_MODIFIED : WebPart =
      not_modified

  // 4xx
  [<Obsolete("open Suave.RequestErrors")>]
  module RequestErrors =

    open Suave.Utils
    open Response
    open Writers

    [<Obsolete>]
    let bad_request s = response HTTP_400 s

    [<Obsolete>]
    let BAD_REQUEST s = bad_request (UTF8.bytes s)

    /// 401: see http://stackoverflow.com/questions/3297048/403-forbidden-vs-401-unauthorized-http-responses/12675357
    [<Obsolete>]
    let unauthorized s =
      setHeader "WWW-Authenticate" "Basic realm=\"protected\""
      >>= response HTTP_401 s
    
    [<Obsolete>]
    let UNAUTHORIZED s = unauthorized (UTF8.bytes s)

    [<Obsolete>]
    let challenge = UNAUTHORIZED HTTP_401.message

    [<Obsolete>]
    let forbidden s = response HTTP_403 s

    [<Obsolete>]
    let FORBIDDEN s = forbidden (UTF8.bytes s)

    [<Obsolete>]
    let not_found s = response HTTP_404 s

    [<Obsolete>]
    let NOT_FOUND message = not_found (UTF8.bytes message)

    [<Obsolete>]
    let method_not_allowed s = response HTTP_405 s

    [<Obsolete>]
    let METHOD_NOT_ALLOWED s = method_not_allowed (UTF8.bytes s)

    [<Obsolete>]
    let not_acceptable s = response HTTP_406 s

    [<Obsolete>]
    let NOT_ACCEPTABLE message = not_acceptable (UTF8.bytes message)

    [<Obsolete>]
    let request_timeout = response HTTP_408 [||]

    // all-caps req.timeout elided intentionally, as nothing can be passed to
    // a writing client

    [<Obsolete>]
    let conflict s = response HTTP_409 s

    [<Obsolete>]
    let CONFLICT message = conflict (UTF8.bytes message)

    [<Obsolete>]
    let gone s = response HTTP_410 s

    [<Obsolete>]
    let GONE s = gone (UTF8.bytes s)

    [<Obsolete>]
    let unsupported_media_type s = response HTTP_415 s

    [<Obsolete>]
    let UNSUPPORTED_MEDIA_TYPE s = unsupported_media_type (UTF8.bytes s)

    [<Obsolete>]
    let unprocessable_entity s = response HTTP_422 s

    [<Obsolete>]
    let UNPROCESSABLE_ENTITY s = unprocessable_entity (UTF8.bytes s)

    [<Obsolete>]
    let precondition_required body = response HTTP_428 body

    [<Obsolete>]
    let PRECONDITION_REQUIRED body = precondition_required (UTF8.bytes body)

    [<Obsolete>]
    let too_many_requests s = response HTTP_429 s

    [<Obsolete>]
    let TOO_MANY_REQUESTS s = too_many_requests (UTF8.bytes s)

  [<Obsolete("open Suave.ServerErrors")>]
  module ServerErrors =
  
    open Suave.Utils
    open Response

    [<Obsolete>]
    let internal_error arr = response HTTP_500 arr

    [<Obsolete>]
    let INTERNAL_ERROR message = internal_error (UTF8.bytes message)

    [<Obsolete>]
    let not_implemented arr = response HTTP_501 arr

    [<Obsolete>]
    let NOT_IMPLEMENTED message = not_implemented (UTF8.bytes message)

    [<Obsolete>]
    let bad_gateway arr = response HTTP_502 arr

    [<Obsolete>]
    let BAD_GATEWAY message = bad_gateway (UTF8.bytes message)

    [<Obsolete>]
    let service_unavailable arr = response HTTP_503 arr

    [<Obsolete>]
    let SERVICE_UNAVAILABLE message = service_unavailable (UTF8.bytes message)

    [<Obsolete>]
    let gateway_timeout arr = response HTTP_504 arr

    [<Obsolete>]
    let GATEWAY_TIMEOUT message = gateway_timeout (UTF8.bytes message)

    [<Obsolete>]
    let invalid_http_version arr = response HTTP_505 arr

    [<Obsolete>]
    let INVALID_HTTP_VERSION = invalid_http_version (UTF8.bytes HTTP_505.message)

  [<Obsolete("open Suave.Filters")>]
  module Applicatives =

    open Suave.Utils
    open Suave.Utils.AsyncExtensions
    open Suave.Logging
    open System
    open System.Text.RegularExpressions

    module private Option =
      let iff b x =
        if b then Some x else None

    [<Obsolete>]
    let path s (x : HttpContext) =
      async.Return (Option.iff (s = x.request.url.AbsolutePath) x)
    
    [<Obsolete>]
    let pathStarts s (x : HttpContext) =
      async.Return (Option.iff (x.request.url.AbsolutePath.StartsWith s) x)
    
    [<Obsolete>]
    let url x = path x

    [<Obsolete>]
    let ``method`` (m : HttpMethod) (x : HttpContext) =
      async.Return (Option.iff (m = x.request.``method``) x)

    [<Obsolete>]
    let isSecure (x : HttpContext) =
      async.Return (Option.iff x.runtime.matchedBinding.scheme.secure x)
    
    [<Obsolete>]
    let pathRegex regex (x : HttpContext) =
      async.Return (Option.iff (Regex.IsMatch(x.request.url.AbsolutePath, regex)) x)
    
    [<Obsolete>]
    let urlRegex x = pathRegex x

    [<Obsolete>]
    let host hostname (x : HttpContext) =
      async.Return (Option.iff (String.equalsOrdinalCI x.request.clientHostTrustProxy hostname) x)

    [<Obsolete>]
    let serverHost hostname (x : HttpContext) =
      async.Return (Option.iff (String.equalsOrdinalCI x.request.host hostname) x)

    [<Obsolete>]
    let clientHost hostname x = host hostname x

    // see http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html

    [<Obsolete>]
    let GET     (x : HttpContext) = ``method`` HttpMethod.GET x
    [<Obsolete>]
    let POST    (x : HttpContext) = ``method`` HttpMethod.POST x
    [<Obsolete>]
    let DELETE  (x : HttpContext) = ``method`` HttpMethod.DELETE x
    [<Obsolete>]
    let PUT     (x : HttpContext) = ``method`` HttpMethod.PUT x
    [<Obsolete>]
    let HEAD    (x : HttpContext) = ``method`` HttpMethod.HEAD x
    [<Obsolete>]
    let CONNECT (x : HttpContext) = ``method`` HttpMethod.CONNECT x
    [<Obsolete>]
    let PATCH   (x : HttpContext) = ``method`` HttpMethod.PATCH x
    [<Obsolete>]
    let TRACE   (x : HttpContext) = ``method`` HttpMethod.TRACE x
    [<Obsolete>]
    let OPTIONS (x : HttpContext) = ``method`` HttpMethod.OPTIONS x

    [<Obsolete>]
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
         | Bytes bs -> bs.LongLength
         | _ -> 0L)
    
    [<Obsolete>]
    let log (logger : Logger) (formatter : HttpContext -> string) (ctx : HttpContext) =
      logger.Log LogLevel.Debug <| fun _ ->
        { trace         = ctx.request.trace
          message       = formatter ctx
          level         = LogLevel.Debug
          path          = "Suave.Http.web-requests"
          ``exception`` = None
          tsUTCTicks    = Globals.utcNow().Ticks }

      succeed ctx

    open Suave.Sscanf
    open ServerErrors

    [<Obsolete>]
    let pathScan (pf : PrintfFormat<_,_,_,_,'t>) (h : 't ->  WebPart) : WebPart =
      
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

    [<Obsolete>]
    let urlScan s x = pathScan s x

    [<Obsolete>]          
    let timeoutWebPart (timeSpan : TimeSpan) (webPart : WebPart) : WebPart =
      fun (ctx : HttpContext) -> async {
        try
          return! Async.WithTimeout (timeSpan, webPart ctx)
        with
          | :? TimeoutException ->
            return! Response.response HttpCode.HTTP_408 (UTF8.bytes "Request Timeout") ctx
            }

  [<Obsolete("open Suave.ServeResource")>]
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
    [<Obsolete>]
    let resource key exists getLast getExtension
                 (send : string -> bool -> WebPart)
                 ctx =
      let log =
        Log.verbose ctx.runtime.logger "Suave.Http.ServeResource.resource" TraceHeader.empty

      let sendIt name compression =
        setHeader "Last-Modified" ((getLast key : DateTime).ToString("R"))
        >>= setHeader "Vary" "Accept-Encoding"
        >>= setMimeType name
        >>= send key compression

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

  [<Obsolete("open Suave.Compression")>]
  module Compression =

    open Suave.Utils
    open Suave.Sockets
    open Suave.Sockets.Control


    open System
    open System.IO
    open System.IO.Compression

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

    [<Obsolete>]
    let loadEncoder s =
      match s with
      | "gzip"    -> Some (GZIP, Compression.gzipEncode)
      | "deflate" -> Some (Deflate, Compression.deflateEncode)
      | _         -> None
    
    [<Obsolete>]
    let getEncoder (request : HttpRequest) =
      match request.header "accept-encoding" with
      | Choice1Of2 value ->
        String.splita ',' value
        |> Array.map String.trim
        |> Array.tryPick loadEncoder
      | _ -> None
    
    [<Obsolete>]
    let parseEncoder (request : HttpRequest) =
      match request.header "accept-encoding" with
      | Choice1Of2 value ->
        String.splita ',' value
        |> Array.map String.trim
        |> Array.tryPick
          (function
           | "gzip"    -> Some GZIP
           | "deflate" -> Some Deflate
           | _         -> None)
      | _ -> None
    
    [<Obsolete>]
    let transform (content : byte []) (ctx : HttpContext) connection : SocketOp<byte []> =
      socket {
        if content.Length > MIN_BYTES_TO_COMPRESS && content.Length < MAX_BYTES_TO_COMPRESS then
          let request = ctx.request
          let enconding = getEncoder request
          match enconding with
          | Some (n,encoder) ->
            do! asyncWriteLn connection (String.Concat [| "Content-Encoding: "; n.ToString() |])
            return encoder content
          | None ->
            return content
        else
          return content
      }

    [<Obsolete>]
    let compress encoding path (fs : Stream) = socket {
      use newFileStream = new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.Write)
      match encoding with
      | GZIP ->
        use gzip = new GZipStream(newFileStream, CompressionMode.Compress)
        do! SocketOp.ofTask (fs.CopyToAsync gzip)
        gzip.Close()
      | Deflate ->
        use deflate = new DeflateStream(newFileStream, CompressionMode.Compress)
        do! SocketOp.ofTask (fs.CopyToAsync deflate)
        deflate.Close()
      | _ ->
        return failwith "invalid case."
      newFileStream.Close()
    }

    [<Obsolete>]
    let compressFile n (stream : Stream) compressionFolder = socket {
      let tempFileName = Path.GetRandomFileName()
      if not (Directory.Exists compressionFolder) then Directory.CreateDirectory compressionFolder |> ignore
      let newPath = Path.Combine(compressionFolder,tempFileName)
      do! compress n newPath stream
      stream.Dispose()
      return newPath
    }

    [<Obsolete>]
    let transformStream (key : string) (getData : string -> Stream) (getLast : string -> DateTime)
                        compression compressionFolder ctx connection =
      socket {
        let stream = getData key
        if compression && stream.Length > int64(MIN_BYTES_TO_COMPRESS) && stream.Length < int64(MAX_BYTES_TO_COMPRESS) then
          let enconding = parseEncoder ctx.request
          match enconding with
          | Some (n) ->
            do! asyncWriteLn connection (String.Concat [| "Content-Encoding: "; n.ToString() |])
            if Globals.compressedFilesMap.ContainsKey key then
              let lastModified = getLast key
              let cmprInfo = new FileInfo(Globals.compressedFilesMap.[key])
              if lastModified > cmprInfo.CreationTime then
                let! newPath = compressFile n stream compressionFolder
                Globals.compressedFilesMap.[key] <- newPath
            else
              let! newPath =  compressFile n stream compressionFolder
              Globals.compressedFilesMap.TryAdd(key,newPath) |> ignore
            return new FileStream(Globals.compressedFilesMap.[key], FileMode.Open, FileAccess.Read, FileShare.Read) :> Stream
          | None ->
            return stream
        else
          return stream
      }

  [<Obsolete("open Suave.Files")>]
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

    [<Obsolete>]
    let sendFile fileName (compression : bool) (ctx : HttpContext) =
      let writeFile file (conn, _) = socket {
        let getFs = fun path -> new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite) :> Stream
        let getLm = fun path -> FileInfo(path).LastWriteTime
        use! fs = Compression.transformStream file getFs getLm compression ctx.runtime.compressionFolder ctx conn

        do! asyncWriteLn conn (sprintf "Content-Length: %d" (fs : Stream).Length)
        do! asyncWriteLn conn ""

        if  ctx.request.``method`` <> HttpMethod.HEAD && fs.Length > 0L then
          do! transferStream conn fs
      }
      { ctx with
          response =
            { ctx.response with
                status = HTTP_200
                content = SocketTask (writeFile fileName) } }
      |> succeed

    [<Obsolete>]
    let file fileName : WebPart =
      resource
        fileName
        (File.Exists)
        (fun name -> FileInfo(name).LastAccessTime)
        (Path.GetExtension)
        sendFile

    [<Obsolete>]
    let resolvePath (rootPath : string) (fileName : string) =
      let fileName =
        if Path.DirectorySeparatorChar.Equals('/') then fileName
        else fileName.Replace('/', Path.DirectorySeparatorChar)
      let calculatedPath = Path.Combine(rootPath, fileName.TrimStart([| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |]))
      if calculatedPath = Path.GetFullPath(calculatedPath) then
        if calculatedPath.StartsWith rootPath then
          calculatedPath
        else raise <| Exception("File canonalization issue.")
      else raise <| Exception("File canonalization issue.")

    [<Obsolete>]
    let browseFile rootPath fileName =
      fun ({request = r; runtime = q} as h) ->
        file (resolvePath rootPath fileName) h

    [<Obsolete>]
    let browseFileHome fileName =
      fun ({request = r; runtime = q} as h) ->
        browseFile q.homeDirectory fileName h

    [<Obsolete>]
    let browse rootPath : WebPart =
      warbler (fun ctx ->
        Log.verbose ctx.runtime.logger
          "Suave.Http.Files.browse"
          TraceHeader.empty
          (sprintf "Files.browse trying file (local file url:'%s' root:'%s')"
            ctx.request.url.AbsolutePath rootPath)
        file (resolvePath rootPath ctx.request.url.AbsolutePath))

    [<Obsolete>]
    let browseHome : WebPart =
      warbler (fun ctx -> browse ctx.runtime.homeDirectory)

    [<Obsolete>]
    let dir rootPath (ctx : HttpContext) =
      let req = ctx.request

      let url = req.url

      let dirname = resolvePath rootPath url.AbsolutePath
      let result = new StringBuilder()

      let filesize  (x : FileSystemInfo) =
        if (x.Attributes ||| FileAttributes.Directory = FileAttributes.Directory) then
          String.Format("{0,-14}",System.Web.HttpUtility.HtmlEncode("<DIR>"))
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
    
    [<Obsolete>]
    let dirHome ctx =
      dir ctx.runtime.homeDirectory ctx
  
  [<Obsolete("open Suave.Embedded")>]
  module Embedded =
    
    open System
    open System.IO
    open System.Reflection

    open Suave.Utils
    open Suave.Sockets.Control

    open Response
    open ServeResource

    [<Obsolete>]
    let defaultSourceAssembly =
      if Assembly.GetEntryAssembly() = null
      then Assembly.GetCallingAssembly()
      else Assembly.GetEntryAssembly()
    
    [<Obsolete>]
    let resources (assembly : Assembly) =
      assembly.GetManifestResourceNames()
    
    [<Obsolete>]
    let lastModified (assembly : Assembly) =
      FileInfo(assembly.Location).CreationTime
    
    [<Obsolete>]
    let sendResource (assembly : Assembly)
                      resourceName
                      (compression : bool)
                      (ctx : HttpContext) =
      let writeResource name (conn, _) = socket {
        let getFs = fun name -> assembly.GetManifestResourceStream(name)
        let getLm = fun _ -> lastModified assembly
        use! fs = Compression.transformStream name getFs getLm compression ctx.runtime.compressionFolder ctx conn

        do! asyncWriteLn conn (sprintf "Content-Length: %d" (fs: Stream).Length)
        do! asyncWriteLn conn ""

        if ctx.request.``method`` <> HttpMethod.HEAD && fs.Length > 0L then
          do! transferStream conn fs
      }
      { ctx with
          response =
            { ctx.response with
                status = HTTP_200
                content = SocketTask (writeResource resourceName) }}
      |> succeed

    [<Obsolete>]
    let sendResourceFromDefaultAssembly resourceName compression =
      sendResource defaultSourceAssembly resourceName compression
    
    [<Obsolete>]
    let resource assembly name =
      resource
        name
        (fun name -> resources assembly |> Array.exists ((=) name))
        (fun _ -> lastModified assembly)
        (Path.GetExtension)
        (sendResource assembly)

    [<Obsolete>]
    let resourceFromDefaultAssembly name =
      resource defaultSourceAssembly name
    
    [<Obsolete>]
    let browse assembly =
      warbler (fun ctx -> resource assembly (ctx.request.url.AbsolutePath.TrimStart [|'/'|]))
    
    [<Obsolete>]
    let browseDefaultAsssembly =
      browse defaultSourceAssembly

  // See www.w3.org/TR/eventsource/#event-stream-interpretation
  [<Obsolete("open Suave.EventSource")>]
  module EventSource =
    open System
    open Suave
    open Suave.Sockets
    open Suave.Sockets.Control
    open Suave.Sockets.Connection
    open Suave.Utils

    [<Literal>]
    [<Obsolete>]
    let private ES_EOL = "\n"

    [<Obsolete>]
    let private ES_EOL_S = ArraySegment<_>(UTF8.bytes ES_EOL, 0, 1)

    [<Obsolete>]
    let asyncWrite (out : Connection) (data : string) =
      asyncWriteBytes out (UTF8.bytes data)

    [<Obsolete>]
    let (<<.) (out : Connection) (data : string) =
      asyncWriteBytes out (UTF8.bytes data)

    [<Obsolete>]
    let dispatch (out : Connection) =
      send out ES_EOL_S

    [<Obsolete>]
    let comment (out : Connection) (cmt : string) =
      out <<. ": " + cmt + ES_EOL

    [<Obsolete>]
    let eventType (out : Connection) (evType : string) =
      out <<. "event: " + evType + ES_EOL

    [<Obsolete>]
    let data (out : Connection) (text : string) =
      out <<. "data: " + text + ES_EOL
    
    [<Obsolete>]
    let esId (out : Connection) (lastEventId : string) =
      out <<. "id: " + lastEventId + ES_EOL
    
    [<Obsolete>]
    let retry (out : Connection) (retry : uint32) =
      out <<. "retry: " + (string retry) + ES_EOL
    
    [<Obsolete>]
    type Message =
      { /// The event ID to set the EventSource object's last event ID value.
        id       : string
        /// The data field for the message. When the EventSource receives multiple consecutive lines that begin with data:, it will concatenate them, inserting a newline character between each one. Trailing newlines are removed.
        data     : string
        /// The event's type. If this is specified, an event will be dispatched on the browser to the listener for the specified event name; the web site source code should use addEventListener() to listen for named events. The onmessage handler is called if no event name is specified for a message.
        ``type`` : string option }

    [<Obsolete>]
    let mkMessage id data =
      { id = id; data = data; ``type`` = None }
    
    [<Obsolete>]
    let mkMessageType id data typ =
      { id = id; data = data; ``type`` = Some typ }
    
    [<Obsolete>]
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
        do! asyncWriteLn out "" // newline after headers

        // Buggy Internet Explorer; 2kB of comment padding for IE
        do! String.replicate 2000 " " |> comment out
        do! 2000u |> retry out
        return! f out
      }
    
    [<Obsolete>]
    let handShake f (ctx : HttpContext) =
      { ctx with
          response =
            { ctx.response with
                status = HTTP_200
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

  [<Obsolete("open Suave.Authentication")>]
  module Authentication =

    open RequestErrors
    open Suave.Utils

    let UserNameKey = "userName"

    let internal parseAuthenticationToken (token : string) =
      let parts = token.Split (' ')
      let enc = parts.[1].Trim()
      let decoded = ASCII.decodeBase64 enc
      let indexOfColon = decoded.IndexOf(':')
      (parts.[0].ToLower(), decoded.Substring(0,indexOfColon), decoded.Substring(indexOfColon+1))

    let inline private addUserName username ctx = { ctx with userState = ctx.userState |> Map.add UserNameKey (box username) }

    [<Obsolete>]
    let authenticateBasic f (protectedPart : WebPart) (ctx : HttpContext) =
      let p = ctx.request
      match p.header "authorization" with
      | Choice1Of2 header ->
        let (typ, username, password) = parseAuthenticationToken header
        if (typ.Equals("basic")) && f (username, password) then
          protectedPart (addUserName username ctx)
        else
          challenge (addUserName username ctx)
      | Choice2Of2 _ ->
        challenge ctx

  [<Obsolete("open Suave.Control")>]
  module Control =
  
    [<Obsolete>]
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