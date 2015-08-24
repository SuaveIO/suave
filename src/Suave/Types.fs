
module Suave.Types

open System
open System.IO
open System.Collections.Generic
open System.Net.Sockets
open System.Net
open System.Text

open Suave.Utils
open Suave.Utils.Collections
open Suave.Sockets
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
  { name      : string
    value     : string
    expires   : DateTimeOffset option
    path      : string option
    domain    : string option
    secure    : bool
    httpOnly  : bool }

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

type Host =
  /// The Http.Applicatives.host function has ensured this value
  | ServerClient of string
  /// The client's Host header is this value
  | ClientOnly of string
  /// The
  | Forwarded of forwardedFor:string * Host

  member x.value =
    match x with
    | ServerClient v -> v
    | ClientOnly v -> v
    | Forwarded (forwardedFor, _) -> forwardedFor

/// A holder for the data extracted from the request.
type HttpRequest =
  { httpVersion      : string
    url              : Uri
    host             : Host
    ``method``       : HttpMethod
    headers          : (string * string) list
    rawForm          : byte []
    rawQuery         : string
    files            : HttpUpload list
    multiPartFields  : (string * string) list
    trace            : TraceHeader
    isSecure         : bool
    ipaddr           : IPAddress }

  static member httpVersion_     = Property<HttpRequest,_> (fun x -> x.httpVersion) (fun v (x : HttpRequest) -> { x with httpVersion = v })
  static member url_             = Property<HttpRequest,_> (fun x -> x.url) (fun v x -> { x with url = v })
  static member method_          = Property<HttpRequest,_> (fun x -> x.``method``) (fun v x -> { x with ``method`` = v })
  static member headers_         = Property<HttpRequest,_> (fun x -> x.headers) (fun v x -> { x with headers = v })
  static member rawForm_         = Property<HttpRequest,_> (fun x -> x.rawForm) (fun v x -> { x with rawForm = v })
  static member rawQuery_        = Property<HttpRequest,_> (fun x -> x.rawQuery) (fun v x -> { x with rawQuery = v })
  static member files_           = Property<HttpRequest,_> (fun x -> x.files) (fun v x -> { x with files = v })
  static member multipartFields_ = Property<HttpRequest,_> (fun x -> x.multiPartFields) (fun v x -> { x with multiPartFields = v })
  static member trace_           = Property<HttpRequest,_> (fun x -> x.trace) (fun v x -> { x with trace = v })
  static member isSecure_        = Property<HttpRequest,_> (fun x -> x.isSecure) (fun v x -> { x with isSecure = v })
  static member ipaddr_          = Property<HttpRequest,_> (fun x -> x.ipaddr) (fun v x -> { x with ipaddr = v })

  /// Gets the query string from the HttpRequest. Use
  /// queryParam to try to fetch data for individual items.
  member x.query =
    Parsing.parseData x.rawQuery

  /// Finds the key k from the query string in the HttpRequest
  member x.queryParam (k : string) =
    getFirstOpt x.query k

  /// Gets the header for the given key in the HttpRequest
  member x.header k =
    getFirst x.headers k

  /// Gets the form as a ((string*string option list) from the HttpRequest. 
  /// Use formData to get the data for a particular key or use the indexed property in the HttpRequest
  member x.form  =
    Parsing.parseData (ASCII.toString x.rawForm)

  /// Finds the key k from the form in the HttpRequest
  /// Match Choice1Of2 to get the value and Choice2Of2 to get an error message
  member x.formData (k : string) =
    getFirstOpt x.form k


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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpRequest =

  open Suave.Utils

  let empty =
    { httpVersion      = "1.1"
      url              = Uri("http://localhost/")
      host             = ClientOnly "localhost"
      ``method``       = HttpMethod.OTHER("")
      headers          = []
      rawForm          = Array.empty
      rawQuery         = ""
      files            = []
      multiPartFields  = []
      trace            = TraceHeader.empty
      isSecure         = false
      ipaddr           = IPAddress.Loopback }

type ITlsProvider =
  abstract member Wrap : Connection -> SocketOp<Connection>

/// Gets the supported protocols, HTTP and HTTPS with a certificate
type Protocol = 
    /// The HTTP protocol is the core protocol
    | HTTP
    /// The HTTP protocol tunneled in a TLS tunnel
    | HTTPS of ITlsProvider
    
    member x.secure = 
        match x with
        | HTTP -> false
        | _ -> true
    
    override x.ToString() = 
        match x with
        | HTTP -> "http"
        | HTTPS _ -> "https"


/// A HTTP binding is a protocol is the product of HTTP or HTTP, a DNS or IP binding and a port number
type HttpBinding = 
  { scheme: Protocol
    socketBinding : SocketBinding } 

  member x.uri path query =
    String.Concat [
      x.scheme.ToString(); "://"; x.socketBinding.ToString()
      path
      (match query with | "" -> "" | qs -> "?" + qs)
    ]
    |> fun x -> Uri x

  /// Overrides the default ToString() method to provide an implementation that is assignable
  /// to a BaseUri for a RestClient/HttpClient.
  override x.ToString() = String.Concat [ x.scheme.ToString(); "://"; x.socketBinding.ToString() ]

  static member scheme_ = Property<HttpBinding,_> (fun x -> x.scheme) (fun v x -> { x with scheme = v })
  static member socketBinding_ = Property<HttpBinding,_> (fun x -> x.socketBinding) (fun v x -> { x with socketBinding=v })

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpBinding =

  let DefaultBindingPort = 8083us

  let defaults =  
    { scheme  = HTTP
      socketBinding = SocketBinding.mk IPAddress.Loopback DefaultBindingPort }

  /// Create a HttpBinding for the given protocol, an IP address to bind to and a port
  /// to listen on.
  let mk scheme (ip:IPAddress) (port:Port) = 
    { scheme  = scheme
      socketBinding = SocketBinding.mk ip port }

  /// Create a HttpBinding for the given protocol, an IP address to bind to and a port
  /// to listen on.
  let mk' scheme ip (port : int) = 
    { scheme  = scheme 
      socketBinding = SocketBinding.mk (IPAddress.Parse ip) (uint16 port) } 

type HttpContent =
  | NullContent
  | Bytes of byte []
  | SocketTask of (Connection -> SocketOp<unit>)

/// The HttpResult is the structure that you work with to tell Suave how to
/// send the response. Have a look at the docs for HttpContent for further
/// details on what is possible.
type HttpResult =
  { status  : HttpCode
    headers : (string * string) list
    content : HttpContent
    writePreamble : bool }

  /// The empty HttpResult, with a 404 and a HttpContent.NullContent content
  static member empty =
    { status  = HTTP_404
      headers = []
      content = HttpContent.NullContent
      writePreamble = true }

  static member status_ = Property<HttpResult,_> (fun x -> x.status) (fun v x -> { x with status = v })
  static member headers_ = Property<HttpResult,_> (fun x -> x.headers) (fun v x -> { x with headers = v })
  static member content_ = Property<HttpResult,_> (fun x -> x.content) (fun v x -> { x with content = v })
  static member writePreamble_ = Property<HttpResult,_> (fun x -> x.writePreamble) (fun v x -> { x with writePreamble = v })

/// A server-key is a 256 bit key with high entropy
type ServerKey = byte []

/// The HttpRuntime is created from the SuaveConfig structure when the web
/// server starts. You can also use the `HttpRuntime` module to create a new
/// value yourself, or use the `empty` one.
type HttpRuntime =
  { serverKey          : ServerKey
    errorHandler       : ErrorHandler
    mimeTypesMap       : MimeTypesMap
    homeDirectory      : string
    compressionFolder  : string
    logger             : Logger
    matchedBinding     : HttpBinding
    parsePostData      : bool }

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

  static member request_ = Property (fun x -> x.request) (fun v x -> { x with request = v })
  static member user_state_ = Property (fun x -> x.userState) (fun v x -> { x with userState = v })
  static member runtime_ = Property (fun x -> x.runtime) (fun v x -> { x with runtime = v })
  static member response_ = Property (fun x -> x.response) (fun v x -> { x with response = v })

/// A WebPart is an asynchronous function that transforms the HttpContext.  An asynchronous return
/// value of None indicates 'did not handle'. 
and WebPart = HttpContext -> Async<HttpContext option>

/// An error handler takes the exception, a programmer-provided message, a
/// request (that failed) and returns an asynchronous workflow for the handling
/// of the error.
and ErrorHandler = Exception -> String -> WebPart

/// A session store is a reader and a writer function pair keyed on strings.
type StateStore =
  /// Get an item from the state store
  abstract get<'T> : string -> 'T option
  /// Set an item in the state store
  abstract set<'T> : string -> 'T -> WebPart

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
      parsePostData     = false }

  /// make a new HttpRuntime from the given parameters
  let mk serverKey errorHandler mimeTypes homeDirectory compressionFolder logger parsePostData binding =
    { serverKey         = serverKey
      errorHandler      = errorHandler
      mimeTypesMap      = mimeTypes
      homeDirectory     = homeDirectory
      compressionFolder = compressionFolder
      logger            = logger
      matchedBinding    = binding
      parsePostData     = parsePostData }


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

open System.Threading

/// The core configuration of suave. See also Suave.Web.default_config which
/// you can use to bootstrap the configuration:
/// <code>{ default_config with bindings = [ ... ] }</code>
type SuaveConfig =
  { /// The bindings for the web server to launch with
    bindings                : HttpBinding list

    /// A server-key to use for cryptographic operations. When generated it
    /// should be completely random; you can share this key between load-balanced
    /// servers if you want to have them cryptographically verify similarly.
    serverKey              : byte []

    /// An error handler to use for handling exceptions that are
    /// are thrown from the web parts
    errorHandler           : ErrorHandler

    /// Timeout to wait for the socket bind to finish
    listenTimeout          : TimeSpan

    /// A cancellation token for the web server. Signalling this token
    /// means that the web server shuts down
    cancellationToken      : CancellationToken

    /// buffer size for socket operations
    bufferSize             : int

    /// max number of concurrent socket operations
    maxOps                 : int

    /// MIME types
    mimeTypesMap          : MimeTypesMap

    /// Home or root directory
    homeFolder             : string option

    /// Folder for temporary compressed files
    compressedFilesFolder : string option

    /// A logger to log with
    logger                  : Logger }

  static member bindings_              = Property<SuaveConfig,_> (fun x -> x.bindings)              (fun v x -> { x with bindings = v })
  static member serverKey_             = Property<SuaveConfig,_> (fun x -> x.serverKey)             (fun v x -> { x with serverKey = v })
  static member errorHandler_          = Property<SuaveConfig,_> (fun x -> x.errorHandler)          (fun v x -> { x with errorHandler = v })
  static member listenTimeout_         = Property<SuaveConfig,_> (fun x -> x.listenTimeout)         (fun v x -> { x with listenTimeout = v })
  static member ct_                    = Property<SuaveConfig,_> (fun x -> x.cancellationToken)     (fun v x -> { x with cancellationToken = v })
  static member bufferSize_            = Property<SuaveConfig,_> (fun x -> x.bufferSize)            (fun v x -> { x with bufferSize = v })
  static member maxOps_                = Property<SuaveConfig,_> (fun x -> x.maxOps)                (fun v x -> { x with maxOps = v })
  static member mimeTypesMap_          = Property<SuaveConfig,_> (fun x -> x.mimeTypesMap)          (fun v x -> { x with mimeTypesMap = v })
  static member homeFolder_            = Property<SuaveConfig,_> (fun x -> x.homeFolder)            (fun v x -> { x with homeFolder = v })
  static member compressedFilesFolder_ = Property<SuaveConfig,_> (fun x -> x.compressedFilesFolder) (fun v x -> { x with compressedFilesFolder = v })
  static member logger_                = Property<SuaveConfig,_> (fun x -> x.logger)                (fun v x -> { x with logger = v })


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SuaveConfig =
  let toRuntime config contentFolder compressionFolder parsePostData =
    HttpRuntime.mk config.serverKey
                   config.errorHandler
                   config.mimeTypesMap
                   contentFolder
                   compressionFolder
                   config.logger
                   parsePostData
