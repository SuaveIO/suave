module Suave.Types

open System
open System.IO
open System.Collections.Generic
open System.Net.Sockets
open System.Net
open System.Text

open Suave.Sockets
open Suave.Utils
open Suave.Log
open Suave.Logging

type Lens<'a,'b> = ('a -> 'b) * ('b -> 'a -> 'a)

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

module Codes =
  type HttpCode =
    | HTTP_100 | HTTP_101
    | HTTP_200 | HTTP_201 | HTTP_202 | HTTP_203 | HTTP_204 | HTTP_205 | HTTP_206
    | HTTP_300 | HTTP_301 | HTTP_302 | HTTP_303 | HTTP_304 | HTTP_305 | HTTP_307
    | HTTP_400 | HTTP_401 | HTTP_402 | HTTP_403 | HTTP_404 | HTTP_405 | HTTP_406
    | HTTP_407 | HTTP_408 | HTTP_409 | HTTP_410 | HTTP_411 | HTTP_412 | HTTP_413
    | HTTP_422 | HTTP_428 | HTTP_429 | HTTP_414 | HTTP_415 | HTTP_416 | HTTP_417
    | HTTP_500 | HTTP_501 | HTTP_502 | HTTP_503 | HTTP_504 | HTTP_505
    static member TryParse (code : int) =
      // TODO: replace with match code with | 100 -> HTTP_100 | ... when API is more set
      let cases = Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<HttpCode>)
      let map_cases =
        cases
        |> Array.map (fun case -> case.Name, Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(case, [||]) :?> HttpCode)
        |> Map.ofArray
      map_cases |> Map.tryFind ("HTTP_" + code.ToString())

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

/// HTTP cookie
type HttpCookie =
  { name      : string
    value     : string
    expires   : DateTimeOffset option
    path      : string option
    domain    : string option
    secure    : bool
    http_only : bool }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpCookie =

  /// Create a new HttpCookie with all the given values.
  let mk name value expires path domain secure http_only =
    { name      = name
      value     = value
      expires   = expires
      path      = path
      domain    = domain
      secure    = secure
      http_only = http_only }

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
  let mk' name value =
    { name      = name
      value     = value
      expires   = None
      path      = None
      domain    = None
      secure    = false
      http_only = true }

  /// An empty cookie value
  let empty = mk' "" ""

  let name x = x.name

  let name_ =
    (fun x -> x.name),
    fun v x -> { x with name = v }

  let value x = x.value

  let value_ =
    (fun x -> x.value),
    fun v x -> { x with value = v }

  let expires x = x.expires

  let expires_ =
    (fun x -> x.expires),
    fun v x -> { x with expires = v }

  let path x = x.path

  let path_ =
    (fun x -> x.path),
    fun v (x : HttpCookie) -> { x with path = v }

  let domain x = x.domain

  let domain_ =
    (fun x -> x.domain),
    fun v x -> { x with domain = v }

  let secure x = x.secure

  let secure_ =
    (fun x -> x.secure),
    fun v x -> { x with secure = v }

  let http_only x = x.http_only

  let http_only_ =
    (fun x -> x.http_only),
    fun v x -> { x with http_only = v }

  /// Assumes only valid characters go in, see http://tools.ietf.org/html/rfc6265#section-4.1.1
  let to_header (x : HttpCookie) =
    let app (sb : StringBuilder) (value : string) = sb.Append value |> ignore
    let sb = new StringBuilder(String.Concat [ x.name; "="; x.value ])
    let app value = app sb (String.Concat [";"; value])
    let appkv k f_map v = v |> Option.iter (fun v -> app (String.Concat [ k; "="; f_map v ]))
    x.domain  |> appkv "Domain" id
    x.path    |> appkv "Path" id
    x.expires |> appkv "Expires" (fun (i : DateTimeOffset) -> i.ToString("R"))
    if x.http_only then app "HttpOnly"
    if x.secure    then app "Secure"
    sb.ToString ()

/// A file's mime type and if compression is enabled or not
type MimeType =
  { name         : string
    compression  : bool }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MimeType =

  let mk name compression =
    { name        = name
      compression = compression }

  let name_ =
    (fun x -> x.name),
    fun v (x : MimeType) -> { x with name = v }

  let compression_ =
    (fun x -> x.compression),
    fun v (x : MimeType) -> { x with compression = v }

type MimeTypesMap = string -> MimeType option

/// A holder for uploaded file meta-data
type HttpUpload =
  { field_name     : string
    file_name      : string
    mime_type      : string
    temp_file_path : string }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpUpload =

  let mk field_name file_name mime_type temp_file_path =
    { field_name     = field_name
      file_name      = file_name
      mime_type      = mime_type
      temp_file_path = temp_file_path }

  let field_name x = x.field_name

  let field_name_ =
    (fun x -> x.field_name),
    fun v x -> { x with field_name = v }

  let file_name x = x.file_name

  let file_name_ =
    (fun x -> x.file_name),
    fun v x -> { x with file_name = v }

  let mime_type x = x.mime_type

  let mime_type_ =
    (fun x -> x.mime_type),
    fun v x -> { x with mime_type = v }

  let temp_file_path x = x.temp_file_path

  let temp_file_path_ =
    (fun x -> x.temp_file_path),
    fun v x -> { x with temp_file_path = v }

type Host =
  /// The Http.Applicatives.host function has ensured this value
  | ServerClient of string
  /// The client's Host header is this value
  | ClientOnly of string
  /// The
  | Forwarded of forwarded_for:string * Host
  member x.value =
    match x with
    | ServerClient v -> v
    | ClientOnly v -> v
    | Forwarded (forwarded_for, _) -> forwarded_for

/// A holder for the data extracted from the request.
type HttpRequest =
  { http_version     : string
    url              : Uri
    host             : Host
    ``method``       : HttpMethod
    headers          : (string * string) list
    raw_form         : byte []
    raw_query        : string
    files            : HttpUpload list
    multipart_fields : (string * string) list
    trace            : TraceHeader
    is_secure        : bool
    ipaddr           : IPAddress }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpRequest =

  open Suave.Utils

  let empty =
    { http_version     = "1.1"
      url              = Uri("http://localhost/")
      host             = ClientOnly "localhost"
      ``method``       = HttpMethod.OTHER("")
      headers          = []
      raw_form         = Array.empty
      raw_query        = ""
      files            = []
      multipart_fields = []
      trace            = TraceHeader.empty
      is_secure        = false
      ipaddr           = IPAddress.Loopback }

  let mk http_version url host meth headers raw_query trace_headers is_secure ip_addr =
    { http_version     = http_version
      url              = url
      host             = host
      ``method``       = meth
      headers          = headers
      raw_form         = Array.empty
      raw_query        = raw_query
      files            = []
      multipart_fields = []
      trace            = trace_headers
      is_secure        = is_secure
      ipaddr           = ip_addr }

  /// Gets the header for the given key in the HttpRequest
  let header x k =
    x.headers %% k

  /// Gets the query string from the HttpRequest. Use
  /// (^^) to try to fetch data from this.
  let query (x : HttpRequest) =
    Parsing.parse_data x.raw_query

  /// Finds the key k from the query string in the HttpRequest
  let query' (x : HttpRequest) (k : string) =
    (query x) ^^ k

  /// Gets the form as a ((string*string option list) from the HttpRequest
  let form  (x : HttpRequest) =
    Parsing.parse_data (ASCII.to_string' x.raw_form)

  /// Finds the key k from the form in the HttpRequest
  let form' (x : HttpRequest) (k : string) =
    (form x) ^^ k

  let http_version x = x.http_version

  let http_version_ =
    (fun x -> x.http_version),
    fun v (x : HttpRequest) -> { x with http_version = v }

  let url x = x.url

  let url_ =
    (fun x -> x.url),
    fun v (x : HttpRequest) -> { x with url = v }

  let ``method`` x = x.``method``

  let method_ =
    (fun x -> x.``method``),
    fun v (x : HttpRequest) -> { x with ``method`` = v }

  let headers x = x.headers

  let headers_ =
    (fun x -> x.headers),
    fun v (x : HttpRequest) -> { x with headers = v }

  let raw_form x = x.raw_form

  let raw_form_ =
    (fun x -> x.raw_form),
    fun v (x : HttpRequest) -> { x with raw_form = v }

  let raw_query x = x.raw_query

  let raw_query_ =
    (fun x -> x.raw_query),
    fun v (x : HttpRequest) -> { x with raw_query = v }

  let files x = x.files

  let files_ =
    (fun x -> x.files),
    fun v (x : HttpRequest) -> { x with files = v }

  let multipart_fields x = x.multipart_fields

  let multipart_fields_ =
    (fun x -> x.multipart_fields),
    fun v (x : HttpRequest) -> { x with multipart_fields = v }

  let trace x = x.trace

  let trace_ =
    (fun x -> x.trace),
    fun v (x : HttpRequest) -> { x with trace = v }

  let is_secure x = x.is_secure

  let is_secure_ =
    (fun x -> x.is_secure),
    fun v x -> { x with is_secure = v }

  let ipaddr x = x.ipaddr

  let ipaddr_ =
    (fun x -> x.ipaddr),
    fun v (x : HttpRequest) -> { x with ipaddr = v }

type ITlsProvider =
  abstract member Wrap : Connection -> SocketOp<Connection>

/// Gets the supported protocols, HTTP and HTTPS with a certificate
type Protocol =
  /// The HTTP protocol is the core protocol
  | HTTP
  /// The HTTP protocol tunneled in a TLS tunnel
  | HTTPS of ITlsProvider
with
  member x.secure =
    match x with | HTTP -> false | _ -> true
  override x.ToString() =
    match x with
    | HTTP    -> "http"
    | HTTPS _ -> "https"

/// A HTTP binding is a protocol is the product of HTTP or HTTP, a DNS or IP binding and a port number
type HttpBinding =
  { /// The scheme in use
    scheme         : Protocol
    socket_binding : SocketBinding }

  member x.uri path query =
    String.Concat [
      x.scheme.ToString(); "://"; x.socket_binding.ToString()
      path
      (match query with | "" -> "" | qs -> "?" + qs)
    ]
    |> fun x -> Uri x

  /// Overrides the default ToString() method to provide an implementation that is assignable
  /// to a BaseUri for a RestClient/HttpClient.
  override x.ToString() =
    String.Concat [
      x.scheme.ToString(); "://"; x.socket_binding.ToString()
      ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpBinding =

  [<Literal>]
  let DefaultBindingPort = 8083us

  let defaults =
    { scheme  = HTTP
      socket_binding = SocketBinding.mk IPAddress.Loopback DefaultBindingPort }

  /// Create a HttpBinding for the given protocol, an IP address to bind to and a port
  /// to listen on.
  let mk scheme ip port =
    { scheme  = scheme
      socket_binding = SocketBinding.mk ip port }

  let mk' scheme ip port =
    { scheme  = scheme
      socket_binding = SocketBinding.mk (IPAddress.Parse ip) (uint16 (port : int)) }

  let scheme x = x.scheme

  let scheme_ =
    (fun x -> x.scheme),
    fun v x -> { x with scheme = v }

  let socket_binding x = x.socket_binding

  let socket_binding_ =
    (fun x -> x.socket_binding),
    fun v x -> { x with socket_binding = v }

type HttpContent =
  | NullContent
  | Bytes of byte []
  | SocketTask of (Connection -> SocketOp<unit>)

open Codes

/// The HttpResult is the structure that you work with to tell Suave how to
/// send the response. Have a look at the docs for HttpContent for further
/// details on what is possible.
type HttpResult =
  { status  : HttpCode
    headers : (string * string) list
    content : HttpContent }

/// A small module that helps you create a HttpResults.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpResult =
  /// The empty HttpResult, with a 404 and a HttpContent.NullContent content
  let empty =
    { status  = HTTP_404
      headers = []
      content = HttpContent.NullContent }

  /// Create a new HttpResult from the status, headers and content.
  let create status headers content =
    { status  = status
      headers = headers
      content = content }

  let status x = x.status

  let status_ =
    (fun x -> x.status),
    fun v (x : HttpResult) -> { x with status = v }

  let headers x = x.headers

  let headers_ =
    (fun x -> x.headers),
    fun v (x : HttpResult) -> { x with headers = v }

  let content x = x.content

  let content_ =
    (fun x -> x.content),
    fun v (x : HttpResult) -> { x with content = v }

/// A SuaveTask is an Async{'a option} which shows that it may need to be
/// evaluated asynchronously to decide whether a value is available.
type SuaveTask<'a> = Async<'a option>

/// A server-key is a 256 bit key with high entropy
type ServerKey = byte []

/// The HttpRuntime is created from the SuaveConfig structure when the web
/// server starts. You can also use the `HttpRuntime` module to create a new
/// value yourself, or use the `empty` one.
type HttpRuntime =
  { server_key         : ServerKey
    error_handler      : ErrorHandler
    mime_types_map     : MimeTypesMap
    home_directory     : string
    compression_folder : string
    logger             : Logger
    matched_binding    : HttpBinding }

/// The HttpContext is the container of the request, runtime, user-state and
/// response.
and HttpContext =
  { request    : HttpRequest
    runtime    : HttpRuntime
    connection : Connection
    user_state : Map<string, obj>
    response   : HttpResult }

and WebPart = HttpContext -> SuaveTask<HttpContext>

/// An error handler takes the exception, a programmer-provided message, a
/// request (that failed) and returns an asynchronous workflow for the handling
/// of the error.
and ErrorHandler = Exception -> String -> WebPart

/// A session store is a reader and a writer function pair keyed on strings.
type StateStore =
  abstract get<'a> : string -> 'a option
  abstract set     : string -> 'a -> WebPart

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
    { server_key         = Crypto.generate_key ServerKeyLength
      error_handler      = fun _ _ -> fun _ -> async.Return None
      mime_types_map     = fun _ -> None
      home_directory     = "."
      compression_folder = "."
      logger             = Loggers.sane_defaults_for LogLevel.Debug
      matched_binding    = HttpBinding.defaults }

  /// make a new HttpRuntime from the given parameters
  let mk server_key error_handler mime_types home_directory compression_folder logger binding =
    { server_key         = server_key
      error_handler      = error_handler
      mime_types_map     = mime_types
      home_directory     = home_directory
      compression_folder = compression_folder
      logger             = logger
      matched_binding    = binding }

  let server_key x = x.server_key

  let server_key_ =
    (fun x -> x.server_key),
    fun v x -> { x with server_key = v }

  let error_handler x = x.error_handler

  let error_handler_ =
    (fun x -> x.error_handler),
    fun v x -> { x with error_handler = v }

  let mime_types_map x = x.mime_types_map

  let mime_types_map_ =
    (fun x -> x.mime_types_map),
    fun v x -> { x with mime_types_map = v }

  let home_directory x = x.home_directory

  let home_directory_ =
    (fun x -> x.home_directory),
    fun v x -> { x with home_directory = v }

  let compression_folder x = x.compression_folder

  let compression_folder_ =
    (fun x -> x.compression_folder),
    fun v x -> { x with compression_folder = v }

  let logger x = x.logger

  let logger_ =
    (fun x -> x.logger),
    fun v x -> { x with logger = v }

  let matched_binding x = x.matched_binding

  let matched_binding_ =
    (fun x -> x.matched_binding),
    fun v x -> { x with matched_binding = v }

/// A module that provides functions to create a new HttpContext.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpContext =
  /// The empty HttpContext is fairly useless for doing real work; you'd be well
  /// adviced to write some of the properties. However, it can be quite useful
  /// in unit tests.
  let empty =
    { request    = HttpRequest.empty
      user_state = Map.empty
      runtime    = HttpRuntime.empty
      connection = Connection.empty
      response   = HttpResult.empty }

  let mk request runtime connection =
    { request    = request
      user_state = Map.empty
      runtime    = runtime
      connection = connection
      response   = { status = HTTP_404
                     headers = []
                     content = NullContent } }

  let request x = x.request

  let request_ =
    (fun x -> x.request),
    fun v x -> { x with request = v }

  let user_state x = x.user_state

  let user_state_ =
    (fun x -> x.user_state),
    fun v x -> { x with user_state = v }

  let runtime x = x.runtime

  let runtime_ =
    (fun x -> x.runtime),
    fun v x -> { x with runtime = v }

  let response x = x.response

  let response_ =
    (fun x -> x.response),
    fun v x -> { x with response = v }

let request f (a : HttpContext) = f a.request a

let context f (a : HttpContext) = f a a

open System.Runtime.Serialization
open System.Runtime.Serialization.Json

[<DataContract>]
type ServerProperties =
  { /// The bindings for the web server to launch with
    bindings                : HttpBinding list
    /// A server-key to use for cryptographic operations. When generated it
    /// should be completely random; you can share this key between load-balanced
    /// servers if you want to have them cryptographically verify similarly.
    server_key              : byte []
    /// Timeout to wait for the socket bind to finish
    listen_timeout          : TimeSpan
    /// buffer size for socket operations
    buffer_size             : int
    /// max number of concurrent socket operations
    max_ops                 : int
    /// MIME types
    mime_types_map          : Map<string, MimeType>
    /// Home or root directory
    home_folder             : string option
    /// Folder for temporary compressed files
    compressed_files_folder : string option }

open System.Threading

/// The core configuration of suave. See also Suave.Web.default_config which
/// you can use to bootstrap the configuration:
/// <code>{ default_config with bindings = [ ... ] }</code>
type SuaveConfig =
  { /// Static configuration variables
    props              : ServerProperties
    /// An error handler to use for handling exceptions that are
    /// are thrown from the web parts
    error_handler           : ErrorHandler
    /// A cancellation token for the web server. Signalling this token
    /// means that the web server shuts down
    ct                      : CancellationToken
    /// A logger to log with
    logger                  : Logger }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ServerProperties =
  let bindings x = x.bindings

  let bindings_ =
    (fun x -> x.bindings),
    fun v x -> { x with bindings = v }

  let server_key x = x.server_key

  let server_key_ =
    (fun x -> x.server_key),
    fun v (x : ServerProperties) -> { x with server_key = v }
  
  let listen_timeout x = x.listen_timeout

  let listen_timeout_ =
    (fun x -> x.listen_timeout),
    fun v x -> { x with listen_timeout = v }

  let buffer_size x = x.buffer_size

  let buffer_size_ =
    (fun x -> x.buffer_size),
    fun v x -> { x with buffer_size = v }

  let max_ops x = x.max_ops

  let max_ops_ =
    (fun x -> x.max_ops),
    fun v x -> { x with max_ops = v }

  let mime_types_map x = x.mime_types_map

  let mime_types_map_ =
    (fun x -> x.mime_types_map),
    fun v (x : ServerProperties) -> { x with mime_types_map = v }

  let home_folder x = x.home_folder

  let home_folder_ =
    (fun x -> x.home_folder),
    fun v x -> { x with home_folder = v }

  let compressed_files_folder x = x.compressed_files_folder

  let compressed_folder_folder_ =
    (fun x -> x.compressed_files_folder),
    fun v x -> { x with compressed_files_folder = v }
  
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SuaveConfig =
  let properties x = x.props

  let to_runtime config content_folder compression_folder =
    HttpRuntime.mk config.props.server_key
                   config.error_handler
                   config.props.mime_types_map.TryFind
                   content_folder
                   compression_folder
                   config.logger

  let error_handler x = x.error_handler

  let error_handler_ =
    (fun x -> x.error_handler),
    fun v (x : SuaveConfig) -> { x with error_handler = v }

  let ct x = x.ct

  let ct_ =
    (fun x -> x.ct),
    fun v x -> { x with ct = v }

  let logger x = x.logger

  let logger_ =
    (fun x -> x.logger),
    fun v (x : SuaveConfig) -> { x with logger = v }

type private SocketBindingForSerialization =
  { ip   : string
    port : string }

type private HttpBindingForSerialization =
  { scheme : string
    socket_binding : SocketBindingForSerialization }

[<DataContract>]
type private ServerPropertiesForSerialization =
  { [<field: DataMember(Name = "bindings")>]
    bindings                : HttpBindingForSerialization []
    [<field: DataMember(Name = "server_key")>]
    server_key              : byte []
    [<field: DataMember(Name = "listen_timeout")>]
    listen_timeout          : TimeSpan
    [<field: DataMember(Name = "namebuffer_size")>]
    buffer_size             : int
    [<field: DataMember(Name = "max_ops")>]
    max_ops                 : int
    [<field: DataMember(Name = "mime_types_map")>]
    mime_types_map          : Map<string, MimeType>
    [<field: DataMember(Name = "home_folder")>]
    home_folder             : string option
    [<field: DataMember(Name = "compressed_files_folder")>]
    compressed_files_folder : string option }

let serialize (properties : ServerProperties)=
  let getSerializableHttpBinding (binding : HttpBinding) =
    let serializable : HttpBindingForSerialization =
      { scheme         = binding.scheme.ToString()
        socket_binding =
          { ip           = binding.socket_binding.ip.ToString()
            port         = binding.socket_binding.port.ToString()} }
    serializable

  let binds = List.toArray properties.bindings

  let new_binds = Array.map(fun n -> getSerializableHttpBinding n) binds

  let props_for_serialization : ServerPropertiesForSerialization =
    { bindings         = new_binds
      server_key       = properties.server_key
      listen_timeout   = properties.listen_timeout
      buffer_size      = properties.buffer_size
      max_ops          = properties.max_ops
      mime_types_map   = properties.mime_types_map
      home_folder      = properties.home_folder
      compressed_files_folder = properties.compressed_files_folder }

  use ms = new MemoryStream()
  (new DataContractJsonSerializer(typeof<ServerPropertiesForSerialization>)).WriteObject(ms, props_for_serialization)
  Encoding.Default.GetString(ms.ToArray())

let private deserialize(s:string)  =
  let json = new DataContractJsonSerializer(typeof<ServerPropertiesForSerialization>)
  let byte_array = Encoding.UTF8.GetBytes(s)
  let stream = new MemoryStream(byte_array)
  let temp_props = json.ReadObject(stream) :?> ServerPropertiesForSerialization
  let to_real_bind (old_bind : HttpBindingForSerialization) =
    let real_bind : HttpBinding =
      { // This does not work with HTTPS
        scheme         = HTTP //if old_bind.scheme = "http" then HTTP else HTTPS
        socket_binding =
          { ip           = IPAddress.Parse old_bind.socket_binding.ip
            port         = Port.Parse old_bind.socket_binding.port} }
    real_bind

  let real_binds = Array.map(fun n -> to_real_bind n) temp_props.bindings
  let props : ServerProperties =
    { bindings         = Array.toList real_binds
      server_key       = temp_props.server_key
      listen_timeout   = temp_props.listen_timeout
      buffer_size      = temp_props.buffer_size
      max_ops          = temp_props.max_ops
      mime_types_map   = temp_props.mime_types_map
      home_folder      = temp_props.home_folder
      compressed_files_folder = temp_props.compressed_files_folder }
  props

let private read_file path =
  try
    Choice1Of2 (File.ReadAllText path)
  with e ->
    Choice2Of2 e.Message

let private parse_config raw_config =
  try
    Choice1Of2 (deserialize raw_config)
  with e ->
    Choice2Of2 e.Message

/// Creates a ServerProperties from a JSON file. Returns the ServerProperties or the
/// error message if something went wrong.
let load_config =
  read_file >> (Choice.bind parse_config)

/// An exception, raised e.g. if writing to the stream fails, should not leak to
/// users of this library
exception InternalFailure of string