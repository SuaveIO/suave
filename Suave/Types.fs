module Suave.Types

open System
open System.IO
open System.Collections.Generic
open System.Net.Sockets
open Socket
open System.Net

/// HTTP cookie
type HttpCookie =
  { name      : string
  ; value     : string
  ; expires   : DateTimeOffset option
  ; path      : string option
  ; domain    : string option
  ; secure    : bool
  ; http_only : bool
  ; version   : string option }
 
/// A file's mime type and if compression is enabled or not
type MimeType =
  { name         : string
  ; compression  : bool }

type MimeTypesMap = string -> MimeType option

/// A holder for uploaded file meta-data
type HttpUpload(fieldname : string, filename : string, mime_type : string, temp_file_name : string) =
  member x.FieldName = fieldname
  member x.FileName  = filename
  member x.MimeType  = mime_type
  member x.Path      = temp_file_name

/// A holder for the data extracted from the request.
type HttpRequest =
  { http_version : string
  ; url          : string
  ; ``method``   : string
  ; headers              : (string * string) list
  ; raw_form     : byte[]
  ; raw_query    : string
  ; files                : HttpUpload list
  ; multipart_fields     : (string * string) list
  ; trace        : Log.TraceHeader
  ; is_secure            : bool
  ; ipaddr               : IPAddress }

open Suave.Utils
///// Gets the query from the HttpRequest
let query (x : HttpRequest) = Parsing.parse_data x.raw_query |> List.ofArray
///// Gets the form from the HttpRequest
let form  (x : HttpRequest) = Parsing.parse_data (ASCII.to_string' x.raw_form) |> List.ofArray

type ITlsProvider =
  abstract member Wrap : Connection -> SocketOp<Connection>

/// Gets the supported protocols, HTTP and HTTPS with a certificate
type Protocol =
  /// The HTTP protocol is the core protocol
  | HTTP
  /// The HTTP protocol tunneled in a TLS tunnel
  | HTTPS of ITlsProvider
with
  override x.ToString() =
    match x with
    | HTTP    -> "http"
    | HTTPS _ -> "https"

open System.Net

type Port = uint16

/// A HTTP binding is a protocol is the product of HTTP or HTTP, a DNS or IP binding and a port number
type HttpBinding =
  /// The scheme in use
  { scheme : Protocol
  /// The host or IP address to bind to. This will be interpreted by the operating system
  ; ip     : IPAddress
  /// The port for the binding
  ; port   : Port }
with
  /// Create a HttpBinding for the given protocol, an IP address to bind to and a port
  /// to listen on.
  static member Create(proto, ip : string, port : int) =
    { scheme = proto
    ; ip     = IPAddress.Parse ip
    ; port   = uint16 port }
  /// Overrides the default ToString() method to provide an implementation that is assignable
  /// to a BaseUri for a RestClient/HttpClient.
  override x.ToString() =
    sprintf "%O://%O:%d/" x.scheme x.ip x.port

/// A session store is a reader and a writer function pair keyed on strings.
type SessionStore<'a> = (string -> 'a option) * (string -> 'a -> unit)

type HttpContent =
  | NullContent
  | Bytes of byte []
  | SocketTask of (Connection -> SocketOp<unit>)

/// <summary>
/// <para>These are the known HTTP methods.</para><para>
/// If you are getting compile-errors
/// on this; make sure you don't mean the similarly named functions from the
/// Applicatives module, e.g. by opening Applicatives after opening Http.
/// </para></summary>
module Methods =
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

open Codes

type HttpResult = 
  { status  : HttpCode
  ; headers : (string * string) list
  ; content : HttpContent }

type SuaveTask<'a> = Async<'a option>

/// An error handler takes the exception, a programmer-provided message, a request (that failed) and returns
/// an asynchronous workflow for the handling of the error.
type ErrorHandler = Exception -> String -> WebPart

and HttpRuntime =
  { protocol           : Protocol
  ; error_handler      : ErrorHandler
  ; mime_types_map     : MimeTypesMap
  ; home_directory     : string
  ; compression_folder : string
  ; logger             : Log.Logger
  ; session_provider   : ISessionProvider }

and HttpContext =
  { request    : HttpRequest
  ; runtime    : HttpRuntime
  ; user_state : Map<string, obj>
  ; response   : HttpResult }

and ISessionProvider =
  abstract member Generate : TimeSpan * HttpContext -> string
  abstract member Validate : string * HttpContext -> bool
  abstract member Session<'a>  : string -> SessionStore<'a>

and WebPart = HttpContext -> SuaveTask<HttpContext>

let request f (a : HttpContext) = f a.request a
let context f (a : HttpContext) = f a a

open System.Threading

/// The core configuration of suave. See also Suave.Web.default_config which
/// you can use to bootstrap the configuration:
/// <code>{ default_config with bindings = [ ... ] }</code>
type SuaveConfig =
  /// The bindings for the web server to launch with
  { bindings         : HttpBinding list

  /// An error handler to use for handling exceptions that are
  /// are thrown from the web parts
  ; error_handler    : ErrorHandler

  /// Timeout to wait for the socket bind to finish
  ; listen_timeout   : TimeSpan

  /// A cancellation token for the web server. Signalling this token
  /// means that the web server shuts down
  ; ct               : CancellationToken

  /// buffer size for socket operations
  ; buffer_size      : int

  /// max number of concurrent socket operations
  ; max_ops          : int

  /// MIME types
  ; mime_types_map   : MimeTypesMap

  /// Home or root directory
  ; home_folder      : string option

  /// Folder for temporary compressed files
  ; compressed_files_folder : string option

  /// A logger to log with
  ; logger           : Log.Logger

  /// A http session provider
  ; session_provider : ISessionProvider }

/// An exception, raised e.g. if writing to the stream fails
exception InternalFailure of string
