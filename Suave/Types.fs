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
  ; expires   : DateTime option
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

/// A holder for headers for the http response
type HttpResponse() =
  let mutable headers : List<string*string> = new List<string * string>()
  member h.Headers with get()               = headers and set x = headers <- x

/// A holder for uploaded file meta-data
type HttpUpload(fieldname : string, filename : string, mime_type : string, temp_file_name : string) =
  member x.FieldName = fieldname
  member x.FileName  = filename
  member x.MimeType  = mime_type
  member x.Path      = temp_file_name

/// A holder for the data extracted from the request.
type HttpRequest =
  { mutable http_version : string
  ; mutable url          : string
  ; mutable ``method``   : string
  ; query                : Dictionary<string,string>
  ; headers              : Dictionary<string,string>
  ; form                 : Dictionary<string,string>
  ; mutable raw_form     : byte[]
  ; mutable raw_query    : string
  ; cookies              : Dictionary<string,(string*string)[]>
  ; mutable user_name    : string
  ; mutable password     : string
  ; mutable session_id   : string
  ; response             : HttpResponse
  ; files                : List<HttpUpload>
  ; mutable trace        : Log.TraceHeader
  ; is_secure            : bool }

/// Gets the query from the HttpRequest
let query (x : HttpRequest) = x.query
/// Gets the form from the HttpRequest
let form  (x : HttpRequest) = x.form

/// TODO: see if we can't get nice perf without resorting to mutable state
/// Clear the request dictionaries for to reuse the request object instance.
let internal clear (request : HttpRequest) =
  request.query.Clear()
  request.headers.Clear()
  request.form.Clear()
  request.cookies.Clear()
  request.files.Clear()
  request.response.Headers.Clear()
  request.trace <- Log.TraceHeader.Empty

open OpenSSL.X509

/// Gets the supported protocols, HTTP and HTTPS with a certificate
type Protocol =
  /// The HTTP protocol is the core protocol
  | HTTP
  /// The HTTP protocol tunneled in a TLS tunnel
  | HTTPS of X509Certificate
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

/// An error handler takes the exception, a programmer-provided message, a request (that failed) and returns
/// an asynchronous workflow for the handling of the error.
type ErrorHandler = Exception -> String -> HttpContext -> Async<unit>

and HttpRuntime =
  { protocol           : Protocol
  ; web_part_timeout   : TimeSpan
  ; error_handler      : ErrorHandler
  ; mime_types_map     : MimeTypesMap
  ; home_directory     : string
  ; compression_folder : string
  ; logger             : Log.Logger }

and HttpContext =
  { request    : HttpRequest
  ; runtime    : HttpRuntime
  ; connection : Connection }

let request f (a : HttpContext) = f a.request a

/// A web part is a thing that executes on a HttpRequest, asynchronously, maybe executing
/// on the request.
type WebPart = HttpContext -> Async<unit> option

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

  /// Timeout for responses to be generated from the web part/user code.
  ; web_part_timeout : TimeSpan

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
  ; logger           : Log.Logger }

/// An exception, raised e.g. if writing to the stream fails
exception InternalFailure of string

/// Supported HTTP compression encondings
type ContentEncoding = GZip | Deflate | Identity
