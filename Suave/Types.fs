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
  ; version   : string option
  }

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
  { connection           : Connection
  ; mutable http_version : string
  ; mutable url          : string
  ; mutable ``method``   : string
  ; remote_address     : IPAddress
  ; query              : Dictionary<string,string>
  ; headers            : Dictionary<string,string>
  ; form               : Dictionary<string,string>
  ; mutable raw_form   : byte[]
  ; mutable raw_query  : string
  ; cookies            : Dictionary<string,(string*string)[]>
  ; mutable user_name  : string
  ; mutable password   : string
  ; mutable session_id : string
  ; response           : HttpResponse
  ; files              : List<HttpUpload>
  ; is_secure          : bool
  ; line_buffer        : ArraySegment<byte>  }

/// Clear the request dictionaries for to reuse the request object instance.
let internal clear (request : HttpRequest) =
  request.query.Clear()
  request.headers.Clear()
  request.form.Clear()
  request.cookies.Clear()
  request.files.Clear()
  request.response.Headers.Clear()

/// Delete all HttpRequest files that were uploaded
let internal delete_files (request : HttpRequest) =
  for upload in request.files do
    if File.Exists(upload.Path) then
      try
        File.Delete(upload.Path)
      with
      | _ as e -> Log.logf "%A" e // we tried

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

/// A web part is a thing that executes on a HttpRequest, asynchronously, maybe executing
/// on the request.
type WebPart = HttpRequest -> Async<unit> option

/// An error handler takes the exception, a programmer-provided message, a request (that failed) and returns
/// an asynchronous workflow for the handling of the error.
type ErrorHandler = Exception -> String -> HttpRequest -> Async<unit>

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
  ; buffer_size    : int

  /// max number of concurrent socket operations
  ; max_ops        : int }

/// An exception, raised e.g. if writing to the stream fails
exception InternalFailure of string