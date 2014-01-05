module Suave.Types

open System
open System.IO
open System.Collections.Generic

/// A holder for headers for the http response
type HttpResponse() =
  let mutable headers : List<string*string> = new List<string*string>()
  member h.Headers with get()              = headers and set x = headers <- x

/// A holder for uploaded file meta-data
type HttpUpload(fieldname : string, filename : string, mime_type : string, temp_file_name : string) =
  member x.FieldName = fieldname
  member x.FileName  = filename
  member x.MimeType  = mime_type
  member x.Path      = temp_file_name

/// A holder for the data extracted from the request
type HttpRequest() =
  let mutable url           : string = null
  let mutable meth0d        : string = null
  let mutable remoteAddress : string = null
  let mutable stream        : Stream = null
  let mutable query         : Dictionary<string,string> = new Dictionary<string,string>()
  let mutable headers       : Dictionary<string,string> = new Dictionary<string,string>()
  let mutable form          : Dictionary<string,string> = new Dictionary<string,string>()
  let mutable rawform       : byte[] = Array.empty
  let mutable rawquery      : string = null
  let mutable cookies       : Dictionary<string,(string*string)[]> = new Dictionary<string,(string*string)[]>()
  let mutable username      : string = null
  let mutable password      : string = null
  let mutable sessionId     : string = null
  let mutable response      : HttpResponse = new HttpResponse()
  let mutable files         : List<HttpUpload> = new List<HttpUpload>()
  let mutable isSecure      : bool = false

  member h.Url           with get() = url and set x = url <- x
  member h.Method        with get() = meth0d and set x = meth0d <- x
  member h.RemoteAddress with get() = remoteAddress and set x = remoteAddress <- x
  member h.Stream        with get() = stream and set x = stream <- x
  member h.Query         with get() = query and set x = query <- x
  member h.Headers       with get() = headers and set x = headers <- x
  member h.Form          with get() = form and set x = form <- x
  member h.RawForm       with get() = rawform and set x = rawform <- x
  member h.RawQuery      with get() = rawquery and set x = rawquery <- x
  member h.Cookies       with get() = cookies and set x = cookies <- x
  member h.Username      with get() = username and set x = username <- x
  member h.Password      with get() = password and set x = password <- x
  member h.SessionId     with get() = sessionId and set x = sessionId <- x
  member h.Response      with get() = response
  member h.Files         with get() = files
  member h.IsSecure      with get() = isSecure and set x = isSecure <- x

  /// Clears the request dictionaries for reuse
  member h.Clear() =
    query.Clear()
    headers.Clear()
    form.Clear()
    cookies.Clear()
    files.Clear()
    response.Headers.Clear()

  member private h.Dispose(disposing : bool) =
    if disposing then
      GC.SuppressFinalize(h)

    for upload in h.Files do
      if File.Exists(upload.Path) then
        try
          File.Delete(upload.Path)
        with
        | _ as e -> Log.log "%A" e // we tried

  override h.Finalize() = h.Dispose false

  interface IDisposable with
    member h.Dispose() =
      h.Dispose true

open OpenSSL.X509

/// Gets the supported protocols, HTTP and HTTPS with a certificate
type Protocol =
  /// The HTTP protocol is the core protocol
  | HTTP
  /// The HTTP protocol tunneled in a TLS tunnel
  | HTTPS of X509Certificate
with
  static member FromString(scheme : string, ?cert) =
    match scheme.ToLowerInvariant() with
    | "http" ->
      HTTP
    | "https" ->
      if cert.IsNone then invalidArg "cert" "must supply a cert if you choose HTTPS protocol"
      HTTPS (cert.Value)
    | _ ->
      invalidArg "scheme" "must supply 'http|https'"
  override x.ToString() =
    match x with
    | HTTP    -> "http"
    | HTTPS _ -> "https"

open System.Net

/// A HTTP binding is a protocol is the product of HTTP or HTTP, a DNS or IP binding and a port number
type HttpBinding =
  /// The scheme in use
  { scheme : Protocol
  /// The host or IP address to bind to. This will be interpreted by the operating system
  ; ip     : IPAddress
  /// The port for the binding
  ; port   : uint16 }
with
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

/// The core configuration of suave
type Config =
  /// The bindings for the web server to launch with
  { bindings       : HttpBinding list

  /// An error handler to use for handling exceptions that are
  /// are thrown from the web parts
  ; error_handler  : ErrorHandler

  /// Timeout for responses to be generated
  ; timeout        : TimeSpan

  /// Timeout to wait for the socket bind to finish
  ; listen_timeout : TimeSpan

  /// A cancellation token for the web server. Signalling this token
  /// means that the web server shuts down
  ; ct             : CancellationToken }

/// An exception, raised e.g. if writing to the stream fails
exception InternalFailure of string
