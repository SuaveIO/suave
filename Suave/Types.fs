module Suave.Types

open System
open System.IO
open System.Collections.Generic

/// A holder for headers for the http response
type HttpResponse() =
  let mutable headers: List<string*string> = new List<string*string>()
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

open System.Security.Cryptography.X509Certificates

/// Gets the supported protocols, HTTP and HTTPS with a certificate
type Protocols =
  /// The HTTP protocol is the core protocol
  | HTTP
  /// The HTTP protocol tunneled in a TLS tunnel
  | HTTPS of X509Certificate

/// A HTTP binding is a protocol is the product of HTTP or HTTP, a DNS or IP binding and a port number
type HttpBinding = Protocols * string * int

/// A web part is a thing that executes on a HttpRequest, asynchronously, maybe executing
/// on the request.
type WebPart = HttpRequest -> Async<unit> option

/// An error handler takes the exception, a programmer-provided message, a request (that failed) and returns
/// an asynchronous workflow for the handling of the error.
type ErrorHandler = Exception -> String -> HttpRequest -> Async<unit>

/// The core configuration of suave
type Config =
  { bindings      : HttpBinding array
  ; error_handler : ErrorHandler
  ; timeout       : int }

/// An exception, raised e.g. if writing to the stream fails
exception InternalFailure of string
