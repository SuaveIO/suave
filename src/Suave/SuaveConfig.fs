namespace Suave

open System

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
    cancellationToken      : Threading.CancellationToken

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

    /// The cookie serialiser to use for converting the data you save in cookies
    /// from your application into a byte array.
    cookieSerialiser      : CookieSerialiser

    /// Make this true, if you want Suave not to display its server header in
    /// every response. Defaults to false.
    hideHeader            : bool

    /// Maximun upload size in bytes
    maxContentLength      : int

    /// Enable background connection health monitoring
    healthCheckEnabled    : bool

    /// Interval for connection health checks in milliseconds
    healthCheckIntervalMs : int

    /// Maximum age for an active connection before it's considered stuck (seconds)
    maxConnectionAgeSeconds : int }

  member x.withBindings(v)              = { x with bindings = v }
  member x.withServerKey(v)             = { x with serverKey = v }
  member x.withErrorHandler(v)          = { x with errorHandler = v }
  member x.withListenTimeout(v)         = { x with listenTimeout = v }
  member x.withCancellationToken(v)     = { x with cancellationToken = v }
  member x.withBufferSize(v)            = { x with bufferSize = v }
  member x.withMaxOps(v)                = { x with maxOps = v }
  member x.withMimeTypesMap(v)          = { x with mimeTypesMap = v }
  member x.withHomeFolder(v)            = { x with homeFolder = v }
  member x.withCompressedFilesFolder(v) = { x with compressedFilesFolder = v }
  member x.withHiddenHeader(v)          = { x with hideHeader = v }
  member x.withMaxContentLength(v)      = { x with maxContentLength = v }
  member x.withHealthCheckEnabled(v)    = { x with healthCheckEnabled = v }
  member x.withHealthCheckIntervalMs(v) = { x with healthCheckIntervalMs = v }
  member x.withMaxConnectionAgeSeconds(v) = { x with maxConnectionAgeSeconds = v }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SuaveConfig =

  /// Convert the Suave configuration to a runtime that the web server understands.
  /// You will normally not have to use this function as a consumer from the
  /// library, but it may be useful for unit testing with the HttpRuntime record.
  let toRuntime config contentFolder compressionFolder =
    HttpRuntime.create config.serverKey
                       config.errorHandler
                       config.mimeTypesMap
                       contentFolder
                       compressionFolder
                       config.cookieSerialiser
                       config.hideHeader
                       config.maxContentLength

  /// Finds an endpoint that is configured from the given configuration. Throws
  /// an exception if the configuration has no bindings. Useful if you make
  /// the suave configuration parametised, because it is then enough for your
  /// software to find a valid endpoint to make HTTP/ES/WebSocket requests to.
  let firstBinding (cfg : SuaveConfig) =
    match cfg.bindings with
    | [] ->
      failwith "No bindings found for SuaveConfig."

    | b :: _ ->
      b

  /// Construct a `System.Uri` from the first binding available in Suave, by
  /// giving a path and a uri.
  let firstBindingUri (cfg : SuaveConfig) path query =
    let binding = firstBinding cfg
    binding.uri path query
