namespace Suave

open Suave.Logging
open Suave.Utils
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

    /// Buffer manager auto grow
    autoGrow               : bool

    /// max number of concurrent socket operations
    maxOps                 : int

    /// MIME types
    mimeTypesMap          : MimeTypesMap

    /// Home or root directory
    homeFolder             : string option

    /// Folder for temporary compressed files
    compressedFilesFolder : string option

    /// Suave's logger. You can override the default instance if you wish to
    /// ship your logs, e.g. using https://www.nuget.org/packages/Logary.Adapters.Suave/
    /// Also, this logger will be configured by default for Suave unless you
    /// explicitly use `Suave.Logging.Global.initialise` before starting the
    /// web server (the first time – the second time, the static will already
    /// have been initialised).
    logger                : Logger

    /// Pluggable TCP async sockets implementation. You can choose betwee libuv
    /// and CLR's Async Socket Event Args. Currently defaults to the managed-only
    /// implementation.
    tcpServerFactory      : TcpServerFactory

    /// The cookie serialiser to use for converting the data you save in cookies
    /// from your application into a byte array.
    cookieSerialiser      : CookieSerialiser

    /// A TLS provider implementation.
    tlsProvider           : TlsProvider

    /// Make this true, if you want Suave not to display its server header in
    /// every response. Defaults to false.
    hideHeader            : bool

    /// Maximun upload size in bytes
    maxContentLength      : int }

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
  static member tcpServerFactory_      = Property<SuaveConfig,_> (fun x -> x.tcpServerFactory)      (fun v x -> { x with tcpServerFactory = v })
  static member hideHeader_            = Property<SuaveConfig,_> (fun x -> x.hideHeader)            (fun v x -> { x with hideHeader = v })
  static member maxContentLength_      = Property<SuaveConfig,_> (fun x -> x.maxContentLength)      (fun v x -> { x with maxContentLength = v })

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
  member x.withLogger(v)                = { x with logger = v }
  member x.withTcpServerFactory(v)      = { x with tcpServerFactory = v }
  member x.withHiddenHeader(v)          = { x with hideHeader = v }
  member x.withMaxContentLength(v)      = { x with maxContentLength = v }


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
                       config.logger
                       config.cookieSerialiser
                       config.tlsProvider
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
