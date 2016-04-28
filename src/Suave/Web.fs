namespace Suave

[<AutoOpen>]
module Web =

  open System
  open System.IO
  open System.Net
  open Suave.Utils
  open Suave.Logging

  /// The default error handler returns a 500 Internal Error in response to
  /// thrown exceptions.
  let defaultErrorHandler (ex : Exception) msg (ctx : HttpContext) =
    ctx.runtime.logger.Log LogLevel.Error (fun _ ->
      LogLine.mk "Suave.Web.defaultErrorHandler" LogLevel.Error
                 ctx.request.trace (Some ex)
                 msg)
    if ctx.isLocal then
      Response.response HTTP_500 (UTF8.bytes (sprintf "<h1>%s</h1><br/>%A" ex.Message ex)) ctx
    else 
      Response.response HTTP_500 (UTF8.bytes HTTP_500.message) ctx

  /// Starts the web server asynchronously.
  ///
  /// Returns the webserver as a tuple of 1) an async computation the yields unit when
  /// the web server is ready to serve quests, and 2) an async computation that yields
  /// when the web server is being shut down and is being terminated. The async values
  /// returned are not 'hot' in the sense that they have started running, so you must manually
  /// start the 'server' (second item in tuple), as this starts the TcpListener.
  /// Have a look at the example and the unit tests for more documentation.
  /// In other words: don't block on 'listening' unless you have started the server.
  /// The return value from 'listening' (first item in tuple) gives you some metrics on
  /// how quickly suave started.
  let startWebServerAsync (config : SuaveConfig) (webpart : WebPart) =
    let homeFolder, compressionFolder =
      ParsingAndControl.resolveDirectory config.homeFolder,
      Path.Combine(ParsingAndControl.resolveDirectory config.compressedFilesFolder, "_temporary_compressed_files")

    // spawn tcp listeners/web workers
    let toRuntime = SuaveConfig.toRuntime config homeFolder compressionFolder true

    let startWebWorkerAsync runtime =
      ParsingAndControl.startWebWorkerAsync 
        (config.bufferSize, config.maxOps) 
        webpart
        runtime 
        (config.tcpServerFactory.create(config.logger, config.maxOps, config.bufferSize,config.autoGrow,runtime.matchedBinding.socketBinding))

    let servers = 
       List.map (toRuntime >> startWebWorkerAsync) config.bindings
              
    let listening = servers |> Seq.map fst |> Async.Parallel
    let server    = servers |> Seq.map snd |> Async.Parallel |> Async.Ignore
    listening, server

  /// Runs the web server and blocks waiting for the asynchronous workflow to be cancelled or
  /// it returning itself.
  let startWebServer (config : SuaveConfig) (webpart : WebPart) =
    Async.RunSynchronously(startWebServerAsync config webpart |> snd, cancellationToken = config.cancellationToken)

  /// The default configuration binds on IPv4, 127.0.0.1:8083 with a regular 500 Internal Error handler,
  /// with a timeout of one minute for computations to run. Waiting for 2 seconds for the socket bind
  /// to succeed.
  let defaultConfig = 
    { bindings              = [ HttpBinding.defaults ]
      serverKey             = Crypto.generateKey HttpRuntime.ServerKeyLength
      errorHandler          = defaultErrorHandler
      listenTimeout         = TimeSpan.FromSeconds 2.
      cancellationToken     = Async.DefaultCancellationToken
      bufferSize            = 8192 // 8 KiB
      maxOps                = 100
      autoGrow              = true
      mimeTypesMap          = Writers.defaultMimeTypesMap
      homeFolder            = None
      compressedFilesFolder = None
      logger                = Loggers.saneDefaultsFor LogLevel.Info
      tcpServerFactory      = new DefaultTcpServerFactory()
      #if NETSTANDARD1_5
      cookieSerialiser      = new JsonFormatterSerialiser()
      #else
      cookieSerialiser      = new BinaryFormatterSerialiser()
      #endif
      tlsProvider           = new DefaultTlsProvider()
      hideHeader            = false }