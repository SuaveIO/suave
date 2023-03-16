namespace Suave

open TcpServerFactory
open System.Threading.Tasks
open System.Threading
open Tcp

[<AutoOpen>]
module Web =

  open System
  open System.IO
  open System.Text
  open Suave.Utils
  open Suave.Logging
  open Suave.Logging.Message

  /// The default error handler returns a 500 Internal Error in response to
  /// thrown exceptions.
  let defaultErrorHandler (ex : Exception) msg (ctx : HttpContext) =
    ctx.runtime.logger.error (
      eventX msg
      >> setSingleName "Suave.Web.defaultErrorHandler"
      >> addExn ex)

    if ctx.isLocalTrustProxy then
      Response.response HTTP_500 (Encoding.UTF8.GetBytes("<h1>" + ex.Message + "</h1><br/>" + ex.ToString())) ctx
    else
      Response.response HTTP_500 (Encoding.UTF8.GetBytes HTTP_500.message) ctx

  /// Starts the web server asynchronously.
  ///
  /// Returns the webserver as a tuple of 1) an async computation that yields startup
  /// metrics DTOs when the web server is ready to serve requests, and 2) an async computation
  /// that yields when the web server is being shut down and is being terminated. The async values
  /// returned are not 'hot' in the sense that they have started running, so you must manually
  /// start the 'server' (second item in tuple), as this starts the TcpListener.
  /// Have a look at the example and the unit tests for more documentation.
  /// In other words: don't block on 'listening' unless you have started the server.
  /// The return value from 'listening' (first item in tuple) gives you some metrics on
  /// how quickly suave started.
  let startWebServerAsync (config : SuaveConfig) (webpart : WebPart) =
    ServerKey.validate config.serverKey |> ignore

    let resolveDirectory homeDirectory =
      match homeDirectory with
      | None   -> Path.GetDirectoryName(System.AppContext.BaseDirectory)
      | Some s -> s

    let homeFolder, compressionFolder =
      resolveDirectory config.homeFolder,
      Path.Combine(resolveDirectory config.compressedFilesFolder, "_temporary_compressed_files")

    // spawn tcp listeners/web workers
    let toRuntime = SuaveConfig.toRuntime config homeFolder compressionFolder

    // If noone has already touched the logging configuration, initialise it to
    // that of Suave's configuration.
    Global.initialiseIfDefault { Global.defaultConfig with getLogger = fun _ -> config.logger }

    let startWebWorkerAsync runtime =
      let tcpServer =
        (tcpServerFactory :> TcpServerFactory).create(config.maxOps, config.bufferSize, runtime.matchedBinding.socketBinding,runtime,config.cancellationToken,webpart)

      startTcpIpServerAsync runtime.matchedBinding.socketBinding  tcpServer

    let servers =
       List.map (toRuntime >> startWebWorkerAsync) config.bindings

    let listening = servers |> Seq.map fst |> Async.Parallel
    let server    = servers |> Seq.map snd |> Task.WhenAll
    listening, server

  /// Runs the web server and blocks waiting for the asynchronous workflow to be cancelled or
  /// it returning itself.
  let startWebServer (config : SuaveConfig) (webpart : WebPart) =
    let task = startWebServerAsync config webpart |> snd
    Task.WaitAll task

  /// The default configuration binds on IPv4, 127.0.0.1:8080 with a regular 500 Internal Error handler,
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
      mimeTypesMap          = Writers.defaultMimeTypesMap
      homeFolder            = None
      compressedFilesFolder = None
      logger                = Targets.create Info [| "Suave" |]
      cookieSerialiser      = new BinaryFormatterSerialiser()
      hideHeader            = false
      maxContentLength      = 10000000 // 10 megabytes
      }
