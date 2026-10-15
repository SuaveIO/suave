namespace Suave

open TcpServerFactory
open System.Threading.Tasks
open Tcp

[<AutoOpen>]
module Web =

  open System
  open System.IO
  open System.Text
  open Suave.Utils

  /// The default error handler returns a plain 500 Internal Error in response
  /// to thrown exceptions, without the exception's message or stack trace.
  /// Log the exception from your own handler, or use `developmentErrorHandler`
  /// while developing to see it in the browser.
  let defaultErrorHandler (ex : Exception) msg (ctx : HttpContext) =
    Response.response HTTP_500 (Encoding.UTF8.GetBytes HTTP_500.message) ctx

  /// An error handler for development: returns a 500 Internal Error whose body
  /// contains the exception's message and stack trace (HTML-encoded) to every
  /// client. Do not use it in production. Opt in with
  /// <code>{ defaultConfig with errorHandler = developmentErrorHandler }</code>.
  let developmentErrorHandler (ex : Exception) msg (ctx : HttpContext) =
    let body =
      "<h1>" + Net.WebUtility.HtmlEncode ex.Message + "</h1><br/><pre>"
      + Net.WebUtility.HtmlEncode (ex.ToString()) + "</pre>"
    Response.response HTTP_500 (Encoding.UTF8.GetBytes body) ctx

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
  /// If a binding cannot be bound (e.g. its port is already in use), 'listening' raises the
  /// exception (typically a <code>SocketException</code>) and the server task is faulted with it;
  /// the other bindings are stopped.
  let startWebServerAsync (config : SuaveConfig) (webpart : WebPart) =
    ServerKey.validate config.serverKey |> ignore

    let resolveDirectory homeDirectory =
      match homeDirectory with
      | None   -> Path.GetDirectoryName(System.AppContext.BaseDirectory)
      | Some s -> s

    let homeFolder, compressionFolder =
      resolveDirectory config.homeFolder,
      Path.Combine(resolveDirectory config.compressedFilesFolder, "_temporary_compressed_files")

    // Compressed copies are a cache on disk; copies left behind by resources
    // that were recompressed, renamed or deleted while the server was down are
    // evicted before we start serving, and again once the server has stopped,
    // so the folder stays bounded instead of growing with every restart.
    Compression.cleanup compressionFolder |> ignore

    // spawn tcp listeners/web workers
    let toRuntime = SuaveConfig.toRuntime config homeFolder compressionFolder

    // The servers run on a token of our own, linked to the caller's, so that
    // a binding that fails to bind can stop the others: otherwise the server
    // task would keep waiting on the bindings that did start, and the failure
    // would only show once something else stopped them.
    let stopAll = Threading.CancellationTokenSource.CreateLinkedTokenSource(config.cancellationToken)

    let startWebWorker runtime =
      let tcpServer =
        (tcpServerFactory :> TcpServerFactory).create(config.maxOps, config.bufferSize, runtime.matchedBinding.socketBinding, runtime, stopAll.Token, config.healthCheckEnabled, config.healthCheckIntervalMs, config.maxConnectionAgeSeconds, config.acceptorCount, webpart)

      startTcpIpServer runtime.matchedBinding.socketBinding tcpServer

    let servers =
       List.map (toRuntime >> startWebWorker) config.bindings

    let listening = servers |> Seq.map fst |> Async.Parallel
    let serverTasks = servers |> Seq.map snd |> Seq.toArray
    // A server task only faults when its binding never started listening
    for serverTask in serverTasks do
      serverTask.ContinueWith(
        Action<Task>(fun _ -> try stopAll.Cancel() with :? ObjectDisposedException -> ()),
        TaskContinuationOptions.OnlyOnFaulted) |> ignore
    // The last sweep runs when every server task has finished - its acceptors
    // stopped and the connections they started drained - rather than when
    // cancellation is merely requested: a compression still in flight would
    // otherwise be free to publish an artifact after the sweep had walked
    // past it, and the shutdown cleanup we advertise would still leave files
    // behind. It also keeps this synchronous directory scan off the thread
    // that calls Cancel.
    let server : Task =
      task {
        try
          do! Task.WhenAll(serverTasks)
        finally
          Compression.cleanup compressionFolder |> ignore
          stopAll.Dispose()
      } :> Task
    listening, server

  /// Runs the web server and blocks waiting for the asynchronous workflow to be cancelled or
  /// it returning itself.
  /// Throws if a binding cannot be bound (e.g. a <code>SocketException</code> when the port
  /// is already in use), so a program that only calls this exits with a non-zero code.
  let startWebServer (config : SuaveConfig) (webpart : WebPart) =
    let task = startWebServerAsync config webpart |> snd
    task.GetAwaiter().GetResult()

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
      cookieSerialiser      = new BinaryFormatterSerialiser()
      hideHeader            = false
      hideStartupMessage    = false
      maxContentLength      = 10000000 // 10 megabytes
      healthCheckEnabled    = true   // Enable connection health monitoring
      healthCheckIntervalMs = 30000  // Check every 30 seconds
      maxConnectionAgeSeconds = 300  // Kill connections after 5 minutes (300 seconds)
      filePartSink          = None
      acceptorCount         = 1
      http2Enabled          = true
      webSocketMaxFrameSize = 16 * 1024 * 1024 // 16 MiB
      }
