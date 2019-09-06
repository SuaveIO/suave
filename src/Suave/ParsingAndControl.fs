namespace Suave

/// Parsing and control flow handling for web requests
module internal ParsingAndControl =

  open System.IO
  open Suave.Sockets
  open Suave.Sockets.Control
  open Suave.Tcp
  open Suave.Utils
  open Suave.Logging
  open Suave.Logging.Message

  /// Free up a list of buffers
  let inline free context connection =
    connection.segments
    |> Seq.iter (fun (x : BufferSegment) ->
      connection.bufferManager.FreeBuffer (x.buffer, context)) 

  /// Load a readable plain-text stream, based on the protocol in use. If plain HTTP
  /// is being used, the stream is returned as it, otherwise a new SslStream is created
  /// to decipher the stream, without client certificates.
  let inline loadConnection (runtime : HttpRuntime) (connection : Connection) = socket{
    match runtime.matchedBinding.scheme with
    | HTTP    ->
      return connection
    | HTTPS o -> 
      return! runtime.tlsProvider.wrap (connection,o)
    }

  let inline cleanResponse (ctx : HttpContext) =
    { ctx with response = HttpResult.empty; userState = Map.empty }

  let inline keepAlive (ctx : HttpContext) =
    match ctx.request.header "connection" with
    | Choice1Of2 conn ->
      String.equalsOrdinalCI conn "keep-alive"
    | Choice2Of2 _ ->
      ctx.request.httpVersion.Equals("HTTP/1.1")

  let httpLoop (ctxOuter : HttpContext) (consumer : WebPart) =

    let logger = ctxOuter.runtime.logger

    let facade = new ConnectionFacade(ctxOuter.connection, logger,  ctxOuter.runtime.matchedBinding)

    let event message =
       eventX message >> setSingleName "Suave.ParsingAndControl.httpLoop.loop"

    let exitHttpLoopWithError (err:Error) = async{
      match err with
      | InputDataError (None, msg) ->
        logger.verbose (event "Error parsing HTTP request with {message}" >> setFieldValue "message" msg)
        match! HttpOutput.run (RequestErrors.BAD_REQUEST msg) ctxOuter with
        | _ ->
          logger.verbose (event "Exiting http loop")

      | InputDataError (Some status,msg) ->
        logger.verbose (event "Error parsing HTTP request with {message}" >> setFieldValue "message" msg)
        match Http.HttpCode.tryParse status with 
        | (Choice1Of2 statusCode) ->
          match! HttpOutput.run (Response.response statusCode (UTF8.bytes msg)) ctxOuter with
          | _ -> logger.verbose (event "Exiting http loop")
        | (Choice2Of2 err) ->
          logger.warn (event "Invalid HTTP status code {statusCode}" >> setFieldValue "statusCode" status)
          match! HttpOutput.run (RequestErrors.BAD_REQUEST msg) ctxOuter with
          | _ ->
            logger.verbose (event "Exiting http loop")
      | err ->
        logger.verbose (event "Socket error while processing request, exiting {error}" >> setFieldValue "error" err)
    }

    let rec loop (_ctx : HttpContext) = async {
      logger.verbose (event "Processing request... -> processor")
      let! result' = facade.processRequest _ctx
      logger.verbose (event "Processed request. <- processor")
      match result' with
      | Choice1Of2 result ->
        match result with
        | None ->
          logger.verbose (event "'result = None', exiting")
        | Some ctx ->
          match! HttpOutput.run consumer (HttpOutput.addKeepAliveHeader ctx) with
          | None -> ()
          | Some ctx ->
              if keepAlive ctx then
                logger.verbose (event "'Connection: keep-alive' recurse")
                return! loop (cleanResponse ctx)
              else
                free "Suave.Web.httpLoop.loop" ctx.connection 
                logger.verbose (event "Connection: close")
                return ()
      | Choice2Of2 err ->
        // Couldn't parse HTTP request; answering with BAD_REQUEST and closing the connection.
        do! exitHttpLoopWithError err
    }
    loop ctxOuter

  /// The request loop initialises a request with a processor to handle the
  /// incoming stream and possibly pass the request to the web parts, a protocol,
  /// a web part, an error handler and a Connection to use for read-write
  /// communication -- getting the initial request stream.
  let inline requestLoop
    (runtime    : HttpRuntime)
    (consumer   : WebPart)
    (connection : Connection) =
    async {
      let! result = loadConnection runtime connection
      match result with
      | Choice1Of2 connection' ->
        do! httpLoop { HttpContext.empty with runtime = runtime; connection = connection' } consumer
      | Choice2Of2 err ->
        runtime.logger.info (eventX "Socket error while loading the connection, exiting")
      return ()
    }

  /// Starts a new web worker, given the configuration and a web part to serve.
  let startWebWorkerAsync (bufferSize, maxOps) (webpart : WebPart) (runtime : HttpRuntime) runServer =
    startTcpIpServerAsync (requestLoop runtime webpart)
                          runtime.matchedBinding.socketBinding
                          runServer

  let resolveDirectory homeDirectory =
    match homeDirectory with
    | None   -> Path.GetDirectoryName(System.AppContext.BaseDirectory)
    | Some s -> s