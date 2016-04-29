namespace Suave

/// Parsing and control flow handling for web requests
module internal ParsingAndControl =
  open System
  open System.IO
  open System.Text
  open System.Diagnostics
  open System.Net
  open System.Net.Sockets
  open System.Threading
  open System.Threading.Tasks
  open System.Collections.Generic

  open Suave.Globals
  open Suave.Compression
  open Suave.Sockets
  open Suave.Sockets.Connection
  open Suave.Sockets.Control
  open Suave.Sockets.SocketOp.Operators
  open Suave.Tcp
  
  open Suave.Utils
  open Suave.Utils.Bytes
  open Suave.Utils.Parsing
  open Suave.Logging

  let BadRequestPrefix = "__suave_BAD_REQUEST"

  /// Free up a list of buffers
  let inline free context connection =
    List.iter (fun (x: BufferSegment) -> connection.bufferManager.FreeBuffer (x.buffer, context)) connection.segments

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

  open System.Net.Sockets

  let inline cleanResponse (ctx : HttpContext) =
    { ctx with response = HttpResult.empty }

  let httpLoop (ctxOuter : HttpContext) (consumer : WebPart) =

    let runtime = ctxOuter.runtime

    let facade = new ConnectionFacade(ctxOuter)

    let rec loop (_ : HttpContext) = async {

      let verbose  = Log.verbose runtime.logger "Suave.Web.httpLoop.loop" TraceHeader.empty
      let verbosef = Log.verbosef runtime.logger "Suave.Web.httpLoop.loop" TraceHeader.empty

      verbose "-> processor"
      let! result' = facade.processRequest
      verbose "<- processor"
      match result' with
      | Choice1Of2 result ->
        match result with
        | None -> verbose "'result = None', exiting"
        | Some ctx ->
          let! result'' = HttpOutput.addKeepAliveHeader ctx |> HttpOutput.run consumer
          match result'' with
          | Choice1Of2 result -> 
            match result with
            | None -> ()
            | Some ctx ->
              match ctx.request.header "connection" with
              | Choice1Of2 conn when String.equalsOrdinalCI conn "keep-alive" ->
                verbose "'Connection: keep-alive' recurse"
                return! loop (cleanResponse ctx)
              | Choice1Of2 _ ->
                free "Suave.Web.httpLoop.loop (case Choice1Of2 _)" ctx.connection
                verbose "Connection: close"
              | Choice2Of2 _ ->
                if ctx.request.httpVersion.Equals("HTTP/1.1") then
                  verbose "'Connection: keep-alive' recurse (!)"
                  return! loop (cleanResponse ctx)
                else
                  free "Suave.Web.httpLoop.loop (case Choice2Of2, else branch)" ctx.connection
                  verbose "Connection: close"
                  return ()
          | Choice2Of2 err ->
            verbose (sprintf "Socket error while running webpart, exiting: %A" err)
      | Choice2Of2 err ->
        match err with
        | InputDataError msg ->
          verbose (sprintf "Error parsing http request: %s" msg)
          let! result''' = HttpOutput.run (RequestErrors.BAD_REQUEST msg) ctxOuter
          match result''' with
          | Choice1Of2 _ ->
            verbose "Exiting http loop"
          | Choice2Of2 err ->
            verbose (sprintf "Socket error while sending BAD_REQUEST, exiting: %A" err)

        | err ->
          verbose (sprintf "Socket error while processing request, exiting: %A" err)
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
    let verbose  = Log.verbose runtime.logger "Suave.Web.httpLoop.loop" TraceHeader.empty
    async {
      let! result = loadConnection runtime connection
      match result with
      | Choice1Of2 connection' ->
        do! httpLoop { HttpContext.empty with runtime = runtime; connection = connection' } consumer
      | Choice2Of2 err ->
        verbose (sprintf "Socket error while loading the connection, exiting.")
      return ()
    }


  /// Starts a new web worker, given the configuration and a web part to serve.
  let startWebWorkerAsync (bufferSize, maxOps) (webpart : WebPart) (runtime : HttpRuntime) runServer =
    startTcpIpServerAsync (requestLoop runtime webpart)
                          runtime.matchedBinding.socketBinding
                          runServer

  open System.Reflection

  let resolveDirectory homeDirectory =
    match homeDirectory with
#if NETSTANDARD1_5
    | None   -> Path.GetDirectoryName(typeof<ScanResult>.GetTypeInfo().Assembly.Location)
#else
    | None   -> Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
#endif
    | Some s -> s

////////////////////////////////////////////////////

