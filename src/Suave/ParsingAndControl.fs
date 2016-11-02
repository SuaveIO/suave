﻿namespace Suave

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
  open Suave.Logging.Message

  /// Free up a list of buffers
  let inline free context connection =
    connection.segments
    |> List.iter (fun (x : BufferSegment) ->
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

  open System.Net.Sockets

  let inline cleanResponse (ctx : HttpContext) =
    { ctx with response = HttpResult.empty }

  open Http2
  open Hpack

  let http2readRequest (matchedBinding: HttpBinding) (cn2 : Http2Connection) : SocketOp<HttpRequest> = 
    socket {

      // Read headers till you get an end header
      let loop = ref true
      let payloads = List<byte[]>()
      while !loop do
        let! frame = cn2.read ()
        match frame with
        | (header,Headers (priorityOption, bytes)) ->
          payloads.Add bytes
          loop := not(testEndHeader header.flags)
        | (header,f) ->
          printfn "Got frame: %A" f
          printfn "Spkiping"
      // Concatenate payloads
      let payload = Array.concat payloads
      // A receiving endpoint reassembles the header block by concatenating its fragments and then 
      // decompresses the block to reconstruct the header list.
      let headerList = decodeHeader cn2.decodeDynamicTable payload

      let (Choice1Of2 _method) = headerList %% ":method"
      let (Choice1Of2 _scheme) = headerList %% ":scheme"
      let (Choice1Of2 _path  ) = headerList %% ":path"
      let (Choice1Of2 _host  ) = headerList %% ":authority"

      let path' = if _path.StartsWith "/" then _path else "/" + _path

      let url =
        String.Concat [
          matchedBinding.scheme.ToString(); "://"; matchedBinding.socketBinding.ToString()
          path' ] |> Uri

      let r = 
        { httpVersion     = "HTTP/2.0"
          url             = url
          host            = _host
          ``method``      = HttpMethod.parse  _method
          headers         = List.filter (fun (x,y) -> x <> ":method" || x <> ":scheme" || x <> ":path" || x <> ":authority") headerList
          rawForm         = Array.empty
          rawQuery        = ""
          files           = []
          multiPartFields = []
          trace           = TraceHeader.empty }

      return r
      }

  let http2Loop (facade : ConnectionFacade) (consumer : WebPart) = 

    socket {
      let cn2 = new Http2Connection(facade)
      let! q = cn2.read ()
      match q with
      | header,Settings (flag,settings) ->
        Async.Start (cn2.writeLoop (facade.GetCurrentContext()) consumer)
        // The SETTINGS frames received from a peer as part of the connection preface MUST be acknowledged 
        // (see Section 6.5.3) after sending the connection preface.
        let! request = http2readRequest (facade.GetCurrentContext().runtime.matchedBinding) cn2
        // queue for processing
        do cn2.send request
        do cn2.stop()
        free "Suave.ParsingAndControl.http2Loop" (facade.GetCurrentContext().connection)
      | _, r ->
        printfn "Expecting SETTINGS frame, got: %A" r
      }

  let httpLoop (ctxOuter : HttpContext) (consumer : WebPart) =

    let logger = ctxOuter.runtime.logger
    let event message =
      eventX message 
      >> setSingleName "Suave.ParsingAndControl.httpLoop.loop"

    let facade = new ConnectionFacade(ctxOuter)

    let rec processRequest ctx = async {
       // Check for Upgrade header.
       (*
       A server that supports HTTP/2 accepts the upgrade with a 101 (Switching Protocols) response. After the empty line 
       that terminates the 101 response, the server can begin sending HTTP/2 frames. These frames MUST include a response
       to the request that initiated the upgrade.
       *)

       match ctx.request.headers %% "upgrade" with
       | Choice1Of2 "h2c" -> 
         let! _ = HttpOutput.run Intermediate.CONTINUE ctx
         logger.verbose (event "sent 100-continue response")
         logger.verbose (event "Connection upgraded to HTTP/2")
         let! a = http2Loop facade consumer
         return ()
       | Choice1Of2 "h2" ->
          // implies TLS
         if ctx.runtime.matchedBinding.scheme <> Http.Protocol.HTTP then
           let! _ = HttpOutput.run Intermediate.CONTINUE ctx
           logger.verbose (event "sent 100-continue response")
           logger.verbose (event "Connection upgraded to HTTP/2")
           let! a = http2Loop facade consumer
           return ()
          else
            failwith "TLS required"
       | Choice1Of2 _ -> 
         ignore ()
       | Choice2Of2 _ -> 
         ignore ()

       // the difference is that in http2 we would queue the response (even the re-solution of the response)
       // and will continue reading (duplex) - we need a loop reading and a loop writing parallel
       let! result'' = HttpOutput.addKeepAliveHeader ctx |> HttpOutput.run consumer
       match result'' with
       | Choice1Of2 result -> 
         match result with
         | None -> ()
         | Some ctx ->
           match ctx.request.header "connection" with
           | Choice1Of2 conn when String.equalsOrdinalCI conn "keep-alive" ->
             logger.verbose (event "'Connection: keep-alive' recurse")
             return! loop (cleanResponse ctx)
           | Choice1Of2 _ ->
             free "Suave.Web.httpLoop.loop (case Choice1Of2 _)" ctx.connection
             logger.verbose (event "Connection: close")
           | Choice2Of2 _ ->
             if ctx.request.httpVersion.Equals("HTTP/1.1") then
               logger.verbose (event "'Connection: keep-alive' recurse (!)")
               return! loop (cleanResponse ctx)
             else
               free "Suave.Web.httpLoop.loop (case Choice2Of2, else branch)" ctx.connection
               logger.verbose (event "Connection: close")
               return ()

       | Choice2Of2 err ->
           logger.info (
             event "Socket error while running webpart, exiting"
             >> addExn err)
      }
    and loop (_ : HttpContext) = async {

      logger.verbose (event "-> processor")
      logger.verbose (event "reading first line of request")
      let! q = facade.readLine
      match q with
      | Choice2Of2 err ->
          logger.verbose (event ("Exiting: " + err.ToString()))
      | Choice1Of2 firstLine ->
        if firstLine.Equals("PRI * HTTP/2.0") then
          logger.verbose (event "HTTP/2 preamble detected.")
          // read the remaining octects "\r\nSM\r\n\r\n"
          let! a = facade.skipLine
          let! a = facade.skipLine
          let! a = facade.skipLine
          // inititate http2 looop
          let! a = http2Loop facade consumer
          return ()
        else
          // else regular http loop
          logger.verbose (event "Processing request... -> processor")
          let! result' = facade.processRequest firstLine
          logger.verbose (event "Processed request. <- processor")
          match result' with
          | Choice1Of2 result ->
            match result with
            | None ->
              logger.verbose (event "'result = None', exiting")
            | Some ctx ->
              return! processRequest ctx
          | Choice2Of2 err ->
            match err with
            | InputDataError msg ->
              logger.info (event "Error parsing HTTP request with {message}" >> setFieldValue "message" msg)
              let! result''' = HttpOutput.run (RequestErrors.BAD_REQUEST msg) ctxOuter
              match result''' with
              | Choice1Of2 _ ->
                logger.verbose (event "Exiting http loop")
              | Choice2Of2 err ->
                logger.verbose (event "Socket error while sending BAD_REQUEST, exiting" >> setFieldValue "error" err)
            | err ->
              logger.verbose (event "Socket error while processing request, exiting" >> setFieldValue "error" err)
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

  open System.Reflection

  let resolveDirectory homeDirectory =
    match homeDirectory with
#if NETSTANDARD1_5
    | None   -> Path.GetDirectoryName(typeof<ScanResult>.GetTypeInfo().Assembly.Location)
#else
    | None   -> Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
#endif
    | Some s -> s