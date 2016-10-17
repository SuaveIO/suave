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

  open Http2
  open Hpack

  let http2readRequest (matchedBinding: HttpBinding) (cn2 : Http2Connection) : SocketOp<HttpRequest> = 
    socket {

      // Read headers till you get an end header
      let loop = ref true
      let payloads = List<byte[]>()
      while !loop do
        let! (header,Headers (priorityOption, bytes)) = cn2.read ()
        payloads.Add bytes
        loop := testEndHeader header.flags
      // Concatenate payloads
      let payload = Array.concat payloads
      // A receiving endpoint reassembles the header block by concatenating its fragments and then 
      // decompresses the block to reconstruct the header list.
      let headerList = decodeHeader (cn2.decodeDynamicTable) (payload)

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
          headers         = []
          rawForm         = Array.empty
          rawQuery        = ""
          files           = []
          multiPartFields = []
          trace           = TraceHeader.empty }

      return r
      }



  let procQueue = new BlockingQueueAgent<HttpRequest>()

  let http2queue (r:HttpRequest) =
    Async.Start (procQueue.AsyncAdd r)

  let http2writeLoop = 
     socket {
       while true do
         let! a = SocketOp.ofAsync <| procQueue.AsyncGet()
         // apply the web part .. get a response back.
         // transform the response to http2 frames.
         // send the frames
         do ()
       }

  let http2Loop (ctxOuter : HttpContext) (consumer : WebPart) = 
    let cn2 = new Http2Connection(ctxOuter.connection.transport)
    socket {
      // start reading frames
      // use a different dispatcher; regular http 1.x uses a serial dispatcher (per connection)
      // the client will send the http2 client connection preface
      // and the server will send a potentially empty SETTINGS frame as its first frame.
      let! (header,Settings (flag,settings)) = cn2.read ()
      // figure out what to do with those headers
      while true do
        let! request = http2readRequest ctxOuter.runtime.matchedBinding cn2
        // queue for processing
        do http2queue request // will queue the request for processing
      }

  let httpLoop (ctxOuter : HttpContext) (consumer : WebPart) =

    let runtime = ctxOuter.runtime

    let facade = new ConnectionFacade(ctxOuter)

    let rec loop (_ : HttpContext) = async {

      let verbose  = Log.verbose runtime.logger "Suave.Web.httpLoop.loop" TraceHeader.empty
      let verbosef = Log.verbosef runtime.logger "Suave.Web.httpLoop.loop" TraceHeader.empty

      verbose "-> processor"

      verbose "reading first line of request"
      let! (Choice1Of2 firstLine) = facade.readLine 

      if firstLine.Equals("PRI * HTTP/2.0") then
        printfn "we have http2"
        // read the remaining octects "\r\nSM\r\n\r\n"
        let! a = facade.skipLine
        let! a = facade.skipLine
        let! a = facade.skipLine
        //inititate http2 looop
        let! a = http2Loop ctxOuter consumer
        return ()
      
      // else regular http loop
      let! result' = facade.processRequest firstLine

      verbose "<- processor"

      match result' with
      | Choice1Of2 result ->
        match result with
        | None -> verbose "'result = None', exiting"
        | Some ctx ->
          // Check for Upgrade header.
          (*
          A server that supports HTTP/2 accepts the upgrade with a 101 (Switching Protocols) response. After the empty line 
          that terminates the 101 response, the server can begin sending HTTP/2 frames. These frames MUST include a response
          to the request that initiated the upgrade.
          *)

          match ctx.request.headers %% "upgrade" with
          | Choice1Of2 "h2c" -> 
            let! _ = HttpOutput.run Intermediate.CONTINUE ctx
            verbose "sent 100-continue response"
            let! a = http2Loop ctx consumer
            return ()
          | Choice1Of2 "h2" ->
             // implies TLS
            if ctx.runtime.matchedBinding.scheme <> Http.Protocol.HTTP then
              let! _ = HttpOutput.run Intermediate.CONTINUE ctx
              verbose "sent 100-continue response"
              let! a = http2Loop ctx consumer
              return ()
             else
               failwith "TLS required"
          | Choice1Of2 _ -> 
            failwith "Invalid identifier"
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

