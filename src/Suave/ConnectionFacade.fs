#nowarn "40"
#nowarn "3370"
namespace Suave

open System
open System.IO
open System.Collections.Generic
open System.Text

open Suave
open Suave.Utils
open Suave.Utils.Parsing
open Suave.Sockets
open Suave.Sockets.Control
open Suave.Sockets.SocketOp.Operators
open Suave.Utils.Bytes
open System.Threading
open System.Threading.Tasks
open ConnectionHealthChecker

type ConnectionFacade(connection: Connection, runtime: HttpRuntime, connectionPool: ConcurrentPool<ConnectionFacade>, tracker: ActiveConnectionTracker<ConnectionFacade>, cancellationToken: CancellationToken, webpart: WebPart) =

  static let mutable connectionIdCounter = 0L
  let connectionId = Interlocked.Increment(&connectionIdCounter)

  let httpOutput = new HttpOutput(connection,runtime)

  let reader = connection.reader

  // Per-connection pooled collections. These are allocated once when the ConnectionFacade
  // is created and then cleared (not reallocated) at the start of every request. Because a
  // ConnectionFacade processes one request at a time on its connection, this is safe.
  // Webparts must not retain references to a request's collections beyond the request's
  // own task — Suave never has and never did promise that.
  let files = List<HttpUpload>()
  let multiPartFields = List<string*string>()
  let requestHeaders = List<string*string>()
  let mutable _rawForm : byte array = [||]

  let readFilePart boundary (headerParams : Dictionary<string,string>) fieldName contentType : SocketOp<HttpUpload option> =
    // Extract the filename from header params BEFORE opening any stream so that sink
    // implementations receive the final file name when their factory is called.
    let filenameResult =
      match headerParams.TryLookup "filename*" with
      | Choice1Of2 _filename ->
        let ix = _filename.IndexOf "''"
        if ix > 0 then
          let enc = _filename.Substring(0,ix).ToLowerInvariant()
          if enc = "utf-8" then
            let filename = Net.WebUtility.UrlDecode(_filename.Substring(ix + 2))
            Ok filename
          else
            Result.Error (InputDataError (None, "Unsupported filename encoding: '" + enc + "'"))
        else
          Result.Error (InputDataError (None, "Invalid filename encoding"))
      | Choice2Of2 _ ->
        match headerParams.TryLookup "filename" |> Choice.map (String.trimc '"') with
        | Choice1Of2 fn -> Ok fn
        | Choice2Of2 _ -> Result.Error (InputDataError (None, "Key 'filename' was not present in 'content-disposition'"))

    match filenameResult with
    | Result.Error e ->
      ValueTask<Result<HttpUpload option,Error>>(Task.FromResult(Result.Error e))
    | Ok filename ->

    // Build a FilePartWriter: either from the user-supplied sink or the built-in temp-file writer.
    let writer =
      match runtime.filePartSink with
      | Some sink ->
        sink fieldName filename contentType
      | None ->
        let tempFilePath = Path.GetTempFileName()
        let tempFile = new FileStream(tempFilePath, FileMode.Truncate) :> IO.Stream
        { stream    = tempFile
          onSuccess = fun () ->
            { fieldName    = fieldName
              fileName     = filename
              mimeType     = contentType
              tempFilePath = tempFilePath }
          onError   = fun () ->
            try File.Delete tempFilePath with _ -> () }

    // Read the part body into the writer's stream, tracking the number of bytes written.
    let readFileDataHelper () =
      let mutable bytesWritten = 0L
      let readOp = reader.readUntilPattern (ASCII.bytes (eol + boundary)) (fun x y ->
        do writer.stream.Write(x.Span.Slice(0,y))
        bytesWritten <- bytesWritten + int64 y
        Continue 0)
      let readTask = readOp.AsTask()

      readTask.ContinueWith(fun (ante: System.Threading.Tasks.Task<_>) ->
        try
          let result = ante.Result
          writer.stream.Dispose()
          (result, bytesWritten, false, "")
        with ex ->
          writer.stream.Dispose()
          (Unchecked.defaultof<_>, 0L, true, ex.Message)
      ) : Task<_>

    let mainTask =
      readFileDataHelper().ContinueWith(fun (ante: System.Threading.Tasks.Task<_>) ->
        let (readResult, bytesWritten, exceptionOccurred, exceptionMsg) = ante.Result

        if exceptionOccurred then
          writer.onError()
          Task.FromResult(Result.Error (InputDataError (None, $"Error reading file part: {exceptionMsg}")))
        else
          match readResult with
          | Result.Error e ->
            writer.onError()
            Task.FromResult(Result.Error e)
          | Ok _ ->
            if bytesWritten > 0L then
              let upload = writer.onSuccess()
              Task.FromResult(Ok (Some upload))
            else
              writer.onError()
              Task.FromResult(Ok None)
      ).Unwrap()

    ValueTask<Result<HttpUpload option,Error>>(mainTask)

  let parseMultipartMixed fieldName boundary : SocketOp<unit> =
    let rec loop () : SocketOp<unit> = socket {
      let! firstLine = reader.readLine()

      if firstLine.Equals("--") then
        return ()
      else
        let! partHeaders = reader.readHeaders()

        let! (contentDisposition : string) =
          (partHeaders @@ "content-disposition")
          @|! (None, "Missing 'content-disposition'")

        match partHeaders @@ "content-type" with
        | Choice1Of2 contentType ->
          let headerParams = headerParams contentDisposition
          let! res = readFilePart boundary headerParams fieldName contentType
          match res with
          | Some upload ->
            files.Add(upload)
            return! loop ()
          | None ->
            return! loop ()

        | Choice2Of2 _ ->
          use mem = new MemoryStream()
          let! a =
            reader.readUntilPattern (ASCII.bytes(eol + boundary)) (fun x y -> 
                mem.Write(x.Span.Slice(0,y))
                Continue 0
              )
          let byts = mem.ToArray()
          multiPartFields.Add (fieldName, Globals.UTF8.GetString(byts, 0, byts.Length))
          return! loop ()
      }
    loop ()

  /// Parses multipart data from the stream, feeding it into the HttpRequest's property Files.
  let parseMultipart (boundary:string) : SocketOp<unit> =
    let parsePart () = socket {
        let! partHeaders = reader.readHeaders()

        let! (contentDisposition : string) =
          (partHeaders @@ "content-disposition")
          @|! (None, "Missing 'content-disposition'")

        let headerParams = headerParams contentDisposition

        let! _ =
          (headerParams.TryLookup "form-data" |> Choice.map (String.trimc '"'))
          @|! (None, "Key 'form-data' was not present in 'content-disposition'")

        let! fieldName =
          (headerParams.TryLookup "name" |> Choice.map (String.trimc '"'))
          @|! (None, "Key 'name' was not present in 'content-disposition'")

        match partHeaders @@ "content-type" with
        | Choice1Of2 x when String.startsWith "multipart/mixed" x ->
          let subboundary = "--" + parseBoundary x
          do! parseMultipartMixed fieldName subboundary
          let a = reader.skip (boundary.Length)
          return ()

        | Choice1Of2 contentType when headerParams.ContainsKey "filename" ->
          let! res = readFilePart boundary headerParams fieldName contentType
          res |> Option.iter files.Add

        | Choice1Of2 _ | Choice2Of2 _ ->
          use mem = new MemoryStream()
          let! a =
            reader.readUntilPattern (ASCII.bytes (eol + boundary)) (fun x y ->
              mem.Write(x.Span.Slice(0,y))
              Continue 0
            )
          let byts = mem.ToArray()
          let str =  Globals.UTF8.GetString(byts, 0, byts.Length)
          multiPartFields.Add(fieldName,str)
          return ()
      }

    let nexPart () = socket{
        do! parsePart ()
        let! line = reader.readLine ()
        return line
    }

    let parsePartLoop () = task{
      let mutable parsing = true
      let mutable error = false
      let mutable result =  Ok()
      while parsing && not error && not(cancellationToken.IsCancellationRequested) do
        let! nextPartResult = (nexPart()).AsTask()
        match nextPartResult with
        | Ok line ->
          if line.StartsWith("--") then
            // reached end boundary
            parsing <- false
          else if line <> String.Empty then
            result <- Result.Error(InputDataError (Some 413, "Invalid multipart format"))
            error <- true
        | Result.Error e ->
          result <- Result.Error e
          error <- true
      if error then
        return result
      else
        return Ok()
    }

    socket {
      let! firstLine = reader.readLine()
      if firstLine<>boundary then
        return! SocketOp.abort (InputDataError (Some 413, "Invalid multipart format"))
      else
        return! SocketOp.ofTask (parsePartLoop ())
    }

  /// Reads raw POST data
  let getRawPostData contentLength =
    ValueTask<Result<byte[],Error>>(
      task {
        let mutable offset = 0
        let rawForm = Array.zeroCreate contentLength
        do! reader.readPostData contentLength (fun a count ->
            let source = a.Span.Slice(0,count)
            let target = new Span<byte>(rawForm, offset, count)
            source.CopyTo(target)
            offset <- offset + count)
        return Ok (rawForm)
      })

  member val Connection = connection with get,set
  member val Runtime = runtime with get,set
  member val ConnectionId = connectionId with get
  /// The web part used to serve requests on this connection. Exposed so that
  /// protocol upgrade handlers (e.g. h2c) can drive their own request loop
  /// using the same web part as the HTTP/1.1 keep-alive loop.
  member val Webpart : WebPart = webpart with get

  /// Optional hook invoked when an incoming HTTP/1.1 request asks to upgrade
  /// to HTTP/2 cleartext (h2c) per RFC 7540 §3.2. When set, the hook owns
  /// the connection from this point forward (it is responsible for writing
  /// the 101 Switching Protocols response and then driving the HTTP/2
  /// protocol). Returning `Ok false` terminates the HTTP/1.1 keep-alive
  /// loop; any error is propagated to `accept`.
  ///
  /// The hook is registered by `Suave.Http2.H2cUpgrade.register` from
  /// `Tcp.fs` at server startup so that `ConnectionFacade.fs` need not
  /// take a forward dependency on `Http2.fs` (which is compiled later).
  static member val Http2UpgradeHandler
    : (ConnectionFacade -> HttpRequest -> Task<Result<bool, Error>>) option = None
    with get, set

  /// Optional hook invoked when an incoming cleartext connection opens with
  /// the HTTP/2 connection preface ("PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n") per
  /// RFC 7540 §3.4 (prior-knowledge mode). When set, the hook owns the
  /// connection from this point forward: it must consume the remainder of
  /// the preface (the request line "PRI * HTTP/2.0\r\n" has already been
  /// read by `readRequest`) and drive the HTTP/2 protocol. Returning
  /// `Ok false` terminates the HTTP/1.1 keep-alive loop; any error is
  /// propagated to `accept`.
  ///
  /// The hook is registered by `Suave.Http2.H2cPriorKnowledge.register`
  /// from `Tcp.fs` at server startup so that `ConnectionFacade.fs` need
  /// not take a forward dependency on `Http2.fs` (which is compiled later).
  static member val Http2PriorKnowledgeHandler
    : (ConnectionFacade -> Task<Result<bool, Error>>) option = None
    with get, set

  member this.parsePostData maxContentLength (contentLengthHeader : Choice<string,_>) (contentTypeHeader:Choice<string,_>) : SocketOp<unit> =
    // Hot-path version: a direct task { } that hand-binds Result values instead of routing
    // through the socket { } CE. Only the cold multipart path still uses socket { }.
    ValueTask<Result<unit,Error>>(
      task {
        match contentLengthHeader with
        | Choice2Of2 _ -> return Ok ()
        | Choice1Of2 contentLengthString ->
          let contentLength = Convert.ToInt32 contentLengthString
          if contentLength > maxContentLength then
            return Result.Error (InputDataError (Some 413, "Payload too large"))
          else
            match contentTypeHeader with
            | Choice1Of2 ce when String.startsWith "multipart/form-data" ce ->
              let boundary = "--" + parseBoundary ce
              // Cold path: keep the socket { } CE for multipart; we don't try to optimize it here.
              let! mp = (parseMultipart boundary).AsTask()
              return mp
            | _ ->
              // application/x-www-form-urlencoded and everything else read raw bytes.
              let! raw = (getRawPostData contentLength).AsTask()
              match raw with
              | Ok rawForm ->
                _rawForm <- rawForm
                return Ok ()
              | Result.Error e ->
                return Result.Error e
      })

  member this.readRequest () : SocketOp<HttpRequest> =
    // Steady-state hot path. We deliberately avoid the socket { } CE here because each
    // of its binds wraps a task in a ValueTask and forces .AsTask() boxing on every let!.
    // Instead we run a single task { } and hand-bind the Result returned by each step.
    ValueTask<Result<HttpRequest,Error>>(
      task {
        // Clear pooled per-connection collections at the start of every request rather than
        // allocating fresh ones. The collections live on the ConnectionFacade and are reset
        // here, before the new request is parsed in.
        requestHeaders.Clear()
        files.Clear()
        multiPartFields.Clear()
        _rawForm <- [||]

        let! firstLineRes = reader.readRequestLine()
        match firstLineRes with
        | Result.Error e -> return Result.Error e
        | Ok (rawMethod, path, rawQuery, httpVersion) ->

        // RFC 7230 §3.1.1: a conforming HTTP/1.x or HTTP/2 request line ends
        // in an "HTTP/x.y" version token. If the parsed token does not start
        // with "HTTP/", the bytes we just read are not an HTTP/1.x request
        // and the connection is most plausibly a misbehaving HTTP/2 client
        // (e.g. h2spec http2/3.5/2 sends the literal "INVALID CONNECTION
        // PREFACE\r\n\r\n" on a fresh TCP connection without negotiating an
        // h2c upgrade). Send a best-effort HTTP/2 GOAWAY(PROTOCOL_ERROR)
        // so HTTP/2 clients observe the protocol error, then close the
        // connection silently — the `_ -> ()` branch of
        // `exitHttpLoopWithError` suppresses the 400 response that would
        // otherwise be sent back as HTTP/1.1 bytes (and which h2spec
        // misparses as a truncated HTTP/2 frame, reporting "unexpected EOF").
        if not (httpVersion.StartsWith("HTTP/", StringComparison.Ordinal)) then
          let goAwayBytes : byte[] =
            // 9-byte frame header (length=8, type=GOAWAY=7, flags=0, stream id=0)
            // followed by an 8-byte payload (last_stream_id=0, error_code=PROTOCOL_ERROR=1).
            [| 0uy; 0uy; 8uy
               7uy
               0uy
               0uy; 0uy; 0uy; 0uy
               0uy; 0uy; 0uy; 0uy
               0uy; 0uy; 0uy; 1uy |]
          try
            let writeVt = connection.transport.write(Memory<byte>(goAwayBytes))
            let! _ = writeVt.AsTask()
            let flushVt = connection.transport.flush()
            let! _ = flushVt.AsTask()
            ()
          with ex ->
            // Best-effort: the peer is not necessarily speaking HTTP/2, so a
            // failed write is expected for plain noise. Surface it at debug
            // level so future preface-related regressions are observable.
            try eprintfn "Suave.Http2: failed to write GOAWAY for invalid HTTP version: %s" ex.Message
            with _ -> ()
          return Result.Error
            (ConnectionError ("Invalid HTTP version: " + httpVersion))
        else

        // RFC 7540 §3.4: prior-knowledge HTTP/2 cleartext clients open the
        // connection with the 24-byte preface beginning with the literal
        // request line "PRI * HTTP/2.0\r\n". No legitimate HTTP/1.1 request
        // can have this exact (method, target, version) triple, so it is a
        // reliable in-band signal that we should switch to HTTP/2 framing.
        // We surface it to `processRequest` as a sentinel `HttpRequest` —
        // headers are not read (the next 8 preface bytes "\r\nSM\r\n\r\n"
        // are consumed by the prior-knowledge handler, not by the HTTP/1.1
        // header parser).
        if ConnectionFacade.isHttp2PriorKnowledgePreface rawMethod path httpVersion then
          let request =
            { httpVersion      = httpVersion
              binding          = runtime.matchedBinding
              rawPath          = path
              rawHost          = ""
              rawMethod        = rawMethod
              headers          = requestHeaders
              rawForm          = [||]
              rawQuery         = rawQuery
              files            = files
              multiPartFields  = multiPartFields }
          return Ok request
        else

        let! headersRes = reader.readHeadersInto(requestHeaders)
        match headersRes with
        | Result.Error e -> return Result.Error e
        | Ok headers ->

        match headers @@ "host" with
        | Choice2Of2 _ ->
          return Result.Error (InputDataError (None, "Missing 'Host' header"))
        | Choice1Of2 rawHost ->

        // 100-continue handling
        if headers @@ "expect" = Choice1Of2 "100-continue" then
          let! _ = httpOutput.run HttpRequest.empty Intermediate.CONTINUE
          ()

        let! postRes =
          this.parsePostData
            runtime.maxContentLength
            (headers @@ "content-length")
            (headers @@ "content-type")
        match postRes with
        | Result.Error e -> return Result.Error e
        | Ok () ->

        let request =
          { httpVersion      = httpVersion
            binding          = runtime.matchedBinding
            rawPath          = path
            rawHost          = rawHost
            rawMethod        = rawMethod
            headers          = headers
            rawForm          = _rawForm
            rawQuery         = rawQuery
            files            = files
            multiPartFields  = multiPartFields }

        return Ok request
      })

  member this.exitHttpLoopWithError (err:Error) = task{
      match err with
      | InputDataError (None, msg) ->
        let! _ = httpOutput.run HttpRequest.empty (RequestErrors.BAD_REQUEST msg)
        ()

      | InputDataError (Some status,msg) ->
        match Http.HttpCode.tryParse status with 
        | (Choice1Of2 statusCode) ->
          let! _ = httpOutput.run HttpRequest.empty (Response.response statusCode (Globals.UTF8.GetBytes msg))
          ()
        | (Choice2Of2 _err) ->
          let! _ = httpOutput.run HttpRequest.empty (RequestErrors.BAD_REQUEST msg)
          ()
      | _ -> ()
      return Ok(false)
    }

  /// Returns true if the parsed request line is the literal HTTP/2 connection
  /// preface ("PRI * HTTP/2.0") that begins a prior-knowledge HTTP/2 cleartext
  /// connection (RFC 7540 §3.4). This (method, target, version) triple is
  /// impossible in any conforming HTTP/1.1 request, making it a safe in-band
  /// discriminator for protocol selection.
  static member internal isHttp2PriorKnowledgePreface (rawMethod: string)
                                                      (path: string)
                                                      (httpVersion: string) : bool =
    rawMethod = "PRI" && path = "*" && httpVersion = "HTTP/2.0"

  /// Returns true if `request` carries an HTTP/1.1 → HTTP/2 cleartext (h2c)
  /// upgrade per RFC 7540 §3.2. The client must send all of:
  ///   * `Connection: Upgrade, HTTP2-Settings` (in any order; case-insensitive)
  ///   * `Upgrade: h2c`
  ///   * `HTTP2-Settings: <base64url-encoded SETTINGS payload>`
  /// The `HTTP2-Settings` value is not validated here — that is the
  /// upgrade handler's job — we only check for its presence.
  static member internal isH2cUpgradeRequest (request: HttpRequest) : bool =
    let isUpgradeH2c =
      match request.header "upgrade" with
      | Choice1Of2 v -> String.equalsOrdinalCI (v.Trim()) "h2c"
      | Choice2Of2 _ -> false
    let connectionMentionsUpgradeAndSettings =
      match request.header "connection" with
      | Choice1Of2 v ->
        let parts =
          v.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
          |> Array.map (fun s -> s.Trim())
        let has token = parts |> Array.exists (fun p -> String.equalsOrdinalCI p token)
        has "upgrade" && has "HTTP2-Settings"
      | Choice2Of2 _ -> false
    let hasSettingsHeader =
      match request.header "http2-settings" with
      | Choice1Of2 _ -> true
      | Choice2Of2 _ -> false
    isUpgradeH2c && connectionMentionsUpgradeAndSettings && hasSettingsHeader

  member this.processRequest () =
    task {
      let! reqRes = this.readRequest()
      match reqRes with
      | Result.Error err ->
        // Couldn't parse HTTP request; answering with BAD_REQUEST and closing the connection.
        return! this.exitHttpLoopWithError err
      | Ok request ->
        // RFC 7540 §3.4: prior-knowledge HTTP/2 cleartext is signalled by
        // the connection opening with "PRI * HTTP/2.0\r\n…" — `readRequest`
        // tags such connections by returning a sentinel request with
        // method="PRI", path="*", version="HTTP/2.0". Hand it to the
        // prior-knowledge handler if one is registered. With no handler
        // present we fall through and the request will be rejected as a
        // missing-Host BAD_REQUEST further down — preserving the
        // pre-HTTP/2 behaviour for servers that haven't wired in HTTP/2.
        if ConnectionFacade.isHttp2PriorKnowledgePreface
             request.rawMethod request.rawPath request.httpVersion then
          match ConnectionFacade.Http2PriorKnowledgeHandler with
          | Some handler ->
            try
              return! handler this
            with ex ->
              return Result.Error (Error.ConnectionError ex.Message)
          | None ->
            return! this.exitHttpLoopWithError
              (InputDataError (Some 400, "HTTP/2 prior-knowledge not supported"))
        else
        // RFC 7540 §3.2: if the client requested an h2c upgrade and a handler
        // is registered, hand the connection over. The handler owns the
        // connection from here on (writes the 101, runs HTTP/2). If no handler
        // is registered the upgrade headers are simply ignored and the request
        // is processed as a normal HTTP/1.1 request — preserving backward
        // compatibility for servers that haven't wired in HTTP/2.
        //
        // The WebSocket Upgrade (`Upgrade: websocket`) is handled by user web
        // parts via the `handShake` combinator and is unaffected: we only
        // intercept requests whose `Upgrade` token is exactly `h2c`.
        match ConnectionFacade.Http2UpgradeHandler with
        | Some handler when ConnectionFacade.isH2cUpgradeRequest request ->
          try
            return! handler this request
          with ex ->
            return Result.Error (Error.ConnectionError ex.Message)
        | _ ->
          try
            let! runRes = httpOutput.run request webpart
            match runRes with
            | Result.Error err -> return Result.Error err
            | Ok keepAlive -> return Ok keepAlive
          with ex ->
            return Result.Error (Error.ConnectionError ex.Message)
    }

  member this.shutdown() =
      reader.cancelPendingReads()
      // Shutdown transport FIRST to unblock any waiting reads in readLoop
      // This prevents the readLoop from being stuck in transport.read() when we set running=false
      connection.transport.shutdown()
      reader.stop()

  member private this.recycleConnection() =
      // Clear the line buffer to prevent data leakage and ensure clean state for reuse
      Array.Clear(connection.lineBuffer)
      connection.lineBufferCount <- 0
      try connection.pipe.Writer.Complete() with _ -> ()
      try connection.pipe.Reader.Complete() with _ -> ()
      try connection.pipe.Reset() with _ -> ()
      // Note: Push() now notifies the tracker that connection is being returned
      connectionPool.Push(this)

  // Return the lineBuffer to Suave's private BufferPool when the connection is disposed.
  interface IDisposable with
    member this.Dispose() =
      Suave.Globals.BufferPool.returnArray connection.lineBuffer true

  /// The request loop initialises a request with a processor to handle the
  /// incoming stream and possibly pass the request to the web parts, a protocol,
  /// a web part, an error handler and a Connection to use for read-write
  /// communication -- getting the initial request stream.
  member this.requestLoop () =
    task {
      let mutable flag = true
      let mutable result = Ok ()
      while flag && not (cancellationToken.IsCancellationRequested) do
        let! b = this.processRequest ()
        match b with
        | Ok b ->
          flag <- b
        | Result.Error e ->
          flag <- false
          result <- Result.Error e
      return result
    }

  member this.accept(binding) = task{
    let clientIp = (binding.ip.ToString())
    if Globals.verbose then
      Console.WriteLine("[Conn:{0}] accept: {1} connected. Now has {2} connected", connectionId, clientIp, tracker.ActiveConnectionCount)
    connection.socketBinding <- binding
    try
      try
        reader.init()
        let! loopRes = this.requestLoop()
        match loopRes with
        | Ok () -> ()
        | Result.Error err ->
          if Globals.verbose then
            do Console.WriteLine($"[Conn:{connectionId}] accept: Error: {err}")
      with ex ->
        if Globals.verbose then
          do Console.WriteLine($"[Conn:{connectionId}] accept: Exception: {ex.Message}")
    finally
      // The reader pumps the inbound transport on demand from inside the request
      // loop, so by the time requestLoop has returned there is no background
      // reader task to await \u2014 just shut the transport down and recycle.
      this.shutdown()

    this.recycleConnection()
    if Globals.verbose then
        do Console.WriteLine("[Conn:{0}] accept:Disconnected {1}. {2} connected.", connectionId, clientIp, tracker.ActiveConnectionCount)
  }
