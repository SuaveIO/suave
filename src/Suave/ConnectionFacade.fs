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

        let! firstLineRes = reader.readLine()
        match firstLineRes with
        | Result.Error e -> return Result.Error e
        | Ok firstLine ->

        match parseUrl firstLine with
        | Choice2Of2 _ ->
          return Result.Error (InputDataError (None, "Invalid first line"))
        | Choice1Of2 (rawMethod, path, rawQuery, httpVersion) ->

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

  member this.processRequest () =
    task {
      let! reqRes = this.readRequest()
      match reqRes with
      | Result.Error err ->
        // Couldn't parse HTTP request; answering with BAD_REQUEST and closing the connection.
        return! this.exitHttpLoopWithError err
      | Ok request ->
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
    // Set the connection ID on the reader for consistent logging
    let mutable readTask : Task<Result<unit,Error>> = null
    try
      try
        reader.init()
        readTask <- reader.readLoop()
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
      // First phase: stop reader and transport (this unblocks readTask)
      this.shutdown()
    
    // Wait for readTask to complete BEFORE recycling the connection
    // This is critical: we must ensure readLoop has finished (and called pipe.Writer.Complete())
    // before we reset the pipe and push the connection back to the pool
    if readTask <> null then
      try
        // Use a timeout to avoid hanging forever if something goes wrong
        let! completed = Task.WhenAny(readTask, Task.Delay(1000))
        if not (Object.ReferenceEquals(completed, readTask)) then
          if Globals.verbose then
            Console.WriteLine("[Conn:{0}] accept: readTask did not complete within timeout", connectionId)
      with ex ->
        if Globals.verbose then
          Console.WriteLine("[Conn:{0}] accept: error waiting for readTask: {1}", connectionId, ex.Message)
    
    // Second phase: reset pipe and recycle connection (only after readTask is done)
    this.recycleConnection()
    if Globals.verbose then
        do Console.WriteLine("[Conn:{0}] accept:Disconnected {1}. {2} connected.", connectionId, clientIp, tracker.ActiveConnectionCount)
  }
