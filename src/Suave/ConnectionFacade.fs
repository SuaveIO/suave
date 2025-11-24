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

  let httpOutput = new HttpOutput(connection,runtime)

  let reader = connection.reader

  let mutable files = List<HttpUpload>()
  let mutable multiPartFields = List<string*string>()
  let mutable _rawForm : byte array = [||]

  let readFilePart boundary (headerParams : Dictionary<string,string>) fieldName contentType : SocketOp<HttpUpload option> =
    // Split the file-reading work into a separate minimal task so the main task only awaits once
    let readFileDataHelper () =
      let tempFilePath = Path.GetTempFileName()
      let tempFile = new FileStream(tempFilePath, FileMode.Truncate)
      // Convert the SocketOp to a Task and handle it without nested task CE
      let readOp = reader.readUntilPattern (ASCII.bytes (eol + boundary)) (fun x y ->
        do tempFile.Write(x.Span.Slice(0,y))
        Continue 0)
      let readTask = readOp.AsTask()
      
      // Use Task.ContinueWith to avoid complex resumable state machine in a task CE
      readTask.ContinueWith(fun (ante: System.Threading.Tasks.Task<_>) ->
        try
          let result = ante.Result
          let fileLength = tempFile.Length
          tempFile.Dispose()
          (tempFilePath, result, fileLength, false, "")
        with ex ->
          tempFile.Dispose()
          (tempFilePath, Unchecked.defaultof<_>, 0L, true, ex.Message)
      ) : Task<_>

    // Main task: convert to continuation to avoid complex resumable state machine
    let mainTask =
      readFileDataHelper().ContinueWith(fun (ante: System.Threading.Tasks.Task<_>) ->
        let (tempFilePath, readResult, fileLength, exceptionOccurred, exceptionMsg) = ante.Result
        
        // Convert result to Task for synchronous processing
        if exceptionOccurred then
          try File.Delete tempFilePath with _ -> ()
          Task.FromResult(Result.Error (InputDataError (None, sprintf "Error reading file part: %s" exceptionMsg)))
        else
          match readResult with
          | Result.Error e ->
            try File.Delete tempFilePath with _ -> ()
            Task.FromResult(Result.Error e)
          | Ok _ ->
            if fileLength > 0L then
              let filename =
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

              match filename with
              | Result.Error e -> Task.FromResult(Result.Error e)
              | Ok fn ->
                let upload =
                  { fieldName    = fieldName
                    fileName     = fn
                    mimeType     = contentType
                    tempFilePath = tempFilePath }
                Task.FromResult(Ok (Some upload))
            else
              try File.Delete tempFilePath with _ -> ()
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

  member this.parsePostData maxContentLength (contentLengthHeader : Choice<string,_>) (contentTypeHeader:Choice<string,_>) : SocketOp<unit> =
    socket {
      match contentLengthHeader with
      | Choice1Of2 contentLengthString ->
        let contentLength = Convert.ToInt32 contentLengthString

        if contentLength > maxContentLength then
          return! SocketOp.abort(InputDataError (Some 413, "Payload too large"))
        else
          match contentTypeHeader with
          | Choice1Of2 ce when String.startsWith "application/x-www-form-urlencoded" ce ->
            let! rawForm = getRawPostData contentLength
            _rawForm <- rawForm
          | Choice1Of2 ce when String.startsWith "multipart/form-data" ce ->
            let boundary = "--" + parseBoundary ce
            do! parseMultipart boundary
          | Choice1Of2 _ | Choice2Of2 _ ->
            let! rawForm = getRawPostData contentLength
            _rawForm <- rawForm

          return ()
      | Choice2Of2 _ -> return ()
    }

  /// Process the request, reading as it goes from the incoming 'stream', yielding a HttpRequest
  /// when done
  member this.readRequest () = socket {

    let! firstLine = reader.readLine()

    let! (rawMethod, path, rawQuery, httpVersion) =
      parseUrl firstLine
      @|! (None, "Invalid first line")

    let! headers = reader.readHeaders()

    // Respond with 400 Bad Request as
    // per http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
    let! rawHost = (headers @@ "host") @|! (None, "Missing 'Host' header")

    if headers @@ "expect" = Choice1Of2 "100-continue" then
      let! _ = SocketOp.ofTask (httpOutput.run HttpRequest.empty Intermediate.CONTINUE)
      ()

    do! this.parsePostData runtime.maxContentLength (headers @@ "content-length") (headers @@ "content-type")

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
    
    // Clear form data before exit
    files <- List<_>()
    multiPartFields <- List<_>()
    _rawForm <- [||]

    return request
  }

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
      let! reqRes = (this.readRequest()).AsTask()
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
      // Shutdown transport FIRST to unblock any waiting reads in readLoop
      // This prevents the readLoop from being stuck in transport.read() when we set running=false
      connection.transport.shutdown()
      
      // Now stop the reader - the transport shutdown will have unblocked any pending reads
      if reader.isDirty then
        reader.stop()
      
      // Clear the line buffer to prevent data leakage and ensure clean state for reuse
      Array.Clear(connection.lineBuffer)
      connection.lineBufferCount <- 0
      
      // Note: Push() now notifies the tracker that connection is being returned
      connectionPool.Push(this)

  // Return the lineBuffer to the ArrayPool when the connection is disposed
  interface IDisposable with
    member this.Dispose() =
      System.Buffers.ArrayPool<byte>.Shared.Return(connection.lineBuffer, true)

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
      Console.WriteLine("{0} connected. Now has {1} connected", clientIp, tracker.ActiveConnectionCount)
    connection.socketBinding <- binding
    try
      try
        // Start read loop in background - use _ignore to suppress async warning
        let readTask = reader.readLoop()
        let! loopRes = this.requestLoop()
        match loopRes with
        | Ok () -> ()
        | Result.Error err ->
          do Console.WriteLine(sprintf "Error: %A" err)
      with
        | ex ->
          do Console.WriteLine("Error: " + ex.Message)
    finally
      do this.shutdown()
      if Globals.verbose then
        do Console.WriteLine("Disconnected {0}. {1} connected.", clientIp, tracker.ActiveConnectionCount)
  }
