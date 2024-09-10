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
open Suave.Logging
open Suave.Sockets
open Suave.Sockets.Control
open Suave.Sockets.SocketOp.Operators
open Suave.Utils.Bytes
open System.Threading
open System.Threading.Tasks

type ConnectionFacade(connection: Connection, runtime: HttpRuntime, connectionPool: ConcurrentPool<ConnectionFacade>, cancellationToken: CancellationToken,webpart:WebPart) =

  let httpOutput = new HttpOutput(connection,runtime)

  let reader = connection.reader

  let files = List<HttpUpload>()
  let multiPartFields = List<string*string>()
  let mutable _rawForm : byte array = [||]

  let readFilePart boundary (headerParams : Dictionary<string,string>) fieldName contentType = socket {
    let tempFilePath = Path.GetTempFileName()
    use tempFile = new FileStream(tempFilePath, FileMode.Truncate)
    let! a =
      reader.readUntilPattern (ASCII.bytes (eol + boundary)) (fun x y ->
          do tempFile.Write(x.Span.Slice(0,y))
          Continue 0)
    let fileLength = tempFile.Length
    tempFile.Dispose()
    if fileLength > 0L then
      let! filename =
        match headerParams.TryLookup "filename*" with
        | Choice1Of2 _filename ->
          let ix = _filename.IndexOf "''"
          if ix > 0 then
            let enc = _filename.Substring(0,ix).ToLowerInvariant()
            if enc = "utf-8" then
              let filename = Net.WebUtility.UrlDecode(_filename.Substring(ix + 2))
              SocketOp.mreturn (filename)
            else
               SocketOp.abort (InputDataError (None, "Unsupported filename encoding: '" + enc + "'"))
          else
            SocketOp.abort (InputDataError (None, "Invalid filename encoding"))
        | Choice2Of2 _ ->
          (headerParams.TryLookup "filename" |> Choice.map (String.trimc '"'))
          @|! (None, "Key 'filename' was not present in 'content-disposition'")

      let upload =
        { fieldName    = fieldName
          fileName     = filename
          mimeType     = contentType
          tempFilePath = tempFilePath }

      return Some upload
    else
      File.Delete tempFilePath
      return None
    }

  let parseMultipartMixed fieldName boundary : SocketOp<unit> =
    let rec loop () = socket {
      let! firstLine = reader.readLine()

      if firstLine.Equals("--") then
        return ()
      else
        let! partHeaders = reader.readHeaders()

        let! (contentDisposition : string) =
          (partHeaders %% "content-disposition")
          @|! (None, "Missing 'content-disposition'")

        match partHeaders %% "content-type" with
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
          multiPartFields.Add (fieldName, Encoding.UTF8.GetString(byts, 0, byts.Length))
          return! loop ()
      }
    loop ()

  /// Parses multipart data from the stream, feeding it into the HttpRequest's property Files.
  let parseMultipart (boundary:string) : SocketOp<unit> =
    let parsePart () = socket {
        let! partHeaders = reader.readHeaders()

        let! (contentDisposition : string) =
          (partHeaders %% "content-disposition")
          @|! (None, "Missing 'content-disposition'")

        let headerParams = headerParams contentDisposition

        let! _ =
          (headerParams.TryLookup "form-data" |> Choice.map (String.trimc '"'))
          @|! (None, "Key 'form-data' was not present in 'content-disposition'")

        let fieldName =
          match headerParams.TryLookup "name" |> Choice.map (String.trimc '"') with
          | Choice1Of2 s -> s
          | _ -> failwith "Key 'name' was not present in 'content-disposition'"

        match partHeaders %% "content-type" with
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
          let str =  Encoding.UTF8.GetString(byts, 0, byts.Length)
          multiPartFields.Add(fieldName,str)
          return ()
      }

    let something () = socket{
        do! parsePart ()
        let! line = reader.readLine()
        return line
    }

    let firstPart(boundary) = socket{
      let! firstLine = reader.readLine()
      if firstLine<>boundary then
        failwithf "Invalid multipart format: expected boundary '%s' got '%s'" boundary firstLine
    }

    let secondPart () = task{
      let mutable parsing = true
      let mutable error = false
      let result = ref (Ok())
      while parsing && not error && not(cancellationToken.IsCancellationRequested) do
        match! something () with
        | Ok line ->
          if line.StartsWith("--") then
            parsing <- false
          else if line <> String.Empty then
            failwith "Invalid multipart format"
        | Result.Error e ->
          result := Result.Error e
          error <- true
      if error then
        return !result
      else
        return Ok()
    }

    socket {
      do! firstPart (boundary)
      do! secondPart ()
      return ()
    }

  /// Reads raw POST data
  let getRawPostData contentLength =
    task {
      let offset = ref 0
      let rawForm = Array.zeroCreate contentLength
      do! reader.readPostData contentLength (fun a count ->
          let source = a.Span.Slice(0,count)
          let target = new Span<byte>(rawForm,!offset,count)
          source.CopyTo(target)
          offset := !offset + count)
      return Ok (rawForm)
    }

  member val Connection = connection with get,set
  member val Runtime = runtime with get,set

  member (*inline*) this.parsePostData maxContentLength (contentLengthHeader : Choice<string,_>) (contentTypeHeader:Choice<string,_>) : Task<Result<unit,Error>>=
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
  member (*inline*) this.readRequest () = socket {

    let! firstLine = reader.readLine()

    let! (rawMethod, path, rawQuery, httpVersion) =
      parseUrl firstLine
      @|! (None, "Invalid first line")

    let! headers = reader.readHeaders()

    // Respond with 400 Bad Request as
    // per http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
    let! rawHost = headers %% "host" @|! (None, "Missing 'Host' header")

    if headers %% "expect" = Choice1Of2 "100-continue" then
      let! _ = httpOutput.run HttpRequest.empty Intermediate.CONTINUE
      ()

    do! this.parsePostData runtime.maxContentLength (headers %% "content-length") (headers %% "content-type")

    let request =
      { httpVersion      = httpVersion
        binding          = runtime.matchedBinding
        rawPath          = path
        rawHost          = rawHost
        rawMethod        = rawMethod
        headers          = headers
        rawForm          = _rawForm
        rawQuery         = rawQuery
        files            = Seq.toList files
        multiPartFields  = Seq.toList multiPartFields
        trace            = TraceHeader.parseTraceHeaders headers }
    
    // Clear form data before exit
    files.Clear()
    multiPartFields.Clear()
    _rawForm <- [||]

    return request
  }

  member this.exitHttpLoopWithError (err:Error) = task{
      match err with
      | InputDataError (None, msg) ->
        match! httpOutput.run HttpRequest.empty (RequestErrors.BAD_REQUEST msg) with
        | _ -> ()

      | InputDataError (Some status,msg) ->
        match Http.HttpCode.tryParse status with 
        | (Choice1Of2 statusCode) ->
          match! httpOutput.run HttpRequest.empty (Response.response statusCode (Encoding.UTF8.GetBytes msg)) with
          | _ -> ()
        | (Choice2Of2 err) ->
          match! httpOutput.run HttpRequest.empty (RequestErrors.BAD_REQUEST msg) with
          | _ -> ()
      | err -> ()
      return Ok(false)
    }

  member this.processRequest () =
    task {
      match! this.readRequest () with
      | Result.Error err ->
        // Couldn't parse HTTP request; answering with BAD_REQUEST and closing the connection.
        return! this.exitHttpLoopWithError err
      | Ok request ->
        try
          match! httpOutput.run request webpart  with
          | Result.Error err ->
            return Result.Error err 
          | Ok keepAlive ->
            return Ok (keepAlive)
        with ex ->
          return Result.Error (Error.ConnectionError ex.Message)
    }

  member this.shutdown() =
    connection.lineBufferCount <- 0
    connection.transport.shutdown()
    connectionPool.Push(this)

  /// The request loop initialises a request with a processor to handle the
  /// incoming stream and possibly pass the request to the web parts, a protocol,
  /// a web part, an error handler and a Connection to use for read-write
  /// communication -- getting the initial request stream.
  member (*inline*) this.requestLoop ()=
    task {
      let flag = ref true
      let result = ref (Ok ())
      while !flag && not (cancellationToken.IsCancellationRequested) do
        let! b = this.processRequest ()
        match b with
        | Ok b ->
          flag := b
        | Result.Error e ->
          flag := false
          result := Result.Error e
      reader.stop()
      return result
      }

  member this.accept(binding) = task{
    Interlocked.Increment Globals.numberOfClients |> ignore
    let clientIp = (binding.ip.ToString())
    if Globals.verbose then
      Console.WriteLine("{0} connected. Now has {1} connected", clientIp,(!Globals.numberOfClients))
    connection.socketBinding <- binding
    try
      let task = Task.Factory.StartNew(reader.readLoop,cancellationToken)
      let! a = this.requestLoop()
      ()
    with
      | ex ->
        if reader.isDirty then
          reader.stop()
        do Console.WriteLine("Error: " + ex.Message)
    Interlocked.Decrement(Globals.numberOfClients) |> ignore
    if Globals.verbose then
      do Console.WriteLine("Disconnected {0}. {1} connected.", clientIp,(!Globals.numberOfClients))
    do this.shutdown()
  }
