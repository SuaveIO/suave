
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
open Suave.Logging.Message
open Suave.Sockets
open Suave.Sockets.Control
open Suave.Sockets.SocketOp.Operators
open Suave.Utils.Bytes
open System.IO.Pipelines
open Suave.Utils.AsyncExtensions
open System.Buffers

type internal ScanResult = NeedMore | Found of int | Error of Error
type internal SelectResult = FailWith of Error | Continue of int
type internal SelectFunction = ReadOnlyMemory<byte> -> int -> SelectResult

type internal Reader(transport : ITransport, lineBuffer : byte array, pipe: Pipe) =

  /// Splits the segment list in two lits of ArraySegment; the first one containing a total of index bytes
  let split (bufferSequence:ReadOnlySequence<byte>) index (select:SelectFunction) : SelectResult =
    let rec loop  (bufferSequence:ReadOnlySequence<byte>) acc selectResult : SelectResult = 
      match selectResult with
      | Continue count ->
        if bufferSequence.Length = 0 then
          Continue count
        else
          let pair = bufferSequence.First
          if acc + pair.Length < index then
            let selectResult = select pair pair.Length
            match selectResult with
            | Continue _ ->
              loop (bufferSequence.Slice(pair.Length)) (acc + pair.Length) (Continue(count + acc + pair.Length))
            | FailWith s ->
              FailWith s
          elif acc + pair.Length >= index then
            let bytesRead = index - acc
            let selectResult = select pair bytesRead
            match selectResult with
            | Continue _ ->
              Continue(count + bytesRead)
              | FailWith s ->
                FailWith s
            else failwith "Suave.Web.split: invalid case"
      | FailWith s ->
        FailWith s
    loop bufferSequence 0 (Continue 0)

  let readMoreData = async {
    let buff = pipe.Writer.GetMemory()
    match! transport.read buff with
    | Choice1Of2 x ->
      pipe.Writer.Advance(x)
      let! flushResult = pipe.Writer.FlushAsync()
      return Choice1Of2()
    | Choice2Of2 error ->
      return Choice2Of2 error
    }

  let getData = async{
      let (success, result) = pipe.Reader.TryRead()
      if success then
        return result
      else
        let! result = readMoreData
        let! result= pipe.Reader.ReadAsync()
        return result
  }
  /// Iterates over a BufferSegment list looking for a marker, data before the marker
  /// is sent to the function select
  /// Returns the number of bytes read.
  let scanMarker (marker: byte[]) (select : SelectFunction) = 
    async{
    let! result = getData
    let bufferSequence = result.Buffer
    match kmpW marker bufferSequence with
    | Some x ->
      let res = split bufferSequence (int x) select
      match res with
      | Continue n ->
        pipe.Reader.AdvanceTo(bufferSequence.GetPosition(int64(n + marker.Length)))
        return Choice1Of2(Found n)
      | FailWith s ->
        return Choice1Of2(Error s)
    | None ->
      let r = split bufferSequence (int(bufferSequence.Length - int64 marker.Length)) select
      pipe.Reader.AdvanceTo(bufferSequence.GetPosition(bufferSequence.Length - int64 marker.Length))
      return Choice1Of2(NeedMore)
  }

  /// Read the passed stream into buff until the EOL (CRLF) has been reached
  /// and returns the number of bytes read and the connection
  let readUntilPattern scanData =
    let rec loop  = socket {
      let! res = scanData
      match res with
      | Found a ->
        return a
      | NeedMore ->
        do! readMoreData
        return! loop
      | Error s ->
        return! SocketOp.abort s
    }
    loop

  /// returns the number of bytes read and the connection
  let readUntilEOL select =
    readUntilPattern (scanMarker EOL select)

  member x.skip n =
     async{
    let! result= pipe.Reader.ReadAsync()
    let bufferSequence = result.Buffer
    // we really do not calling split because we are not doing anything with it
    let res = split bufferSequence n (fun a b -> Continue 0 )
    match res with
      | Continue n ->
        pipe.Reader.AdvanceTo(bufferSequence.GetPosition(n))
        return Found n
      | FailWith s ->
        return Error s
    }
  /// Read the stream until the marker appears and return the number of bytes
  /// read.
  member x.readUntil (marker : byte []) (select : SelectFunction) =
    readUntilPattern (scanMarker marker select)

  /// Read a line from the stream, calling UTF8.toString on the bytes before the EOL marker
  member x.readLine = socket {
    let offset = ref 0
    let! count =
      readUntilEOL (fun a count -> 
        if !offset + count > lineBuffer.Length then 
          FailWith (InputDataError (Some 414, "Line Too Long"))
        else
          let source = a.Span.Slice(0,count)
          let target = new Span<byte>(lineBuffer,!offset,count)
          source.CopyTo(target)
          offset := !offset + count
          Continue !offset
      )
    let result = Encoding.UTF8.GetString(lineBuffer, 0, !offset)
    return result
  }

  member x.skipLine = socket {
    let offset = ref 0
    let! _ =
      readUntilEOL (fun a count ->
        offset := !offset + count
        Continue !offset
      )
    return offset
  }

  /// Read all headers from the stream, returning a dictionary of the headers found
  member x.readHeaders =
    let rec loop headers = socket {
      let offset = ref 0
      let buf = lineBuffer
      let! count =
        readUntilEOL (fun a count -> 
          if !offset + count > lineBuffer.Length then 
            FailWith (InputDataError (Some 431, "Request Header Fields Too Large"))
          else
            let source = a.Span.Slice(0,count)
            let target = new Span<byte>(lineBuffer, !offset, count)
            source.CopyTo(target)
            offset := !offset + count
            Continue !offset
        )
      if !offset <> 0 then
        let line = Encoding.UTF8.GetString(buf, 0, !offset)
        let indexOfColon = line.IndexOf(':')
        let header = (line.Substring(0, indexOfColon).ToLower(), line.Substring(indexOfColon+1).TrimStart())
        return! loop (header :: headers)
      else return headers
    }
    loop []

  /// Read the post data from the stream, given the number of bytes that makes up the post data.
  member x.readPostData (bytes : int) (select:ReadOnlyMemory<byte> -> int -> unit)  : SocketOp<unit> =
    let rec loop n : SocketOp<unit> =
      async {
        if n = 0 then
          return Choice1Of2()
        else
          let! result = getData
          let bufferSequence = result.Buffer
          if bufferSequence.Length > 0  then
            let segment = bufferSequence.First
            if segment.Length > n then
              select segment n
              pipe.Reader.AdvanceTo(bufferSequence.GetPosition(int64(n)))
              return Choice1Of2()
            else
              select segment segment.Length
              pipe.Reader.AdvanceTo(bufferSequence.GetPosition(int64(segment.Length)))
              return! loop (n - segment.Length)
          else
            if n = 0 then
              return Choice1Of2()
            else
              match! readMoreData with
              | Choice1Of2 _ ->
                return! loop n
              | Choice2Of2 error ->
                return Choice2Of2 error
      }
    loop bytes

type internal ConnectionFacade(connection: Connection, logger:Logger,matchedBinding: HttpBinding) =

  let reader = new Reader(connection.transport,connection.lineBuffer,connection.pipe)

  let files = List<HttpUpload>()
  let multiPartFields = List<string*string>()
  let mutable _rawForm : byte array = [||]

  let readFilePart boundary (headerParams : Dictionary<string,string>) fieldName contentType = socket {
    let tempFilePath = Path.GetTempFileName()
    use tempFile = new FileStream(tempFilePath, FileMode.Truncate)
    let! a =
      reader.readUntil (ASCII.bytes (eol + boundary)) (fun x y ->
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
    let rec loop = socket {
      let! firstLine = reader.readLine

      if firstLine.Equals("--") then
        return ()
      else
        let! partHeaders = reader.readHeaders

        let! (contentDisposition : string) =
          (partHeaders %% "content-disposition")
          @|! (None, "Missing 'content-disposition'")

        match partHeaders %% "content-type" with
        | Choice1Of2 contentType ->
          let headerParams = headerParams contentDisposition
          logger.verbose (
            eventX "Parsing {contentType}... -> readFilePart"
            >> setFieldValue "contentType" contentType)

          let! res = readFilePart boundary headerParams fieldName contentType
          logger.verbose (
            eventX "Parsed {contentType} <- readFilePart"
            >> setFieldValue "contentType" contentType)

          match res with
          | Some upload ->
            files.Add(upload)
            return! loop
          | None ->
            return! loop

        | Choice2Of2 _ ->
          use mem = new MemoryStream()
          let! a =
            reader.readUntil (ASCII.bytes(eol + boundary)) (fun x y -> 
                mem.Write(x.Span.Slice(0,y))
                Continue 0
              )
          let byts = mem.ToArray()
          multiPartFields.Add (fieldName, Encoding.UTF8.GetString(byts, 0, byts.Length))
          return! loop
      }
    loop

  /// Parses multipart data from the stream, feeding it into the HttpRequest's property Files.
  let parseMultipart (boundary:string) : SocketOp<unit> =
    let parsePart = socket {

        let! partHeaders = reader.readHeaders

        let! (contentDisposition : string) =
          (partHeaders %% "content-disposition")
          @|! (None, "Missing 'content-disposition'")

        let headerParams = headerParams contentDisposition

        let! _ =
          (headerParams.TryLookup "form-data" |> Choice.map (String.trimc '"'))
          @|! (None, "Key 'form-data' was not present in 'content-disposition'")

        let! fieldName =
          (headerParams.TryLookup "name" |> Choice.map (String.trimc '"'))
          @|! (None, "Key 'name' was not present in 'content-disposition'")

        match partHeaders %% "content-type" with
        | Choice1Of2 x when String.startsWith "multipart/mixed" x ->
          let subboundary = "--" + parseBoundary x
          do! parseMultipartMixed fieldName subboundary
          let a = reader.skip (boundary.Length)
          return ()

        | Choice1Of2 contentType when headerParams.ContainsKey "filename" ->
          logger.verbose (
            eventX "Parsing {contentType}... -> readFilePart"
            >> setFieldValue "contentType" contentType
            >> setSingleName "Suave.Web.parseMultipart")
          let! res = readFilePart boundary headerParams fieldName contentType
          logger.verbose (
            eventX "Parsed {contentType} <- readFilePart"
            >> setFieldValue "contentType" contentType
            >> setSingleName "Suave.Web.parseMultipart")

          res |> Option.iter files.Add

        | Choice1Of2 _ | Choice2Of2 _ ->
          use mem = new MemoryStream()
          let! a =
            reader.readUntil (ASCII.bytes (eol + boundary)) (fun x y ->
              mem.Write(x.Span.Slice(0,y))
              Continue 0
            )
          let byts = mem.ToArray()
          let str =  Encoding.UTF8.GetString(byts, 0, byts.Length)
          multiPartFields.Add(fieldName,str)
          return ()
      }

    socket {
      let mutable parsing = true
      let! firstLine = reader.readLine
      if firstLine<>boundary then
        failwithf "Invalid multipart format: expected boundary '%s' got '%s'" boundary firstLine

      while parsing do
        do! parsePart
        let! line = reader.readLine
        if line.StartsWith("--") then
          parsing <- false
        else if line <> String.Empty then
          failwith "Invalid multipart format"

      return ()
    }

  /// Reads raw POST data
  let getRawPostData contentLength =
    socket {
      let offset = ref 0
      let rawForm = Array.zeroCreate contentLength
      do! reader.readPostData contentLength (fun a count ->
          let source = a.Span.Slice(0,count)
          let target = new Span<byte>(rawForm,!offset,count)
          source.CopyTo(target)
          offset := !offset + count)
      return rawForm
    }

  let parsePostData maxContentLength (contentLengthHeader : Choice<string,_>) contentTypeHeader = socket {
    match contentLengthHeader with
    | Choice1Of2 contentLengthString ->
      let contentLength = Convert.ToInt32 contentLengthString

      if contentLength > maxContentLength then
        return! async.Return (Choice2Of2 (InputDataError (Some 413, "Payload too large")))
      else
        logger.verbose (eventX "Expecting {contentLength} bytes" >> setFieldValue "contentLength" contentLength)

        match contentTypeHeader with
        | Choice1Of2 ce when String.startsWith "application/x-www-form-urlencoded" ce ->
          let! rawForm = getRawPostData contentLength
          _rawForm <- rawForm
          return ()

        | Choice1Of2 ce when String.startsWith "multipart/form-data" ce ->
          let boundary = "--" + parseBoundary ce

          logger.verbose (eventX "Parsing multipart")
          do! parseMultipart boundary
          return ()

        | Choice1Of2 _ | Choice2Of2 _ ->
          let! rawForm = getRawPostData contentLength
          _rawForm <- rawForm
          return ()
      | Choice2Of2 _ -> return ()
    }

  /// Process the request, reading as it goes from the incoming 'stream', yielding a HttpRequest
  /// when done
  member this.processRequest (ctx:HttpContext) = socket {
    let verbose message = logger.verbose (eventX message)

    verbose "reading first line of request"
    let! firstLine = reader.readLine

    let! rawMethod, path, rawQuery, httpVersion = 
      parseUrl firstLine
      @|! (None, "Invalid ")

    verbose "reading headers"
    let! headers = reader.readHeaders

    // Respond with 400 Bad Request as
    // per http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
    let! rawHost = headers %% "host" @|! (None, "Missing 'Host' header")

    if headers %% "expect" = Choice1Of2 "100-continue" then
      let! _ = SocketOp.ofAsync(HttpOutput.run Intermediate.CONTINUE ctx)
      verbose "sent 100-continue response"

    verbose "parsing post data"
    do! parsePostData ctx.runtime.maxContentLength (headers %% "content-length") (headers %% "content-type")

    let request =
      { httpVersion      = httpVersion
        binding          = matchedBinding
        rawPath          = path
        rawHost          = rawHost
        rawMethod        = rawMethod
        headers          = headers
        rawForm          = _rawForm
        rawQuery         = rawQuery
        files            = Seq.toList files
        multiPartFields  = Seq.toList multiPartFields
        trace            = TraceHeader.parseTraceHeaders headers }
    
    // clear form data before exit
    files.Clear()
    multiPartFields.Clear()
    _rawForm <- [||]

    return Some { ctx with request = request; }
  }
