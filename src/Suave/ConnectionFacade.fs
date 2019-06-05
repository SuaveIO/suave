
#nowarn "40"
namespace Suave

open System
open System.IO
open System.Collections.Generic
open System.Net.Sockets

open Suave
open Suave.Utils
open Suave.Utils.Parsing
open Suave.Logging
open Suave.Logging.Message
open Suave.Sockets
open Suave.Sockets.Control
open Suave.Sockets.SocketOp.Operators
open Suave.Utils.Bytes

type internal ScanResult = NeedMore | Found of int | Error of Error
type internal SelectResult = FailWith of Error | Continue of int
type internal SelectFunction = ArraySegment<byte> -> int -> SelectResult

module internal Aux =

  let inline skipBuffers (pairs : LinkedList<BufferSegment>) (number : int)  =
    let rec loop acc =
      if pairs.Count > 0 then
        let x = pairs.First.Value
        pairs.RemoveFirst()

        if x.length + acc >= number then
          let segment = BufferSegment.create x.buffer (x.offset  + (number - acc)) (x.length - number + acc)
          pairs.AddFirst segment |> ignore
        else loop (acc + x.length)
    loop 0

  let inline readData (transport:ITransport) buff = socket {
    let! b = transport.read buff
    if b > 0 then
      return BufferSegment(buff, buff.Offset, b)
    else
      return! SocketOp.abort (Error.SocketError SocketError.Shutdown)
    }

type internal Reader(segments:LinkedList<BufferSegment>, bufferManager : BufferManager, transport, lineBuffer : ArraySegment<byte>) =
  /// Splits the segment list in two lits of ArraySegment; the first one containing a total of index bytes
  // this function deals with nothing async we soul remove the async
  let split index (select:SelectFunction) markerLength : SelectResult =
    let rec loop acc selectResult : SelectResult  = 
      match selectResult with
      | Continue count ->
        if segments.Count = 0 then
         Continue count
        else
          let pair = segments.First.Value
          segments.RemoveFirst()
          if acc + pair.length < index then
            let selectResult = select (ArraySegment(pair.buffer.Array, pair.offset, pair.length)) pair.length
            match selectResult with
            | Continue _ ->
              bufferManager.FreeBuffer(pair.buffer, "Suave.Web.split")
              loop (acc + pair.length) (Continue(count + acc + pair.length))
            | FailWith s ->
              FailWith s
          elif acc + pair.length >= index then
            let bytesRead = index - acc
            let selectResult = select (ArraySegment(pair.buffer.Array, pair.offset, bytesRead)) bytesRead
            match selectResult with
            | Continue _ ->
              let remaining = pair.length - bytesRead
              if remaining = markerLength then
                bufferManager.FreeBuffer(pair.buffer, "Suave.Web.split")
                Continue(count + bytesRead)
              else
                if remaining - markerLength >= 0 then
                  let segment =
                    BufferSegment.create pair.buffer
                                         (pair.offset  + bytesRead  + markerLength)
                                         (remaining - markerLength)
                  segments.AddFirst(segment) |> ignore
                  Continue (count + bytesRead)
                else
                  Aux.skipBuffers segments (markerLength - remaining)
                  Continue(count + bytesRead)
             | FailWith s ->
               FailWith s
            else failwith "Suave.Web.split: invalid case"
      | FailWith s ->
        FailWith s
    loop 0 (Continue 0)

  let freeArraySegments index (select: SelectFunction) =
    let mutable n = 0
    let mutable currentnode = segments.Last
    let mutable error = false
    let mutable result = Choice1Of2 ()
    while currentnode<>null && not error do
        let b = currentnode.Value
        if n > 0 && n + b.length >= index then
          //procees, free and remove
          let res = select (ArraySegment(b.buffer.Array, b.offset, b.length)) b.length
          match res with
          | Continue _ ->
            do bufferManager.FreeBuffer(b.buffer, "Suave.Web.freeArraySegments")
            segments.Remove currentnode
          | FailWith err ->
            result <- Choice2Of2 err
            error <- true
        n <- n + b.length
        currentnode <- currentnode.Previous
    result

  /// Iterates over a BufferSegment list looking for a marker, data before the marker
  /// is sent to the function select and the corresponding buffers are released
  /// Returns the number of bytes read.
  let scanMarker (marker: byte[]) (select : SelectFunction) = 
    fun _ ->

    match kmpW marker segments with
    | Some x ->
      let res = split x select marker.Length
      match res with
      | Continue n ->
        Found n
      | FailWith s ->
        Error s
    | None   ->
      match freeArraySegments marker.Length select with
      | Choice1Of2 () ->
        NeedMore
      | Choice2Of2 s ->
        Error s

  let readMoreData = async {
    let buff = bufferManager.PopBuffer("Suave.Web.readMoreData")
    let! result = Aux.readData transport buff
    match result with
    | Choice1Of2 data ->
      segments.AddLast data |> ignore
      return Choice1Of2()
    | Choice2Of2 error ->
      for b in segments do
        do bufferManager.FreeBuffer(b.buffer, "Suave.Web.readMoreData")
      do bufferManager.FreeBuffer(buff, "Suave.Web.readMoreData")
      return Choice2Of2 error
    }

  /// Read the passed stream into buff until the EOL (CRLF) has been reached
  /// and returns the number of bytes read and the connection
  let readUntilPattern scanData =
    let rec loop  = socket {
      let res = scanData ()
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

  /// takes a pick at the next buffer segment in the parser queue
  let pick =
    let rec loop = socket {
      if segments.Count > 0 then
        return segments.First.Value
      else
        do! readMoreData
        return! loop
    }
    loop

  /// returns the number of bytes read and the connection
  let readUntilEOL select =
    readUntilPattern (scanMarker EOL select)

  member x.skip n = 
    split n (fun a b -> Continue 0 ) 0

  /// Read the stream until the marker appears and return the number of bytes
  /// read.
  member x.readUntil (marker : byte []) (select : ArraySegment<_> -> int -> SelectResult) =
    readUntilPattern (scanMarker marker select)

  /// Read a line from the stream, calling UTF8.toString on the bytes before the EOL marker
  member x.readLine = socket {
    let offset = ref 0
    let! count =
      readUntilEOL (fun a count -> 
        if !offset + count > lineBuffer.Count then 
          FailWith (InputDataError (Some 414, "Line Too Long"))
        else
          Array.blit a.Array a.Offset lineBuffer.Array (lineBuffer.Offset + !offset) count
          offset := !offset + count
          Continue !offset
      )
    let result = UTF8.toStringAtOffset lineBuffer.Array lineBuffer.Offset count
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
          if !offset + count > lineBuffer.Count then 
            FailWith (InputDataError (Some 431, "Request Header Fields Too Large"))
          else
            Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count
            offset := !offset + count
            Continue !offset
        )
      if count <> 0 then
        let line = UTF8.toStringAtOffset buf.Array buf.Offset count
        let indexOfColon = line.IndexOf(':')
        let header = (line.Substring(0, indexOfColon).ToLower(), line.Substring(indexOfColon+1).TrimStart())
        return! loop (header :: headers)
      else return headers
    }
    loop []

  /// Read the post data from the stream, given the number of bytes that makes up the post data.
  member x.readPostData  (bytes : int) select  : SocketOp<unit> =
    let rec loop n : SocketOp<unit> =
      socket {
        if segments.Count > 0  then
          let segment = segments.First.Value
          segments.RemoveFirst() |> ignore
          if segment.length > n then
            do! SocketOp.ofAsync <| select (BufferSegment.toArraySegment(BufferSegment(segment.buffer,segment.offset, n))) n
            segments.AddFirst (BufferSegment(segment.buffer, segment.offset + n, segment.length - n)) |> ignore
            return ()
          else
            do! SocketOp.ofAsync <| select (BufferSegment.toArraySegment segment) segment.length
            do bufferManager.FreeBuffer(segment.buffer, "Suave.Web.readPostData.loop")
            return! loop (n - segment.length)
        else
          if n = 0 then
            return ()
          else
            do! readMoreData
            return! loop n
      }
    loop bytes

type internal ConnectionFacade(connection: Connection, logger:Logger,matchedBinding: HttpBinding) =

  let reader = new Reader(connection.segments,connection.bufferManager,connection.transport,connection.lineBuffer)

  let files = List<HttpUpload>()
  let multiPartFields = List<string*string>()
  let mutable _rawForm : byte array = [||]

  let readFilePart boundary (headerParams : Dictionary<string,string>) fieldName contentType = socket {
    let tempFilePath = Path.GetTempFileName()
    use tempFile = new FileStream(tempFilePath, FileMode.Truncate)
    let! a =
      reader.readUntil (ASCII.bytes (eol + boundary)) (fun x y -> 
          do tempFile.Write(x.Array, x.Offset, y)
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
                mem.Write(x.Array, x.Offset, y)
                Continue 0
              )
          let byts = mem.ToArray()
          multiPartFields.Add (fieldName, UTF8.toStringAtOffset byts 0 byts.Length)
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
          let subboundary = "--" + (x.Substring(x.IndexOf('=') + 1) |> String.trimStart |> String.trimc '"')
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
              mem.Write(x.Array, x.Offset, y)
              Continue 0
            )
          let byts = mem.ToArray()
          multiPartFields.Add(fieldName, UTF8.toStringAtOffset byts 0 byts.Length)
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
      do! reader.readPostData contentLength (fun a count -> async {
          Array.blit a.Array a.Offset rawForm !offset count;
          offset := !offset + count
        })
      return rawForm
    }

  let parsePostData maxContentLength (contentLengthHeader : Choice<string,_>) contentTypeHeader = socket {
    match contentLengthHeader with
    | Choice1Of2 contentLengthString ->
      let contentLength = Convert.ToInt32 contentLengthString

      if contentLength > maxContentLength then
        return! async.Return (Choice2Of2 (InputDataError (Some 413, "Payload too large")))
      else
        logger.verbose (eventX "Expecting {contentLength}" >> setFieldValue "contentLength" contentLength)

        match contentTypeHeader with
        | Choice1Of2 ce when String.startsWith "application/x-www-form-urlencoded" ce ->
          let! rawForm = getRawPostData contentLength
          _rawForm <- rawForm
          return ()

        | Choice1Of2 ce when String.startsWith "multipart/form-data" ce ->
          let boundary = "--" + (ce |> String.substring (ce.IndexOf('=') + 1) |> String.trimStart |> String.trimc '"')

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
