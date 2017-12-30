
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
open Suave.Sockets.Connection
open Suave.Sockets.Control
open Suave.Sockets.SocketOp.Operators
open Suave.Utils.Bytes

type ScanResult = NeedMore | Found of int
type SelectResult = FailWith of Error | Continue of int
type SelectFunction = ArraySegment<byte> -> int -> Async<SelectResult>

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

type Reader(segments:LinkedList<BufferSegment>, bufferManager : BufferManager, transport, lineBuffer : ArraySegment<byte>) =
  /// Splits the segment list in two lits of ArraySegment; the first one containing a total of index bytes
  let split index (select:SelectFunction) markerLength : Async<SelectResult> =
    let rec loop acc selectResult : Async<SelectResult>  =  async {
      match selectResult with
      | Continue count ->
        if segments.Count = 0 then
         return Continue count
        else
          let pair = segments.First.Value
          segments.RemoveFirst()
          if acc + pair.length < index then
            let! selectResult = select (ArraySegment(pair.buffer.Array, pair.offset, pair.length)) pair.length
            match selectResult with
            | Continue _ ->
              bufferManager.FreeBuffer(pair.buffer, "Suave.Web.split")
              return! loop (acc + pair.length) (Continue(count + acc + pair.length))
            | FailWith s ->
              return FailWith s
          elif acc + pair.length >= index then
            let bytesRead = index - acc
            let! selectResult = select (ArraySegment(pair.buffer.Array, pair.offset, bytesRead)) bytesRead
            match selectResult with
            | Continue _ ->
              let remaining = pair.length - bytesRead
              if remaining = markerLength then
                bufferManager.FreeBuffer(pair.buffer, "Suave.Web.split")
                return Continue(count + bytesRead)
              else
                if remaining - markerLength >= 0 then
                  let segment =
                    BufferSegment.create pair.buffer
                                         (pair.offset  + bytesRead  + markerLength)
                                         (remaining - markerLength)
                  segments.AddFirst(segment) |> ignore
                  return Continue (count + bytesRead)
                else
                  Aux.skipBuffers segments (markerLength - remaining)
                  return Continue(count + bytesRead)
             | FailWith s ->
               return FailWith s
            else return failwith "Suave.Web.split: invalid case"
      | FailWith s ->
        return FailWith s
      }
    loop 0 (Continue 0)

  let freeArraySegments index (select: SelectFunction) = socket {
    let mutable n = 0
    let mutable currentnode = segments.Last
    while currentnode<>null do
        let b = currentnode.Value
        if n > 0 && n + b.length >= index then
          //procees, free and remove
          let! res = SocketOp.ofAsync <| select (ArraySegment(b.buffer.Array, b.offset, b.length)) b.length
          match res with
          | Continue _ ->
            do bufferManager.FreeBuffer(b.buffer, "Suave.Web.scanMarker")
            segments.Remove currentnode
          | FailWith err ->
            return! SocketOp.abort err
        n <- n + b.length
        currentnode <- currentnode.Previous
    }

  /// Iterates over a BufferSegment list looking for a marker, data before the marker
  /// is sent to the function select and the corresponding buffers are released
  /// Returns the number of bytes read.
  let scanMarker (marker: byte[]) (select : SelectFunction) = socket {

    match kmpW marker segments with
    | Some x ->
      let! res = SocketOp.ofAsync <| split x select marker.Length
      match res with
      | Continue n ->
        return Found n
      | FailWith s ->
        return! SocketOp.abort s
    | None   ->
      do! freeArraySegments marker.Length select
      return NeedMore
    }

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
      let! res = scanData
      match res with
      | Found a ->
        return a
      | NeedMore ->
        do! readMoreData
        return! loop
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

  member x.skip n = socket {
    return! SocketOp.ofAsync <| split n (fun a b -> async { return Continue 0}  ) 0
    }

  /// Read the stream until the marker appears and return the number of bytes
  /// read.
  member x.readUntil (marker : byte []) (select : ArraySegment<_> -> int -> Async<SelectResult>) =
    readUntilPattern (scanMarker marker select)

  /// Read a line from the stream, calling UTF8.toString on the bytes before the EOL marker
  member x.readLine = socket {
    let offset = ref 0
    let! count =
      readUntilEOL (fun a count -> async {
        if !offset + count > lineBuffer.Count then 
          return FailWith (InputDataError (Some 414, "Line Too Long"))
        else
          Array.blit a.Array a.Offset lineBuffer.Array (lineBuffer.Offset + !offset) count
          offset := !offset + count
          return Continue !offset
      })
    let result = UTF8.toStringAtOffset lineBuffer.Array lineBuffer.Offset count
    return result
  }

  member x.skipLine = socket {
    let offset = ref 0
    let! _ =
      readUntilEOL (fun a count -> async {
        offset := !offset + count
        return Continue !offset
      })
    return offset
  }

  /// Read all headers from the stream, returning a dictionary of the headers found
  member x.readHeaders =
    let rec loop headers = socket {
      let offset = ref 0
      let buf = lineBuffer
      let! count =
        readUntilEOL (fun a count -> async {
          if !offset + count > lineBuffer.Count then 
            return FailWith (InputDataError (Some 431, "Request Header Fields Too Large"))
          else
            Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count
            offset := !offset + count
            return Continue !offset
        })
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

type ConnectionFacade(logger:Logger,matchedBinding: HttpBinding) =

  let files = List<HttpUpload>()
  let multiPartFields = List<string*string>()
  let mutable _rawForm : byte array = [||]

  let readFilePart (reader:Reader) boundary (headerParams : Dictionary<string,string>) fieldName contentType = socket {
    let tempFilePath = Path.GetTempFileName()
    use tempFile = new FileStream(tempFilePath, FileMode.Truncate)
    let! a =
      reader.readUntil (ASCII.bytes (eol + boundary)) (fun x y -> async {
          do! tempFile.AsyncWrite(x.Array, x.Offset, y)
          return Continue 0
          })
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

  let parseMultipartMixed (reader:Reader) fieldName boundary : SocketOp<unit> =
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

          let! res = readFilePart reader boundary headerParams fieldName contentType

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
            reader.readUntil (ASCII.bytes(eol + boundary)) (fun x y -> async {
                do! mem.AsyncWrite(x.Array, x.Offset, y)
                return Continue 0
              })
          let byts = mem.ToArray()
          multiPartFields.Add (fieldName, UTF8.toStringAtOffset byts 0 byts.Length)
          return! loop
      }
    loop

  /// Parses multipart data from the stream, feeding it into the HttpRequest's property Files.
  let parseMultipart (reader:Reader) (boundary:string) : SocketOp<unit> =
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
          do! parseMultipartMixed reader fieldName subboundary
          let! a = reader.skip (boundary.Length)
          return ()

        | Choice1Of2 contentType when headerParams.ContainsKey "filename" ->
          logger.verbose (
            eventX "Parsing {contentType}... -> readFilePart"
            >> setFieldValue "contentType" contentType
            >> setSingleName "Suave.Web.parseMultipart")

          let! res = readFilePart reader boundary headerParams fieldName contentType

          logger.verbose (
            eventX "Parsed {contentType} <- readFilePart"
            >> setFieldValue "contentType" contentType
            >> setSingleName "Suave.Web.parseMultipart")

          res |> Option.iter files.Add

        | Choice1Of2 _ | Choice2Of2 _ ->
          use mem = new MemoryStream()
          let! a =
            reader.readUntil (ASCII.bytes (eol + boundary)) (fun x y -> async {
              do! mem.AsyncWrite(x.Array, x.Offset, y)
              return Continue 0
            })
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
  let getRawPostData (reader:Reader) contentLength =
    socket {
      let offset = ref 0
      let rawForm = Array.zeroCreate contentLength
      do! reader.readPostData contentLength (fun a count -> async {
          Array.blit a.Array a.Offset rawForm !offset count;
          offset := !offset + count
        })
      return rawForm
    }

  let parsePostData (reader:Reader)  maxContentLength (contentLengthHeader : Choice<string,_>) contentTypeHeader = socket {
    match contentLengthHeader with
    | Choice1Of2 contentLengthString ->
      let contentLength = Convert.ToInt32 contentLengthString

      if contentLength > maxContentLength then
        return! async.Return (Choice2Of2 (InputDataError (Some 413, "Payload too large")))
      else
        logger.verbose (eventX "Expecting {contentLength}" >> setFieldValue "contentLength" contentLength)

        match contentTypeHeader with
        | Choice1Of2 ce when String.startsWith "application/x-www-form-urlencoded" ce ->
          let! rawForm = getRawPostData reader contentLength
          _rawForm <- rawForm
          return ()

        | Choice1Of2 ce when String.startsWith "multipart/form-data" ce ->
          let boundary = "--" + (ce |> String.substring (ce.IndexOf('=') + 1) |> String.trimStart |> String.trimc '"')

          logger.verbose (eventX "Parsing multipart")
          do! parseMultipart reader boundary
          return ()

        | Choice1Of2 _ | Choice2Of2 _ ->
          let! rawForm = getRawPostData reader contentLength
          _rawForm <- rawForm
          return ()
      | Choice2Of2 _ -> return ()
    }

  /// Process the request, reading as it goes from the incoming 'stream', yielding a HttpRequest
  /// when done
  member this.processRequest (ctx:HttpContext) = socket {
    let verbose message = logger.verbose (eventX message)

    let segments = new LinkedList<_>(ctx.connection.segments)
    let reader = new Reader(segments,ctx.connection.bufferManager,ctx.connection.transport,ctx.connection.lineBuffer)

    verbose "reading first line of request"
    let! firstLine = reader.readLine

    let! rawMethod, path, rawQuery, httpVersion = 
      parseUrl firstLine
      @|! (None, "Invalid ")

    let meth = HttpMethod.parse rawMethod

    verbose "reading headers"
    let! headers = reader.readHeaders

    // Respond with 400 Bad Request as
    // per http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
    let! host =
      (headers %% "host" |> Choice.map (function
        | s when System.Text.RegularExpressions.Regex.IsMatch(s, ":\d+$") ->
          s.Substring(0, s.LastIndexOf(':'))
        | s -> s))
      @|! (None, "Missing 'Host' header")

    if headers %% "expect" = Choice1Of2 "100-continue" then
      let! _ = HttpOutput.run Intermediate.CONTINUE ctx
      verbose "sent 100-continue response"

    verbose "parsing post data"
    do! parsePostData reader ctx.runtime.maxContentLength (headers %% "content-length") (headers %% "content-type")

    let request =
      { httpVersion      = httpVersion
        url              = matchedBinding.uri path rawQuery
        host             = host
        ``method``       = meth
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

    return Some { ctx with request = request; connection = { ctx.connection with segments = Seq.toList segments } }
  }
