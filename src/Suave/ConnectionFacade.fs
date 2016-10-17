
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
open Suave.Sockets
open Suave.Sockets.Connection
open Suave.Sockets.Control
open Suave.Sockets.SocketOp.Operators
open Suave.Utils.Bytes

type ScanResult = NeedMore | Found of int

module internal Aux =

  let inline skipBuffers (pairs : LinkedList<BufferSegment>) (number : int)  =
    let rec loop acc = 
      if pairs.Count > 0 then
        let x = pairs.First.Value
        pairs.RemoveFirst()
      
        if x.length + acc >= number then 
          let segment = BufferSegment.mk x.buffer (x.offset  + (number - acc)) (x.length - number + acc)
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

type ConnectionFacade(ctx) =

  let BadRequestPrefix = "__suave_BAD_REQUEST"

  let transport = ctx.connection.transport
  let bufferManager = ctx.connection.bufferManager
  let lineBuffer = ctx.connection.lineBuffer
  let logger = ctx.runtime.logger
  let trace = ctx.request.trace
  let matchedBinding = ctx.runtime.matchedBinding

  let segments = new LinkedList<BufferSegment>()
  let files = List<HttpUpload>()
  let multiPartFields = List<string*string>()
  let mutable _rawForm : byte array = [||]

  /// Splits the segment list in two lits of ArraySegment; the first one containing a total of index bytes 
  let split index select markerLength : Async<int> =
    let rec loop acc count =  async {
      if segments.Count = 0 then 
       return count
      else
        let pair = segments.First.Value
        segments.RemoveFirst()
        if acc + pair.length < index then
          do! select (ArraySegment(pair.buffer.Array, pair.offset, pair.length)) pair.length
          bufferManager.FreeBuffer( pair.buffer, "Suave.Web.split")
          return! loop (acc + pair.length) (count + acc + pair.length)
        elif acc + pair.length >= index then
          let bytesRead = index - acc
          do! select (ArraySegment(pair.buffer.Array, pair.offset, bytesRead)) bytesRead
          let remaining = pair.length - bytesRead
          if remaining = markerLength then
            bufferManager.FreeBuffer( pair.buffer, "Suave.Web.split")
            return count + bytesRead
          else
            if remaining - markerLength >= 0 then
              let segment = BufferSegment.mk pair.buffer
                                             (pair.offset  + bytesRead  + markerLength)
                                             (remaining - markerLength)
              segments.AddFirst( segment) |> ignore
              return count + bytesRead
            else
              Aux.skipBuffers segments (markerLength - remaining)
              return count + bytesRead
        else return failwith "Suave.Web.split: invalid case"
      }
    loop 0 0

  let freeArraySegments index select = socket {
    let mutable n = 0
    let mutable currentnode = segments.Last
    while currentnode<>null do
        let b = currentnode.Value
        if n > 0 && n + b.length >= index then
          //procees, free and remove
          do! SocketOp.ofAsync <| select (ArraySegment(b.buffer.Array, b.offset, b.length)) b.length
          do bufferManager.FreeBuffer( b.buffer,"Suave.Web.scanMarker" )
          segments.Remove currentnode
        n <- n + b.length
        currentnode <- currentnode.Previous
    }


  /// Iterates over a BufferSegment list looking for a marker, data before the marker 
  /// is sent to the function select and the corresponding buffers are released
  /// Returns the number of bytes read.
  let scanMarker (marker: byte[]) select = socket {

    match kmpW marker segments with
    | Some x -> 
      let! res = SocketOp.ofAsync <| split x select marker.Length
      return Found res
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

  let skip n = socket {
    return! SocketOp.ofAsync <| split n (fun a b -> async { return () }  ) 0
    }

  /// returns the number of bytes read and the connection
  let readUntilEOL select =
    readUntilPattern (scanMarker EOL select)

  /// Read the stream until the marker appears and return the number of bytes
  /// read.
  let readUntil (marker : byte []) (select : ArraySegment<_> -> int -> Async<unit>) =
    readUntilPattern (scanMarker marker select)

  /// Read a line from the stream, calling ASCII.toString on the bytes before the EOL marker
  member this.readLine = socket {
    let offset = ref 0
    let! count =
      readUntilEOL (fun a count -> async {
        Array.blit a.Array a.Offset lineBuffer.Array (lineBuffer.Offset + !offset) count
        offset := !offset + count
      })
    let result = ASCII.toStringAtOffset lineBuffer.Array lineBuffer.Offset count
    return result
  }

  member this.skipLine = socket {
    let offset = ref 0
    let! _ =
      readUntilEOL (fun a count -> async {
        offset := !offset + count
      })
    return offset
  }

  /// Read all headers from the stream, returning a dictionary of the headers found
  member this.readHeaders =
    let rec loop headers = socket {
      let offset = ref 0
      let buf = lineBuffer
      let! count =
        readUntilEOL (fun a count -> async {
          Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count
          offset := !offset + count
        })
      if count <> 0 then
        let line = ASCII.toStringAtOffset buf.Array buf.Offset count
        let indexOfColon = line.IndexOf(':')
        let header = (line.Substring(0, indexOfColon).ToLower(), line.Substring(indexOfColon+1).TrimStart())
        return! loop (header :: headers)
      else return headers
    }
    loop []

  /// Read the post data from the stream, given the number of bytes that makes up the post data.
  member this.readPostData  (bytes : int) select  : SocketOp<unit> =

    let rec loop n : SocketOp<unit> =
      socket {
        if segments.Count > 0  then
          let segment = segments.First.Value
          segments.RemoveFirst() |> ignore
          if segment.length > n then
            do! SocketOp.ofAsync <| select (BufferSegment.toArraySegment(BufferSegment(segment.buffer,n,segment.length))) n
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

  member this.readFilePart boundary (headerParams : Dictionary<string,string>) fieldName contentType = socket {
    let tempFilePath = Path.GetTempFileName()
    use tempFile = new FileStream(tempFilePath, FileMode.Truncate)
    let! a =
      readUntil (ASCII.bytes (eol + boundary)) (fun x y -> async {
          do! tempFile.AsyncWrite(x.Array, x.Offset, y)
          })
    let fileLength = tempFile.Length
    tempFile.Close()

    if fileLength > 0L then
      let! filename =
        (headerParams.TryLookup "filename" |> Choice.map (String.trimc '"'))
        @|! "Key 'filename' was not present in 'content-disposition'"

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

  member this.parseMultipartMixed fieldName boundary : SocketOp<unit> =
    let verbose = Log.verbose logger "Suave.Web.parseMultipartMixed" trace
    let verbosef = Log.verbosef logger "Suave.Web.parseMultipartMixed" trace

    let rec loop = socket {

      let! firstLine = this.readLine

      if firstLine.Equals("--") then
        return ()
      else

        let! partHeaders = this.readHeaders

        let! (contentDisposition : string) =
          (partHeaders %% "content-disposition")
          @|! "Missing 'content-disposition'"

        match partHeaders %% "content-type" with
        | Choice1Of2 contentType ->
          let headerParams = headerParams contentDisposition
          verbosef (fun f -> f "parsing content type %s -> readFilePart" contentType)
          let! res = this.readFilePart boundary headerParams fieldName contentType
          verbosef (fun f -> f "parsing content type %s -> readFilePart" contentType)

          match res with
          | Some upload ->
            files.Add(upload)
            return! loop
          | None ->
            return! loop

        | Choice2Of2 _ ->
          use mem = new MemoryStream()
          let! a =
            readUntil (ASCII.bytes(eol + boundary)) (fun x y -> async {
                do! mem.AsyncWrite(x.Array, x.Offset, y)
              })
          let byts = mem.ToArray()
          multiPartFields.Add (fieldName, ASCII.toStringAtOffset byts 0 byts.Length)
          return! loop 
      }
    loop

  /// Parses multipart data from the stream, feeding it into the HttpRequest's property Files.
  member this.parseMultipart (boundary:string) : SocketOp<unit> =
    let verbosef = Log.verbosef logger "Suave.Web.parseMultipart" trace

    let  parsePart  = socket {

        let! partHeaders = this.readHeaders

        let! (contentDisposition : string) =
          (partHeaders %% "content-disposition")
          @|! "Missing 'content-disposition'"

        let headerParams = headerParams contentDisposition

        let! _ =
          (headerParams.TryLookup "form-data" |> Choice.map (String.trimc '"'))
          @|! "Key 'form-data' was not present in 'content-disposition'"
        
        let! fieldName =
          (headerParams.TryLookup "name" |> Choice.map (String.trimc '"'))
          @|! "Key 'name' was not present in 'content-disposition'"

        match partHeaders %% "content-type" with
        | Choice1Of2 x when String.startsWith "multipart/mixed" x ->
          let subboundary = "--" + (x.Substring(x.IndexOf('=') + 1) |> String.trimStart |> String.trimc '"')
          do! this.parseMultipartMixed fieldName subboundary
          let! a = skip (boundary.Length)
          return () 

        | Choice1Of2 contentType ->
          verbosef (fun f -> f "parsing content type %s -> readFilePart" contentType)
          let! res = this.readFilePart boundary  headerParams fieldName contentType
          verbosef (fun f -> f "parsing content type %s <- readFilePart" contentType)

          match res with
          | Some upload ->
            files.Add(upload)
            return () 
          | None ->
            return () 

        | Choice2Of2 _ ->
          use mem = new MemoryStream()
          let! a =
            readUntil (ASCII.bytes(eol + boundary)) (fun x y -> async {
                do! mem.AsyncWrite(x.Array, x.Offset, y)
              })
          let byts = mem.ToArray()
          multiPartFields.Add(fieldName, ASCII.toStringAtOffset byts 0 byts.Length)
          return () 
      }

    socket {
      let mutable parsing = true
      let! firstLine = this.readLine

      assert(firstLine=boundary)
      while parsing do
        do! parsePart
        //pick at the next two bytes and decide where to exit the loop
        let! b = pick
        if b.buffer.Array.[b.offset] = 45uy && b.buffer.Array.[b.offset+1] = 45uy then
          let! a = skip 2
          parsing <- false
          return ()
        elif b.buffer.Array.[b.offset] = EOL.[0] && b.buffer.Array.[b.offset+1] = EOL.[1] then
          let! a = skip 2
          return ()
        else
          failwith "invalid multipart format"
    } 

  /// Reads raw POST data
  member this.getRawPostData contentLength =
    socket {
      let offset = ref 0
      let rawForm = Array.zeroCreate contentLength
      do! this.readPostData contentLength (fun a count -> async {
          Array.blit a.Array a.Offset rawForm !offset count;
          offset := !offset + count
        })
      return rawForm
    }

  member this.parsePostData (contentLengthHeader:Choice<string,_>) contentTypeHeader = socket {

    let verbosef = Log.verbosef logger "Suave.Web.parsePostData" trace
    let verbose = Log.verbose logger "Suave.Web.parsePostData" trace

    match contentLengthHeader with 
    | Choice1Of2 contentLengthString ->
      let contentLength = Convert.ToInt32 contentLengthString
      verbosef (fun f -> f "expecting content length %d" contentLength)

      match contentTypeHeader with
      | Choice1Of2 ce when String.startsWith "application/x-www-form-urlencoded" ce ->
        let! rawForm = this.getRawPostData contentLength
        _rawForm <- rawForm
        return Some ()

      | Choice1Of2 ce when String.startsWith "multipart/form-data" ce ->
        let boundary = "--" + (ce |> String.substring (ce.IndexOf('=') + 1) |> String.trimStart |> String.trimc '"')

        verbose "parsing multipart"
        do! this.parseMultipart boundary
        return Some ()

      | Choice1Of2 _ | Choice2Of2 _ ->
        let! rawForm = this.getRawPostData contentLength
        _rawForm <- rawForm
        return Some ()
    | Choice2Of2 _ -> return Some ()
    }

  /// Process the request, reading as it goes from the incoming 'stream', yielding a HttpRequest
  /// when done
  member this.processRequest firstLine = socket {
    let verbose = Log.verbose logger "Suave.Web.processRequest" trace

    let rawMethod, path, rawQuery, httpVersion = parseUrl firstLine
    let meth = HttpMethod.parse rawMethod

    verbose "reading headers"
    let! headers = this.readHeaders

    // Respond with 400 Bad Request as
    // per http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
    let! host =
      (headers %% "host" |> Choice.map (function
        | s when System.Text.RegularExpressions.Regex.IsMatch(s, ":\d+$") ->
          s.Substring(0, s.LastIndexOf(':'))
        | s -> s))
      @|! "Missing 'Host' header"

    if headers %% "expect" = Choice1Of2 "100-continue" then
      let! _ = HttpOutput.run Intermediate.CONTINUE ctx
      verbose "sent 100-continue response"

    if ctx.runtime.parsePostData then
      verbose "parsing post data"
      let! _ = this.parsePostData (headers %% "content-length") (headers %% "content-type")
      ()

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

    return Some { ctx with request = request; connection = { ctx.connection with segments = Seq.toList segments } }
  }