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

  let BadRequestPrefix = "__suave_BAD_REQUEST"

  /// Free up a list of buffers
  let inline free context connection =
    List.iter (fun (x: BufferSegment) -> connection.bufferManager.FreeBuffer (x.buffer, context)) connection.segments

  let inline internal skipBuffers (pairs : BufferSegment list) (number : int) :  BufferSegment list =
    let rec loop (xxs: BufferSegment list) acc = 
      match xxs with
      | [] -> []
      | x :: tail ->
        if x.length + acc >= number then 
          let segment = BufferSegment.mk x.buffer (x.offset  + (number - acc)) (x.length - number + acc)
          segment :: tail
        else loop tail (acc + x.length)
    loop pairs 0

  let split index connection select markerLength : Async<int * Connection> =
    let rec loop connection acc count =  async {
      match connection.segments with
      | [] -> return count, connection
      | pair :: tail ->
        if acc + pair.length < index then
          do! select (ArraySegment(pair.buffer.Array, pair.offset, pair.length)) pair.length
          connection.bufferManager.FreeBuffer( pair.buffer, "Suave.Web.split")
          return! loop {connection with segments = tail } (acc + pair.length) (count + acc + pair.length)
        elif acc + pair.length >= index then
          let bytesRead = index - acc
          do! select (ArraySegment(pair.buffer.Array, pair.offset, bytesRead)) bytesRead
          let remaining = pair.length - bytesRead
          if remaining = markerLength then
            connection.bufferManager.FreeBuffer( pair.buffer, "Suave.Web.split")
            return count + bytesRead, { connection with segments = tail }
          else
            if remaining - markerLength >= 0 then
              let segment = BufferSegment.mk pair.buffer
                                             (pair.offset  + bytesRead  + markerLength)
                                             (remaining - markerLength)
              return count + bytesRead, { connection with segments = segment :: tail}
            else
              let newTail = skipBuffers tail (markerLength - remaining)
              return count + bytesRead, { connection with segments = newTail }
        else return failwith "Suave.Web.split: invalid case"
      }
    loop connection 0 0

  type ScanResult = NeedMore | Found of int

  /// Iterates over a BufferSegment list looking for a marker, data before the marker 
  /// is sent to the function select and the corresponding buffers are released
  /// Returns the number of bytes read.
  let scanMarker marker select connection = socket {

    match kmpZ marker connection.segments with
    | Some x -> 
      let! res, connection = SocketOp.ofAsync <| split x connection select marker.Length
      return Found res, connection
    | None   ->
      let rec loop (xs : BufferSegment list) (acc,n) =
        if n >= marker.Length then
          acc,xs
        else
          match xs with
          | x :: tail -> loop tail (acc @ [x],n + x.length)
          | []  -> acc,[]
      let rev = List.rev connection.segments
      let ret, free = loop rev ([],0)
      for b in free do
        assert (b.length >= 0)
        do! SocketOp.ofAsync <| select (ArraySegment(b.buffer.Array, b.offset, b.length)) b.length
        do connection.bufferManager.FreeBuffer( b.buffer,"Suave.Web.scanMarker" )
      return NeedMore, { connection with segments = ret }
    }

  let inline readData (connection : Connection) buff = socket {
    let! b = receive connection buff
    if b > 0 then
      return BufferSegment(buff, buff.Offset, b)
    else
      return! SocketOp.abort (Error.SocketError SocketError.Shutdown)
    }

  let inline readMoreData connection = async {
    let buff = connection.bufferManager.PopBuffer("Suave.Web.readMoreData")
    let! result = readData connection buff
    match result with
    | Choice1Of2 data ->
      return { connection with segments = connection.segments @ [data] } |> Choice1Of2
    | Choice2Of2 error ->
      for b in connection.segments do
        do connection.bufferManager.FreeBuffer(b.buffer, "Suave.Web.readMoreData")
      do connection.bufferManager.FreeBuffer(buff, "Suave.Web.readMoreData")
      return Choice2Of2 error
    }

  /// Read the passed stream into buff until the EOL (CRLF) has been reached
  /// and returns the number of bytes read and the connection
  let readUntilPattern (connection : Connection) scanData =
    let rec loop (connection : Connection)  = socket {
      let! res, connection = scanData connection
      match res with
      | Found a ->
        return (a, connection)
      | NeedMore ->
        let! connection = readMoreData connection
        return! loop connection
    }
    loop connection

  /// returns the number of bytes read and the connection
  let inline readUntilEOL (connection : Connection) select =
    readUntilPattern connection (scanMarker EOL select)

  /// Read the stream until the marker appears and return the number of bytes
  /// read.
  let inline readUntil (marker : byte []) (select : ArraySegment<_> -> int -> Async<unit>) (connection : Connection) =
    readUntilPattern connection (scanMarker marker select)

  let parseTraceHeaders (headers : NameValueList) =
    let tryParseUint64 x = 
      match UInt64.TryParse x with 
      | true, value -> Choice1Of2 value
      | false, _    -> Choice2Of2 (sprintf "Couldn't parse '%s' to int64" x)
    let parent = "x-b3-spanid"  |> getFirst headers |> Choice.bind tryParseUint64 |> Option.ofChoice
    let trace  = "x-b3-traceid" |> getFirst headers |> Choice.bind tryParseUint64 |> Option.ofChoice
    TraceHeader.mk trace parent

  /// Read a line from the stream, calling ASCII.toString on the bytes before the EOL marker
  let inline readLine (connection : Connection) = socket {
    let offset = ref 0
    let buf = connection.lineBuffer
    let! count, connection =
      readUntilEOL connection (fun a count -> async {
        Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count
        offset := !offset + count
      })
    let result = ASCII.toStringAtOffset buf.Array buf.Offset count
    return result, connection
  }

  /// Read all headers from the stream, returning a dictionary of the headers found
  let inline readHeaders connection =
    let rec loop (connection : Connection) headers = socket {
      let offset = ref 0
      let buf = connection.lineBuffer
      let! count, connection =
        readUntilEOL connection (fun a count -> async {
          Array.blit a.Array a.Offset buf.Array (buf.Offset + !offset) count
          offset := !offset + count
        })
      if count <> 0 then
        let line = ASCII.toStringAtOffset buf.Array buf.Offset count
        let indexOfColon = line.IndexOf(':')
        let header = (line.Substring(0, indexOfColon).ToLower(), line.Substring(indexOfColon+1).TrimStart())
        return! loop connection (header :: headers)
      else return (headers, connection )
    }
    loop connection []

  let inline arraySegmentFromBufferSegment (b: BufferSegment) =
    ArraySegment(b.buffer.Array, b.offset, b.length)

  /// Read the post data from the stream, given the number of bytes that makes up the post data.
  let readPostData (connection : Connection) (bytes : int) select  : SocketOp<Connection> =

    let rec loop n (connection : Connection) : SocketOp<Connection> =
      socket {
        match connection.segments with
        | segment :: tail ->
          if segment.length > n then
            do! SocketOp.ofAsync <| select (arraySegmentFromBufferSegment(BufferSegment(segment.buffer,n,segment.length))) n
            return { connection with segments = BufferSegment(segment.buffer, segment.offset + n, segment.length - n):: tail }
          else
            do! SocketOp.ofAsync <| select (arraySegmentFromBufferSegment segment) segment.length
            do connection.bufferManager.FreeBuffer(segment.buffer, "Suave.Web.readPostData.loop")
            return! loop (n - segment.length) { connection with segments = tail }
        | [] ->
          if n = 0 then
            return connection
          else
            let! connection = readMoreData connection
            return! loop n connection
      }
    loop bytes connection

  let readFilePart boundary ctx (headerParams : Dictionary<string,string>) fieldName contentType = socket {
    let tempFilePath = Path.GetTempFileName()
    let tempFile = new FileStream(tempFilePath, FileMode.Truncate)
    let! a, connection =
      readUntil (ASCII.bytes (eol + boundary)) (fun x y -> async {
          do! tempFile.AsyncWrite(x.Array, x.Offset, y)
          }) ctx.connection
    let fileLength = tempFile.Length
    tempFile.Dispose()

    if fileLength > 0L then
      let! filename =
        (headerParams.TryLookup "filename" |> Choice.map (String.trimc '"'))
        @|! "Key 'filename' was not present in 'content-disposition'"

      let upload =
        { fieldName    = fieldName
          fileName     = filename
          mimeType     = contentType
          tempFilePath = tempFilePath }

      return connection, Some upload
    else
      File.Delete tempFilePath
      return connection, None
    }

  let parseMultipartMixed fieldName boundary (context : HttpContext) : SocketOp<HttpContext> =
    let verbose = Log.verbose context.runtime.logger "Suave.Web.parseMultipartMixed" context.request.trace
    let verbosef = Log.verbosef context.runtime.logger "Suave.Web.parseMultipartMixed" context.request.trace

    let rec loop (ctx : HttpContext) = socket {
      let! firstLine, connection = readLine ctx.connection

      if not (firstLine.Equals("--")) then
        let! partHeaders, connection = readHeaders connection
        let! (contentDisposition : string) =
          (partHeaders %% "content-disposition")
          @|! "Missing 'content-disposition'"

        match partHeaders %% "content-type" with
        | Choice1Of2 contentType ->
          let headerParams = headerParams contentDisposition
          verbosef (fun f -> f "parsing content type %s -> readFilePart" contentType)
          let! res = readFilePart boundary ctx headerParams fieldName contentType
          verbosef (fun f -> f "parsing content type %s -> readFilePart" contentType)

          match res with
          | connection, Some upload -> 
            return! loop { ctx with request = { ctx.request with files = upload :: ctx.request.files }
                                    connection = connection }
          | connection, None ->
            return! loop { ctx with connection = connection }

        | Choice2Of2 _ ->
          use mem = new MemoryStream()
          let! a, connection =
            readUntil (ASCII.bytes(eol + boundary)) (fun x y -> async {
                do! mem.AsyncWrite(x.Array, x.Offset, y)
              }) connection
          let byts = mem.ToArray()
          return! loop { ctx with request = { ctx.request with multiPartFields = (fieldName, ASCII.toStringAtOffset byts 0 byts.Length)::(ctx.request.multiPartFields) }; connection = connection}
      else
        return { ctx with connection = connection }
      }

    loop context

  /// Parses multipart data from the stream, feeding it into the HttpRequest's property Files.
  let parseMultipart boundary (context : HttpContext) : SocketOp<HttpContext> =
    let verbosef = Log.verbosef context.runtime.logger "Suave.Web.parseMultipart" context.request.trace

    let rec loop (ctx : HttpContext) = socket {
      let! firstLine, connection = readLine ctx.connection

      if firstLine.Equals("--") || firstLine.Equals(boundary + "--") || firstLine.Equals("--" + boundary) then
        return { ctx with connection = connection }
      else
        let! partHeaders, connection = readHeaders connection
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
          let! ctx = parseMultipartMixed fieldName subboundary { ctx with connection = connection }
          return! loop ctx

        | Choice1Of2 contentType ->
          verbosef (fun f -> f "parsing content type %s -> readFilePart" contentType)
          let! res = readFilePart boundary { ctx with connection = connection } headerParams fieldName contentType
          verbosef (fun f -> f "parsing content type %s <- readFilePart" contentType)

          match res with
          | connection, Some upload -> 
            return! loop { ctx with request = { ctx.request with files = upload :: ctx.request.files }
                                    connection = connection }
          | connection, None ->
            return! loop { ctx with connection = connection }

        | Choice2Of2 _ ->
          use mem = new MemoryStream()
          let! a, connection =
            readUntil (ASCII.bytes(eol + boundary)) (fun x y -> async {
                do! mem.AsyncWrite(x.Array, x.Offset, y)
              }) connection
          let byts = mem.ToArray()
          let fields = (fieldName, ASCII.toStringAtOffset byts 0 byts.Length) :: ctx.request.multiPartFields
          return! loop { ctx with request = { ctx.request with multiPartFields = fields }
                                  connection = connection }
      }

    loop context

  /// Reads raw POST data
  let getRawPostData connection contentLength =
    socket {
      let offset = ref 0
      let rawForm = Array.zeroCreate contentLength
      let! connection =
        readPostData connection contentLength (fun a count -> async {
          Array.blit a.Array a.Offset rawForm !offset count;
          offset := !offset + count
        })
      return rawForm, connection
    }

  let parsePostData (ctx : HttpContext) = socket {
    let verbosef = Log.verbosef ctx.runtime.logger "Suave.Web.parsePostData" ctx.request.trace
    let verbose = Log.verbose ctx.runtime.logger "Suave.Web.parsePostData" ctx.request.trace
    let request = ctx.request

    match request.header "content-length" with 
    | Choice1Of2 contentLengthString ->
      let contentLength = Convert.ToInt32 contentLengthString
      verbosef (fun f -> f "expecting content length %d" contentLength)

      match request.header "content-type" with
      | Choice1Of2 ce when String.startsWith "application/x-www-form-urlencoded" ce ->
        let! rawForm, connection = getRawPostData ctx.connection contentLength
        return Some { ctx with request = { request with rawForm = rawForm}
                               connection = connection }

      | Choice1Of2 ce when String.startsWith "multipart/form-data" ce ->
        let boundary = "--" + (ce |> String.substring (ce.IndexOf('=') + 1) |> String.trimStart |> String.trimc '"')

        verbose "parsing multipart"
        let! ctx = parseMultipart boundary ctx
        return Some ctx

      | Choice1Of2 _ | Choice2Of2 _ ->
        let! rawForm, connection = getRawPostData ctx.connection contentLength
        return Some { ctx with request = { request with rawForm = rawForm}; connection = connection }
    | Choice2Of2 _ -> return Some ctx
    }

  /// Process the request, reading as it goes from the incoming 'stream', yielding a HttpRequest
  /// when done
  let processRequest (ctx : HttpContext) = socket {
    let verbose = Log.verbose ctx.runtime.logger "Suave.Web.processRequest" ctx.request.trace

    verbose "reading first line of request"
    let! firstLine, connection' = readLine ctx.connection

    let rawMethod, path, rawQuery, httpVersion = parseUrl firstLine
    let meth = HttpMethod.parse rawMethod

    verbose "reading headers"
    let! headers, connection'' = readHeaders connection'

    // Respond with 400 Bad Request as
    // per http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
    let! host =
      (headers %% "host" |> Choice.map (function
        | s when System.Text.RegularExpressions.Regex.IsMatch(s, ":\d+$") ->
          s.Substring(0, s.LastIndexOf(':'))
        | s -> s))
      @|! "Missing 'Host' header"

    let request =
      { httpVersion      = httpVersion
        url              = ctx.runtime.matchedBinding.uri path rawQuery
        host             = host
        ``method``       = meth  
        headers          = headers
        rawForm          = [||]
        rawQuery         = rawQuery
        files            = []
        multiPartFields  = []
        trace            = parseTraceHeaders headers }

    if request.headers %% "expect" = Choice1Of2 "100-continue" then
      let! _ = HttpOutput.run Intermediate.CONTINUE ctx
      verbose "sent 100-continue response"

    if ctx.runtime.parsePostData then
      verbose "parsing post data"
      return! parsePostData { ctx with request = request; connection = connection'' }
    else
      verbose "avoiding to parse post data"
      return Some { ctx with request = request; connection = connection'' }
  }

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

  let httpLoop (ctxOuter : HttpContext) (consumer : WebPart) =

    let runtime = ctxOuter.runtime

    let rec loop (ctx : HttpContext) = async {

      let verbose  = Log.verbose runtime.logger "Suave.Web.httpLoop.loop" TraceHeader.empty
      let verbosef = Log.verbosef runtime.logger "Suave.Web.httpLoop.loop" TraceHeader.empty

      verbose "-> processor"
      let! result' = processRequest ctx
      verbose "<- processor"
      match result' with
      | Choice1Of2 result ->
        match result with
        | None -> verbose "'result = None', exiting"
        | Some ctx ->
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
          let! result''' = HttpOutput.run (RequestErrors.BAD_REQUEST msg) ctx
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

