namespace Suave

open System.Collections.Generic
open Suave.Utils
open Suave.Sockets

open System

#nowarn "3391"

module ByteConstants =

  // Pre-compute byte arrays as ReadOnlyMemory<byte> to avoid wrapping allocations
  let defaultContentTypeHeaderBytes = ReadOnlyMemory<byte>(ASCII.bytes "Content-Type: text/html\r\n")
  let serverHeaderBytes = ReadOnlyMemory<byte>(ASCII.bytes (Globals.ServerHeader + "\r\n"))

  let contentEncodingBytes = ReadOnlyMemory<byte>(ASCII.bytes "Content-Encoding: ")
  let contentLengthBytes = ReadOnlyMemory<byte>(ASCII.bytes "Content-Length: ")
  let EOL    =  ReadOnlyMemory<byte>(ASCII.bytes "\r\n")
  let EOLEOL =  ReadOnlyMemory<byte>(ASCII.bytes "\r\n\r\n")

  /// Format an integer to ASCII bytes without allocating a string
  /// Uses Span<T> for zero-allocation number formatting
  let formatIntToBytes (value: int) : byte[] =
    // Allocate buffer on stack for formatting (max 11 chars for int32: "-2147483648")
    let charBuffer = Array.zeroCreate<char>(11)
    let charSpan = System.Span<char>(charBuffer)
    let mutable charsWritten = 0
    
    // Use TryFormat for zero-allocation integer formatting
    if value.TryFormat(charSpan, &charsWritten) then
      // Convert chars directly to ASCII bytes without intermediate string
      let result = Array.zeroCreate<byte>(charsWritten)
      for i = 0 to charsWritten - 1 do
        result.[i] <- byte charBuffer.[i]
      result
    else
      // Fallback (should never happen for int32)
      ASCII.bytes (value.ToString())

  let httpVersionBytes = ReadOnlyMemory<byte>(ASCII.bytes "HTTP/1.1 ")
  let spaceBytes = ReadOnlyMemory<byte>(ASCII.bytes " ")
  let dateBytes = ReadOnlyMemory<byte>(ASCII.bytes "\r\nDate: ")
  let colonBytes = ReadOnlyMemory<byte>(ASCII.bytes ": ")
  
  // Pre-computed status code bytes for common HTTP status codes
  let statusCode200 = ReadOnlyMemory<byte>(ASCII.bytes "200")
  let statusCode201 = ReadOnlyMemory<byte>(ASCII.bytes "201")
  let statusCode204 = ReadOnlyMemory<byte>(ASCII.bytes "204")
  let statusCode301 = ReadOnlyMemory<byte>(ASCII.bytes "301")
  let statusCode302 = ReadOnlyMemory<byte>(ASCII.bytes "302")
  let statusCode304 = ReadOnlyMemory<byte>(ASCII.bytes "304")
  let statusCode400 = ReadOnlyMemory<byte>(ASCII.bytes "400")
  let statusCode401 = ReadOnlyMemory<byte>(ASCII.bytes "401")
  let statusCode403 = ReadOnlyMemory<byte>(ASCII.bytes "403")
  let statusCode404 = ReadOnlyMemory<byte>(ASCII.bytes "404")
  let statusCode500 = ReadOnlyMemory<byte>(ASCII.bytes "500")
  let statusCode502 = ReadOnlyMemory<byte>(ASCII.bytes "502")
  let statusCode503 = ReadOnlyMemory<byte>(ASCII.bytes "503")
  
  // Pre-computed reason phrases for common status codes
  let reason200 = ReadOnlyMemory<byte>(ASCII.bytes "OK")
  let reason201 = ReadOnlyMemory<byte>(ASCII.bytes "Created")
  let reason204 = ReadOnlyMemory<byte>(ASCII.bytes "No Content")
  let reason301 = ReadOnlyMemory<byte>(ASCII.bytes "Moved Permanently")
  let reason302 = ReadOnlyMemory<byte>(ASCII.bytes "Found")
  let reason304 = ReadOnlyMemory<byte>(ASCII.bytes "Not Modified")
  let reason400 = ReadOnlyMemory<byte>(ASCII.bytes "Bad Request")
  let reason401 = ReadOnlyMemory<byte>(ASCII.bytes "Unauthorized")
  let reason403 = ReadOnlyMemory<byte>(ASCII.bytes "Forbidden")
  let reason404 = ReadOnlyMemory<byte>(ASCII.bytes "Not Found")
  let reason500 = ReadOnlyMemory<byte>(ASCII.bytes "Internal Server Error")
  let reason502 = ReadOnlyMemory<byte>(ASCII.bytes "Bad Gateway")
  let reason503 = ReadOnlyMemory<byte>(ASCII.bytes "Service Unavailable")
  
  // Pre-computed common header names as bytes
  let headerContentType = ReadOnlyMemory<byte>(ASCII.bytes "Content-Type")
  let headerContentLength = ReadOnlyMemory<byte>(ASCII.bytes "Content-Length")
  let headerConnection = ReadOnlyMemory<byte>(ASCII.bytes "Connection")
  let headerLocation = ReadOnlyMemory<byte>(ASCII.bytes "Location")
  let headerCacheControl = ReadOnlyMemory<byte>(ASCII.bytes "Cache-Control")
  let headerSetCookie = ReadOnlyMemory<byte>(ASCII.bytes "Set-Cookie")
  let headerAccept = ReadOnlyMemory<byte>(ASCII.bytes "Accept")
  let headerUserAgent = ReadOnlyMemory<byte>(ASCII.bytes "User-Agent")
  let headerHost = ReadOnlyMemory<byte>(ASCII.bytes "Host")
  let headerUpgrade = ReadOnlyMemory<byte>(ASCII.bytes "Upgrade")
  
  /// Get pre-computed header name bytes if available, otherwise convert
  let getHeaderBytes (headerName: string) =
    match headerName.ToLowerInvariant() with
    | "content-type" -> headerContentType
    | "content-length" -> headerContentLength
    | "connection" -> headerConnection
    | "location" -> headerLocation
    | "cache-control" -> headerCacheControl
    | "set-cookie" -> headerSetCookie
    | "accept" -> headerAccept
    | "user-agent" -> headerUserAgent
    | "host" -> headerHost
    | "upgrade" -> headerUpgrade
    | _ -> ASCII.bytes headerName

type HttpOutput(connection: Connection, runtime: HttpRuntime) =

  let mutable freshContext =
        { connection = connection
        ; runtime = runtime
        ; request = HttpRequest.empty
        ; userState = Globals.DictionaryPool.Get()
        ; response = HttpResult.empty }
  
  // Expose connection as a property to enable inlining of write methods
  member val Connection = connection with get
       
  member inline this.writeContentType (headers : (string*string) list) = task {
    if not(List.exists(fun (x : string,_) -> x.ToLower().Equals("content-type")) headers )then
      return! this.Connection.asyncWriteBufferedBytes ByteConstants.defaultContentTypeHeaderBytes
  }

  member inline this.writeContentLengthHeader (content : byte[]) (context : HttpContext) = task {
    match context.request.``method``, context.response.status.code with
    | (_, 100)
    | (_, 101)
    | (_, 204)
    | (HttpMethod.CONNECT, 201)
    | (HttpMethod.CONNECT, 202)
    | (HttpMethod.CONNECT, 203)
    | (HttpMethod.CONNECT, 205)
    | (HttpMethod.CONNECT, 206) ->
      return! this.Connection.asyncWriteBufferedBytes ByteConstants.EOL
    | _ ->
      // Use Span-based formatting to avoid string allocation
      let lengthBytes = ByteConstants.formatIntToBytes content.Length
      // Write sequentially to avoid array allocation
      do! this.Connection.asyncWriteBufferedBytes ByteConstants.contentLengthBytes
      do! this.Connection.asyncWriteBufferedBytes lengthBytes
      return! this.Connection.asyncWriteBufferedBytes ByteConstants.EOLEOL
    }

  member inline this.writeHeaders exclusions (headers : (string*string) seq) = task {
    use sourceEnumerator = headers.GetEnumerator()
    while sourceEnumerator.MoveNext() do
      let x,y = sourceEnumerator.Current
      if not (List.exists (fun y -> x.ToLower().Equals(y)) exclusions) then
        // Use pre-computed header name bytes when available
        let headerNameBytes = ByteConstants.getHeaderBytes x
        do! this.Connection.asyncWriteBufferedBytes headerNameBytes
        do! this.Connection.asyncWrite ": "
        do! this.Connection.asyncWriteLn y
    }

  member this.writePreamble (response:HttpResult) = task {

    let r = response
    // Use pre-computed status code and reason bytes for common codes
    let statusCodeBytes, reasonBytes =
      match r.status.code with
      | 200 -> ByteConstants.statusCode200, ByteConstants.reason200
      | 201 -> ByteConstants.statusCode201, ByteConstants.reason201
      | 204 -> ByteConstants.statusCode204, ByteConstants.reason204
      | 301 -> ByteConstants.statusCode301, ByteConstants.reason301
      | 302 -> ByteConstants.statusCode302, ByteConstants.reason302
      | 304 -> ByteConstants.statusCode304, ByteConstants.reason304
      | 400 -> ByteConstants.statusCode400, ByteConstants.reason400
      | 401 -> ByteConstants.statusCode401, ByteConstants.reason401
      | 403 -> ByteConstants.statusCode403, ByteConstants.reason403
      | 404 -> ByteConstants.statusCode404, ByteConstants.reason404
      | 500 -> ByteConstants.statusCode500, ByteConstants.reason500
      | 502 -> ByteConstants.statusCode502, ByteConstants.reason502
      | 503 -> ByteConstants.statusCode503, ByteConstants.reason503
      | code -> ASCII.bytes (code.ToString()), ASCII.bytes (r.status.reason)
    
    // Write status line sequentially to avoid array allocation
    do! connection.asyncWriteBufferedBytes ByteConstants.httpVersionBytes
    do! connection.asyncWriteBufferedBytes statusCodeBytes
    do! connection.asyncWriteBufferedBytes ByteConstants.spaceBytes
    do! connection.asyncWriteBufferedBytes reasonBytes
    do! connection.asyncWriteBufferedBytes ByteConstants.dateBytes
    do! connection.asyncWriteBufferedBytes (Globals.DateCache.getHttpDateBytes())
    do! connection.asyncWriteBufferedBytes ByteConstants.EOL

    if runtime.hideHeader then
      do! this.writeHeaders ["date";"content-length"] r.headers
    else
      do! connection.asyncWriteBufferedBytes ByteConstants.serverHeaderBytes
      do! this.writeHeaders ["server";"date";"content-length"] r.headers
    do! this.writeContentType r.headers
    }

  member inline this.writeContent writePreamble context = function
    | Bytes b -> task {
      let! (encoding, content : byte []) = Compression.transform b context 
      match encoding with
      | Some n ->
        // Write Content-Encoding header without string concatenation
        do! this.Connection.asyncWrite "Content-Encoding: "
        do! this.Connection.asyncWriteLn (n.ToString())
        // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
        // https://tools.ietf.org/html/rfc7230#section-3.3.2
        do! this.writeContentLengthHeader content context
        if context.request.``method`` <> HttpMethod.HEAD && content.Length > 0 then
          do! this.Connection.asyncWriteBufferedBytes content
          do! this.Connection.flush()
        else
          do! this.Connection.flush()
      | None ->
        // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
        // https://tools.ietf.org/html/rfc7230#section-3.3.2
        do! this.writeContentLengthHeader content context
        if context.request.``method`` <> HttpMethod.HEAD && content.Length > 0 then
          do! this.Connection.asyncWriteBufferedBytes content
          do! this.Connection.flush()
        else
          do! this.Connection.flush()
      }
    | SocketTask f -> task{
      do! f (this.Connection, context.response)
      // CRITICAL: Must flush lineBuffer after SocketTask to prevent data bleeding
      // in keep-alive scenarios. SocketTask is used for file transfers.
      do! this.Connection.flush()
      }
    | NullContent -> task {
        if writePreamble then
          do! this.writeContentLengthHeader [||] context
          do! this.Connection.flush()
        else
          do! this.Connection.flush()
           }

  member this.executeTask task  = async {
    try
      let! q = task
      return q
    with ex ->
      return! runtime.errorHandler ex "request failed" { HttpContext.empty with connection = connection; runtime = runtime }
  }

  member this.writeResponse (newCtx:HttpContext) =
    task{
      if newCtx.response.writePreamble then
        do! this.writePreamble newCtx.response
        do! this.writeContent true newCtx newCtx.response.content
      else
        do! this.writeContent false newCtx newCtx.response.content
        }

  /// Check if the web part can perform its work on the current request. If it
  /// can't it will return None and the run method will return.
  member this.run (request:HttpRequest) (webPart : WebPart) = 
    task {
      try
        freshContext.request <- request
        freshContext.userState.Clear()
        let task = webPart freshContext
        match! this.executeTask task with 
        | Some ctx ->
          let! _ = this.writeResponse ctx
          let keepAlive =
            match ctx.request.header "connection" with
            | Choice1Of2 conn ->
              String.equalsOrdinalCI conn "keep-alive"
            | Choice2Of2 _ ->
              ctx.request.httpVersion.Equals("HTTP/1.1")
          return Ok (keepAlive)
        | None ->
          return Ok (false)
      with ex ->
        return Result.Error(Error.ConnectionError ex.Message)
  }
