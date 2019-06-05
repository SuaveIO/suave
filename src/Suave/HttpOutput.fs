namespace Suave

module HttpOutput =

  open Suave.Globals
  open Suave.Sockets
  open Suave.Sockets.Control
  open Suave.Utils
  open Suave.Logging
  open Suave.Logging.Message

  open System

  let inline writeContentType (headers : (string*string) list) = withConnection {
    if not(List.exists(fun (x : string,_) -> x.ToLower().Equals("content-type")) headers )then
      do! asyncWriteLn "Content-Type: text/html"
  }

  let addKeepAliveHeader (context : HttpContext) =
    match context.request.httpVersion, context.request.header "connection" with
    | "HTTP/1.0", Choice1Of2 v when String.equalsOrdinalCI v "keep-alive" ->
      { context with response = { context.response with headers = ("Connection","Keep-Alive") :: context.response.headers } }
    | _ -> context

  let inline writeContentLengthHeader (content : byte[]) (context : HttpContext) = withConnection {
    match context.request.``method``, context.response.status.code with
    | (_, 100)
    | (_, 101)
    | (_, 204)
    | (HttpMethod.CONNECT, 201)
    | (HttpMethod.CONNECT, 202)
    | (HttpMethod.CONNECT, 203)
    | (HttpMethod.CONNECT, 205)
    | (HttpMethod.CONNECT, 206) ->
      do! asyncWriteLn ""
    | _ ->
      do! asyncWriteLn (String.Concat [| "Content-Length: "; content.Length.ToString(); Bytes.eol |])
    }

  let inline writeHeaders exclusions (headers : (string*string) seq) = withConnection {
    for x,y in headers do
      if not (List.exists (fun y -> x.ToLower().Equals(y)) exclusions) then
        do! asyncWriteLn (String.Concat [| x; ": "; y |])
    }

  let inline writePreamble exclusions (context: HttpContext) = withConnection {

    let r = context.response

    do! asyncWriteLn (String.concat " " [ "HTTP/1.1"; r.status.code.ToString(); r.status.reason ])
    if not context.runtime.hideHeader then do! asyncWriteLn ServerHeader
    do! asyncWriteLn (String.Concat( [| "Date: "; Globals.utcNow().ToString("R") |]))

    do! writeHeaders exclusions r.headers
    do! writeContentType r.headers
    }

  let inline writeContent writePreamble context = function
    | Bytes b -> socket {
      let connection = context.connection
      let! (encoding, content : byte []) = Compression.transform b context connection
      match encoding with
      | Some n ->
        let! (_, connection) = asyncWriteLn (String.Concat [| "Content-Encoding: "; n.ToString() |]) connection
        // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
        // https://tools.ietf.org/html/rfc7230#section-3.3.2
        let! (_, connection) = writeContentLengthHeader content context connection
        if context.request.``method`` <> HttpMethod.HEAD && content.Length > 0 then
          let! (_,connection) = asyncWriteBufferedBytes content connection
          let! connection = flush connection
          return connection
        else
          let! connection = flush connection
          return connection
      | None ->
        // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
        // https://tools.ietf.org/html/rfc7230#section-3.3.2
        let! (_, connection) = writeContentLengthHeader content context connection
        if context.request.``method`` <> HttpMethod.HEAD && content.Length > 0 then
          let! (_,connection) = asyncWriteBufferedBytes content connection
          let! connection = flush connection
          return connection
        else
          let! connection = flush connection
          return connection
      }
    | SocketTask f -> socket {
        return! f (context.connection, context.response)
      }
    | NullContent -> socket {
        if writePreamble then
          let! (_, connection) = writeContentLengthHeader [||] context context.connection
          let! connection = flush connection
          return connection
        else
          let! connection = flush context.connection
          return connection
           }

  let flushChunk conn = socket {
    let! conn = flush conn
    return (), conn
  }

  let inline writeChunk (chunk : byte []) = withConnection {
    let chunkLength = chunk.Length.ToString("X")
    do! asyncWriteLn chunkLength
    do! asyncWriteLn (System.Text.Encoding.UTF8.GetString(chunk))
    do! flushChunk
  }

  let inline executeTask ctx r errorHandler = async {
    try
      let! q  = r
      return q
    with ex ->
      return! errorHandler ex "request failed" ctx
  }

  let writeResponse (newCtx:HttpContext) =
    socket{
      if newCtx.response.writePreamble then
        let! (_, connection) = writePreamble ["server";"date";"content-length"] newCtx newCtx.connection
        let! connection = writeContent true { newCtx with connection = connection } newCtx.response.content
        return { newCtx with connection = connection }
      else
        let! connection =  writeContent false newCtx newCtx.response.content
        return { newCtx with connection = connection }
        }

  /// Check if the web part can perform its work on the current request. If it
  /// can't it will return None and the run method will return.
  let inline run (webPart : WebPart) ctx = 
    async {
      match! executeTask ctx (webPart ctx) ctx.runtime.errorHandler with 
      | Some newCtx ->
        match! writeResponse newCtx with
        | Choice1Of2 ctx -> return Some ctx
        | Choice2Of2 err ->
          newCtx.runtime.logger.error (eventX "Socket error while writing response {error}" >> setFieldValue "error" err)
          return None
      | None ->
        return None
  }
