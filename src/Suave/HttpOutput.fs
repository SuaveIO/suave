namespace Suave

module HttpOutput =

  open Suave.Globals
  open Suave.Sockets
  open Suave.Sockets.Control
  open Suave.Utils

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

  let inline writeHeaders (headers : (string*string) seq) = withConnection {
    for x,y in headers do
      if not (List.exists (fun y -> x.ToLower().Equals(y)) ["server";"date";"content-length"]) then
        do! asyncWriteLn (String.Concat [| x; ": "; y |])
    }

  let inline writePreamble (context: HttpContext) = withConnection {

    let r = context.response

    do! asyncWriteLn (String.concat " " [ "HTTP/1.1"; r.status.code.ToString(); r.status.reason ])
    if not context.runtime.hideHeader then do! asyncWriteLn ServerHeader
    do! asyncWriteLn (String.Concat( [| "Date: "; Globals.utcNow().ToString("R") |]))

    do! writeHeaders r.headers
    do! writeContentType r.headers
    }

  let inline writeContent context = function
    | Bytes b -> socket {
      let connection = context.connection
      let! (encoding, content : byte []) = Compression.transform b context connection
      match encoding with
      | Some n ->
        let! (_, connection) = asyncWriteLn (String.Concat [| "Content-Encoding: "; n.ToString() |]) connection
        // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
        let! (_, connection) = asyncWriteLn (String.Concat [| "Content-Length: "; content.Length.ToString(); Bytes.eol |]) connection
        if context.request.``method`` <> HttpMethod.HEAD && content.Length > 0 then
          let! (_,connection) = asyncWriteBufferedBytes content connection
          let! connection = flush connection
          return connection
        else
          let! connection = flush connection
          return connection
      | None ->
        // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
        let! (_, connection) = asyncWriteLn (String.Concat [| "Content-Length: "; content.Length.ToString(); Bytes.eol |]) connection
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
    | NullContent -> socket { return context.connection }

  let inline executeTask ctx r errorHandler = async {
    try
      let! q  = r
      return q
    with ex ->
      return! errorHandler ex "request failed" ctx
  }

  /// Check if the web part can perform its work on the current request. If it
  /// can't it will return None and the run method will return.
  let inline run (webPart : WebPart) ctx = 
    socket {
      let! result = SocketOp.ofAsync <| executeTask ctx (webPart ctx) ctx.runtime.errorHandler
      match result with 
      | Some newCtx ->
        if newCtx.response.writePreamble then
          let! (_, connection) = writePreamble newCtx newCtx.connection
          let! connection = writeContent { newCtx with connection = connection } newCtx.response.content
          return Some { newCtx with connection = connection }
        else
          let! connection =  writeContent newCtx newCtx.response.content
          return Some { newCtx with connection = connection }
      | None -> return None
  }
