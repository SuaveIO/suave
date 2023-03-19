namespace Suave

open System.Collections.Generic
open Suave.Utils
open Suave.Sockets
open Suave.Sockets.Control
open Suave.Logging
open Suave.Logging.Message

open System

module ByteConstants =

  let defaultContentTypeHeaderBytes = ASCII.bytes "Content-Type: text/html\r\n"
  let serverHeaderBytes = ASCII.bytes (Globals.ServerHeader + "\r\n")

  let contentEncodingBytes = ASCII.bytes "Content-Encoding: "
  let contentLengthBytes = ASCII.bytes "Content-Length: "
  let EOL    =  ASCII.bytes "\r\n"
  let EOLEOL =  ASCII.bytes "\r\n\r\n"

  let httpVersionBytes = ASCII.bytes "HTTP/1.1 "
  let spaceBytes = ASCII.bytes " "
  let dateBytes = ASCII.bytes "\r\nDate: "
  let colonBytes = ASCII.bytes ": "

type HttpOutput(connection: Connection, runtime: HttpRuntime) =

  let mutable freshContext =
        { connection = connection
        ; runtime = runtime
        ; request = HttpRequest.empty
        ; userState = new Dictionary<string,obj>()
        ; response = HttpResult.empty }
       
  member (*inline*) this.writeContentType (headers : (string*string) list) = socket {
    if not(List.exists(fun (x : string,_) -> x.ToLower().Equals("content-type")) headers )then
      return! connection.asyncWriteBufferedBytes ByteConstants.defaultContentTypeHeaderBytes
  }

  member (*inline*) this.writeContentLengthHeader (content : byte[]) (context : HttpContext) = socket {
    match context.request.``method``, context.response.status.code with
    | (_, 100)
    | (_, 101)
    | (_, 204)
    | (HttpMethod.CONNECT, 201)
    | (HttpMethod.CONNECT, 202)
    | (HttpMethod.CONNECT, 203)
    | (HttpMethod.CONNECT, 205)
    | (HttpMethod.CONNECT, 206) ->
      return! connection.asyncWriteBufferedBytes ByteConstants.EOL
    | _ ->
      return! connection.asyncWriteBufferedArrayBytes [| ByteConstants.contentLengthBytes; ASCII.bytes (content.Length.ToString()); ByteConstants.EOLEOL |]
    }

  member (*inline*) this.writeHeaders exclusions (headers : (string*string) seq) = socket {
    for x,y in headers do
      if not (List.exists (fun y -> x.ToLower().Equals(y)) exclusions) then
        do! connection.asyncWriteLn (String.Concat [| x; ": "; y |])
    }

  member this.writePreamble (response:HttpResult) = socket {

    let r = response
    let preamble = [| ByteConstants.httpVersionBytes; ASCII.bytes (r.status.code.ToString());
      ByteConstants.spaceBytes; ASCII.bytes (r.status.reason); ByteConstants.dateBytes; ASCII.bytes (Globals.utcNow().ToString("R")); ByteConstants.EOL |]
    do! connection.asyncWriteBufferedArrayBytes preamble 

    if runtime.hideHeader then
      do! this.writeHeaders ["date";"content-length"] r.headers
    else
      do! connection.asyncWriteBufferedBytes ByteConstants.serverHeaderBytes
      do! this.writeHeaders ["server";"date";"content-length"] r.headers
    do! this.writeContentType r.headers
    }

  member (*inline*) this.writeContent writePreamble context = function
    | Bytes b -> socket {
      let! (encoding, content : byte []) = Compression.transform b context
      match encoding with
      | Some n ->
        do! connection.asyncWriteLn (String.Concat [| "Content-Encoding: "; n.ToString() |])
        // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
        // https://tools.ietf.org/html/rfc7230#section-3.3.2
        do! this.writeContentLengthHeader content context
        if context.request.``method`` <> HttpMethod.HEAD && content.Length > 0 then
          do! connection.asyncWriteBufferedBytes content
          do! connection.flush()
        else
          do! connection.flush()
      | None ->
        // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
        // https://tools.ietf.org/html/rfc7230#section-3.3.2
        do! this.writeContentLengthHeader content context
        if context.request.``method`` <> HttpMethod.HEAD && content.Length > 0 then
          do! connection.asyncWriteBufferedBytes content
          do! connection.flush()
        else
          do! connection.flush()
      }
    | SocketTask f -> socket{
      do! f (connection, context.response)
      }
    | NullContent -> socket {
        if writePreamble then
          do! this.writeContentLengthHeader [||] context
          do! connection.flush()
        else
          do! connection.flush()
           }

  member this.executeTask task  = async {
    try
      let! q = task
      return q
    with ex ->
      return! runtime.errorHandler ex "request failed" { HttpContext.empty with connection = connection; runtime = runtime }
  }

  member this.writeResponse (newCtx:HttpContext) =
    socket{
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
        //Console.WriteLine "before calling webpart"
        let task = webPart freshContext
        //Console.WriteLine "after calling webpart"
        //Console.WriteLine "before Execute task"
        match! this.executeTask task with 
        | Some ctx ->
          //Console.WriteLine "before writeresponse"
          match! this.writeResponse ctx with
          | Ok () ->
            //Console.WriteLine "after writeresponse"
            let keepAlive =
              match ctx.request.header "connection" with
              | Choice1Of2 conn ->
                String.equalsOrdinalCI conn "keep-alive"
              | Choice2Of2 _ ->
                ctx.request.httpVersion.Equals("HTTP/1.1")
            //Console.WriteLine "exiting httpOutput.run"
            return Ok (keepAlive)
          | Result.Error err ->
            ctx.runtime.logger.error (eventX "Socket error while writing response {error}" >> setFieldValue "error" err)
            return Result.Error err
        | None ->
          return Ok (false)
      with ex ->
        //Console.WriteLine "nother one bytes the doost"
        return Result.Error(Error.ConnectionError ex.Message)
  }
