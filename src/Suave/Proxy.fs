module Suave.Proxy

open System
open System.Net
open Suave
open Suave.Utils
open Suave.Operators
open Suave.Successful
open Suave.Sockets

let private (?) headers (name : string)  =
  headers
  |> Seq.tryFind (fun (k, _) -> String.Equals(k, name, StringComparison.OrdinalIgnoreCase))
  |> Option.map snd

let private httpWebResponseToHttpContext (ctx : HttpContext) (response : HttpWebResponse) =
  let status =
    match HttpCode.tryParse (int response.StatusCode) with
    | Choice1Of2 x -> x.status
    | _ -> HTTP_502.status

  let allHeaders =
    response.Headers.AllKeys
    |> Seq.map (fun k -> k, response.Headers.Get k)
    |> Seq.toList

  // `HttpWebResponse.GetResponseStream()` transparently de-chunks a chunked
  // response body. If we forward the origin's `Transfer-Encoding: chunked`
  // header verbatim while writing the raw (already de-chunked) bytes, the
  // client will fail to parse the body (curl: "Illegal or missing hexadecimal
  // sequence in chunked-encoding"). Detect chunked responses and re-chunk the
  // body on the way out, filtering the hop-by-hop `Transfer-Encoding` and any
  // mutually-exclusive `Content-Length` header from the origin.
  let isChunked =
    match allHeaders ? ("Transfer-Encoding") with
    | Some v ->
      v.Split(',')
      |> Array.exists (fun t -> String.Equals(t.Trim(), "chunked", StringComparison.OrdinalIgnoreCase))
    | None -> false

  let forwardedHeaders =
    if isChunked then
      allHeaders
      |> List.filter (fun (k, _) ->
          not (String.Equals(k, "Transfer-Encoding", StringComparison.OrdinalIgnoreCase))
          && not (String.Equals(k, "Content-Length", StringComparison.OrdinalIgnoreCase)))
    else
      allHeaders

  let headers =
    if isChunked then
      ("Transfer-Encoding", "chunked") :: forwardedHeaders
    else
      forwardedHeaders

  let writeContentLengthHeader (conn:Connection) = task {
    match allHeaders ? ("Content-Length") with
    | Some x -> do! conn.asyncWriteLn ($"Content-Length: {x}")
    | None -> ()
    }

  let content =
    SocketTask
      (fun (conn, _) -> task {
          if not isChunked then
            do! writeContentLengthHeader conn
          do! conn.asyncWriteLn ""
          do! conn.flush()
          let stream = response.GetResponseStream ()
          if isChunked then
            do! transferStreamChunked conn stream
          else
            do! transferStream conn stream
       })

  {
    ctx with
      response =
        {
          ctx.response with
            status = status
            headers = headers
            content = content
        }
  }

let proxy (newHost : Uri) : WebPart =
  (fun ctx ->
    async {
      let remappedAddress =
        if [ 80; 443 ] |> Seq.contains newHost.Port
        then
          $"{newHost.Scheme}://{newHost.Host}{ctx.request.path}"
        else
          $"{newHost.Scheme}://{newHost.Host}:{newHost.Port}{ctx.request.path}"

      let request = WebRequest.Create remappedAddress :?> HttpWebRequest

      request.Method <- ctx.request.rawMethod
      request.Proxy <- null
      request.AllowAutoRedirect <- false
      request.AllowReadStreamBuffering <- false
      request.AllowWriteStreamBuffering <- false

      match ctx.request.headers ? ("User-Agent") with | Some x -> request.UserAgent <- x | None -> ()
      match ctx.request.headers ? ("Accept") with | Some x -> request.Accept <- x | None -> ()
      match ctx.request.headers ? ("Date") |> Option.bind (Parse.dateTime >> Choice.toOption) with | Some x -> request.Date <- x | None -> ()
      match ctx.request.headers ? ("Host") with | Some x -> request.Host <- x | None -> ()
      match ctx.request.headers ? ("Content-Type") with | Some x -> request.ContentType <- x | None -> ()
      match ctx.request.headers ? ("Content-Length") |> Option.bind (Parse.int64 >> Choice.toOption) with | Some x -> request.ContentLength <- x | None -> ()

      request.Headers.Add("X-Forwarded-For", ctx.request.host)

      if [ HttpMethod.POST; HttpMethod.PUT ] |> Seq.contains ctx.request.method
      then
        let! requestStream =
          request.GetRequestStreamAsync ()
          |> Async.AwaitTask

        for b in ctx.request.rawForm do
          requestStream.WriteByte b

      try
        let! response = request.AsyncGetResponse ()
        let response = response :?> HttpWebResponse

        return httpWebResponseToHttpContext ctx response |> Some
      with
      | :? WebException as ex when not (isNull ex.Response) ->
        let response = ex.Response :?> HttpWebResponse

        return httpWebResponseToHttpContext ctx response |> Some
      | exn ->
        return!
          (
            OK "Unable to proxy the request. "
            >=> Writers.setStatus HTTP_502
          ) ctx
    })
