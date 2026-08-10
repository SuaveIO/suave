module Suave.Proxy

open System
open System.Net.Http
open System.Threading.Tasks
open Suave
open Suave.Utils
open Suave.Operators
open Suave.Successful
open Suave.Sockets

let private (?) headers (name : string)  =
  headers
  |> Seq.tryFind (fun (k, _) -> String.Equals(k, name, StringComparison.OrdinalIgnoreCase))
  |> Option.map snd

/// A single, shared `HttpClient` for all proxied requests: creating one per
/// request exhausts sockets under load.
let private client =
  let handler = new HttpClientHandler()
  handler.AllowAutoRedirect <- false
  handler.UseProxy <- false
  handler.Proxy <- null
  new HttpClient(handler)

let private httpResponseToHttpContext (ctx : HttpContext) (response : HttpResponseMessage) =
  let status =
    match HttpCode.tryParse (int response.StatusCode) with
    | Choice1Of2 x -> x.status
    | _ -> HTTP_502.status

  // `HttpClient` splits response headers between the message and its content.
  let allHeaders =
    [ for h in response.Headers do yield h.Key, String.Join(",", h.Value)
      for h in response.Content.Headers do yield h.Key, String.Join(",", h.Value) ]

  // Reading the response content stream transparently de-chunks a chunked
  // response body. If we forward the origin's `Transfer-Encoding: chunked`
  // header verbatim while writing the raw (already de-chunked) bytes, the
  // client will fail to parse the body (curl: "Illegal or missing hexadecimal
  // sequence in chunked-encoding"). Detect chunked responses and re-chunk the
  // body on the way out, filtering the hop-by-hop `Transfer-Encoding` and any
  // mutually-exclusive `Content-Length` header from the origin.
  let isChunked =
    if response.Headers.TransferEncodingChunked.GetValueOrDefault() then true
    else
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
          use response = response
          if not isChunked then
            do! writeContentLengthHeader conn
          do! conn.asyncWriteLn ""
          do! conn.flush()
          use! stream = response.Content.ReadAsStreamAsync()
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
    let work = task {
      let remappedAddress =
        if [ 80; 443 ] |> Seq.contains newHost.Port
        then
          $"{newHost.Scheme}://{newHost.Host}{ctx.request.path}"
        else
          $"{newHost.Scheme}://{newHost.Host}:{newHost.Port}{ctx.request.path}"

      use request = new HttpRequestMessage()
      request.Method <- new System.Net.Http.HttpMethod(ctx.request.rawMethod)
      request.RequestUri <- Uri remappedAddress

      if [ Suave.Http.HttpMethod.POST; Suave.Http.HttpMethod.PUT ] |> Seq.contains ctx.request.method then
        request.Content <- new ByteArrayContent(ctx.request.rawForm)

      let withContent f =
        match request.Content with
        | null -> ()
        | content -> f content

      match ctx.request.headers ? ("User-Agent") with
      | Some x -> request.Headers.TryAddWithoutValidation("User-Agent", x) |> ignore
      | None -> ()
      match ctx.request.headers ? ("Accept") with
      | Some x -> request.Headers.TryAddWithoutValidation("Accept", x) |> ignore
      | None -> ()
      match ctx.request.headers ? ("Date") |> Option.bind (Parse.dateTime >> Choice.toOption) with
      | Some x -> request.Headers.Date <- Nullable (DateTimeOffset x)
      | None -> ()
      match ctx.request.headers ? ("Host") with
      | Some x -> request.Headers.TryAddWithoutValidation("Host", x) |> ignore
      | None -> ()
      // Content-related headers live on the content, not on the request message.
      match ctx.request.headers ? ("Content-Type") with
      | Some x -> withContent (fun c -> c.Headers.TryAddWithoutValidation("Content-Type", x) |> ignore)
      | None -> ()
      match ctx.request.headers ? ("Content-Length") |> Option.bind (Parse.int64 >> Choice.toOption) with
      | Some x -> withContent (fun c -> c.Headers.ContentLength <- Nullable x)
      | None -> ()

      request.Headers.TryAddWithoutValidation("X-Forwarded-For", ctx.request.host) |> ignore

      try
        // `ResponseHeadersRead` keeps the body streaming instead of buffering it.
        // Non-2xx responses come back normally, so there is no error path to unwrap.
        let! response = client.SendAsync(request, HttpCompletionOption.ResponseHeadersRead)
        return httpResponseToHttpContext ctx response |> Some
      with
      // `HttpClient` surfaces connection failures as `HttpRequestException` /
      // `TaskCanceledException` and never exposes an error response object.
      | _ ->
        return!
          (
            OK "Unable to proxy the request. "
            >=> Writers.setStatus HTTP_502
          ) ctx
      }
    Async.AwaitTask work)
