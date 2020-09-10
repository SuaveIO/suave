module Suave.Proxy

open System
open System.Net
open Suave
open Suave.Utils
open Suave.Operators
open Suave.Successful
open Suave.Sockets
open Suave.Sockets.Control

let private (?) headers (name : string)  =
  headers
  |> Seq.tryFind (fun (k, _) -> String.Equals(k, name, StringComparison.OrdinalIgnoreCase))
  |> Option.map snd

let private httpWebResponseToHttpContext (ctx : HttpContext) (response : HttpWebResponse) =
  let status =
    match HttpCode.tryParse (int response.StatusCode) with
    | Choice1Of2 x -> x.status
    | _ -> HTTP_502.status

  let headers =
    response.Headers.AllKeys
    |> Seq.map (fun k -> k, response.Headers.Get k)
    |> Seq.toList

  let writeContentLengthHeader conn = socket {
    match headers ? ("Content-Length") with
    | Some x ->
      let! (_, conn) = asyncWriteLn (sprintf "Content-Length: %s" x) conn
      return conn
    | None ->
      return conn
    }

  let content =
    SocketTask
      (fun (conn, _) -> socket {
          let! conn = writeContentLengthHeader conn
          let! (_, conn) = asyncWriteLn "" conn
          let! conn = flush conn
          let stream = response.GetResponseStream ()
          do! transferStream conn stream
          return conn
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
          sprintf "%s://%s%s" newHost.Scheme newHost.Host ctx.request.path
        else
          sprintf "%s://%s:%i%s" newHost.Scheme newHost.Host newHost.Port ctx.request.path

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
        ctx.runtime.logger.log
          Logging.Error
          (fun lvl ->
            Logging.Message.event lvl (sprintf "Unable to proxy the request %A %A. " ctx.request.rawMethod remappedAddress)
            |> Logging.Message.addExn exn)

        return!
          (
            OK "Unable to proxy the request. "
            >=> Writers.setStatus HTTP_502
          ) ctx
    })
