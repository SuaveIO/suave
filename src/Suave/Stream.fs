module Suave.Stream

open System.IO
open Suave
open Suave.Sockets
open Suave.Sockets.Control

/// Send a stream back in the response with 200 status.
/// A new stream will be created for every request and it will be disposed after the request completes.
/// You are responsible for setting the MIME type.
/// The stream must support the `Length` property.
let okStream (makeStream : Async<Stream>) : WebPart =
  fun ctx ->
    let write (conn, _) =
      socket {
        use! stream = SocketOp.ofAsync makeStream

        let! (), conn = asyncWriteLn $"Content-Length: %i{stream.Length}\r\n" conn
        let! conn = flush conn

        if ctx.request.``method`` <> HttpMethod.HEAD then
          do! transferStream conn stream

        return conn
      }

    {
      ctx with
        response =
          {
            ctx.response with
              status = HTTP_200.status
              content = SocketTask write
          }
    }
    |> succeed

/// Send a stream back in the response with 200 status using chunked transfer-encoding.
/// A new stream will be created for every request and it will be disposed after the request completes.
/// You are responsible for setting the MIME type.
let okStreamChunked (makeStream : Async<Stream>) : WebPart =
  fun ctx ->
    let write (conn, _) =
      socket {
        use! stream = SocketOp.ofAsync makeStream

        let! (), conn = asyncWriteLn "" conn
        let! conn = flush conn

        if ctx.request.``method`` <> HttpMethod.HEAD then
          do! transferStreamChunked conn stream

        return conn
      }

    {
      ctx with
        response =
          {
            ctx.response with
              status = HTTP_200.status
              headers =
                  ("Transfer-Encoding", "chunked") :: ctx.response.headers
              writePreamble = true
              content = SocketTask write
          }
    }
    |> succeed
