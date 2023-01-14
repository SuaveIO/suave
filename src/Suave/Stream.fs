module Suave.Stream

open System
open System.IO
open Suave
open Suave.Sockets
open Suave.Sockets.Control

let private tryStreamLength (stream : Stream) =
  try
    ValueSome stream.Length
  with
    | :? NotSupportedException -> ValueNone
    | _ -> reraise ()

/// Send a stream back in the response with 200 status.
/// A new stream will be created for every request and it will be disposed after the request completes.
/// You are responsible for setting the MIME type.
let okStream (makeStream : Async<Stream>) : WebPart =
  fun ctx ->
    let write (conn, _) =
      socket {
        use! stream = SocketOp.ofAsync makeStream

        let maybeLength = tryStreamLength stream

        let! conn =
          match maybeLength with
          | ValueSome length ->
            socket {
              let start = 0L
              let total = length
              let finish = start + length - 1L

              let! (), conn = asyncWriteLn $"Content-Range: bytes %i{start}-%i{finish}/%i{total}" conn
              let! (), conn = asyncWriteLn $"Content-Length: %i{length}\r\n" conn
              let! conn = flush conn

              return conn
            }
          | ValueNone ->
            SocketOp.mreturn conn

        let shouldTransferStream =
          (ctx.request.``method`` <> HttpMethod.HEAD)
          && (
            maybeLength
            |> ValueOption.map (fun x -> x > 0L)
            |> ValueOption.defaultValue true
          )

        if shouldTransferStream then
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
