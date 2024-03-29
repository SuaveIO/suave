[<AutoOpen>]
module Suave.Sockets.AsyncSocket

open Suave.Utils
open Suave.Sockets
open Suave.Sockets.Connection
open Suave.Sockets.Control

open System
open System.IO
open System.Text

let transferStreamWithBuffer (buf: ArraySegment<_>) (toStream : Connection) (from : Stream) : SocketOp<unit> =
  task {
    try
      let reading = ref true
      let error = ref false
      let errorResult = ref (Ok())
      while !reading && not !error do
        let! read = from.ReadAsync (buf.Array, 0, buf.Array.Length)
        if read <= 0 then
          reading := false
        else
          let! a = send toStream (new Memory<_>(buf.Array, 0, read))
          match a with
          | Ok () -> ()
          | Result.Error e as a ->
            error := true
            errorResult := a
      if !error then
        return !errorResult
      else
        return Ok ()
    with ex ->
      return Result.Error(Error.ConnectionError ex.Message)
    }

/// Asynchronously write from the 'from' stream to the 'to' stream.
let transferStream (toStream : Connection) (from : Stream) : SocketOp<unit> =
  socket {
    let buf = new ArraySegment<_>(Array.zeroCreate 8192)
    do! transferStreamWithBuffer buf toStream from
  }

let internal zeroCharMemory = new Memory<byte>(Encoding.ASCII.GetBytes "0")

let transferStreamChunked (conn : Connection) (from : Stream) : SocketOp<unit> =
  socket {
    let buf = new ArraySegment<_>(Array.zeroCreate 1024)

    let rec doBlock conn =
      socket {
        let! read = SocketOp.ofAsync <| from.AsyncRead (buf.Array, buf.Offset, buf.Count)

        if read <= 0 then
          do! send conn zeroCharMemory
          do! send conn Bytes.eolMemory
          do! send conn Bytes.eolMemory
        else
          let readHex = read.ToString("X")

          do! send conn (Memory<_>(Encoding.ASCII.GetBytes readHex))
          do! send conn Bytes.eolMemory

          do! send conn (Memory<_>(buf.Array, buf.Offset, read))
          do! send conn Bytes.eolMemory

          do! doBlock conn
      }

    do! doBlock conn
  }
