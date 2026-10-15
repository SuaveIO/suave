[<AutoOpen>]
module Suave.Sockets.AsyncSocket

open Suave.Utils
open Suave.Sockets
open Suave.Sockets.Connection
open Suave.Sockets.Control

open System
open System.IO
open System.Text

open System.Buffers

let inline private check (r : Result<unit, Error>) =
  match r with
  | Ok () -> ()
  | Result.Error e -> raise (SocketWriteException e)

/// Copy the 'from' stream to the connection using the given buffer. Raises
/// `SocketWriteException` when a write fails, e.g. because the client has
/// disconnected, so the copy stops instead of draining the stream.
let transferStreamWithBuffer (buf: ArraySegment<_>) (toStream : Connection) (from : Stream) =
  task {
      let mutable reading = true
      while reading do
        let! read = from.ReadAsync (buf.Array, 0, buf.Array.Length)
        if read <= 0 then
          reading <- false
        else
          let! r = send toStream (new Memory<byte>(buf.Array, 0, read))
          check r
    }

/// Asynchronously write from the 'from' stream to the 'to' stream.
let transferStream (toStream : Connection) (from : Stream) =
  task {
    let buf = Suave.Globals.BufferPool.rent 8192
    try
      do! transferStreamWithBuffer (new ArraySegment<_>(buf, 0, 8192)) toStream from
    finally
      Suave.Globals.BufferPool.returnArray buf true
  }

let internal zeroCharMemory = new Memory<byte>(Encoding.ASCII.GetBytes "0")

/// Copy the 'from' stream to the connection as HTTP/1.1 chunks, ending with
/// the terminating chunk. Raises `SocketWriteException` when a write fails.
let transferStreamChunked (conn : Connection) (from : Stream) =
  task {
    let buf = Suave.Globals.BufferPool.rent 1024
    try
      let mutable reading = true
      while reading do
        let! read = from.AsyncRead (buf, 0, 1024)
        if read <= 0 then
          let! r = send conn zeroCharMemory
          check r
          let! r = send conn Bytes.eolMemory
          check r
          let! r = send conn Bytes.eolMemory
          check r
          reading <- false
        else
          let readHex = read.ToString("X")

          let bytes = Encoding.ASCII.GetBytes readHex
          let! r = send conn (new Memory<byte>(bytes))
          check r
          let! r = send conn Bytes.eolMemory
          check r

          let! r = send conn (new Memory<byte>(buf, 0, read))
          check r
          let! r = send conn Bytes.eolMemory
          check r
    finally
      Suave.Globals.BufferPool.returnArray buf true
  }
