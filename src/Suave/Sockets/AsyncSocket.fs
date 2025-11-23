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

let transferStreamWithBuffer (buf: ArraySegment<_>) (toStream : Connection) (from : Stream) =
  task {
      let mutable reading = true
      while reading do
        let! read = from.ReadAsync (buf.Array, 0, buf.Array.Length)
        if read <= 0 then
          reading <- false
        else
          let! a = send toStream (new Memory<byte>(buf.Array, 0, read))
          ()
    }

/// Asynchronously write from the 'from' stream to the 'to' stream.
let transferStream (toStream : Connection) (from : Stream) =
  task {
    let buf = ArrayPool<byte>.Shared.Rent(8192)
    try
      do! transferStreamWithBuffer (new ArraySegment<_>(buf, 0, 8192)) toStream from
    finally
      ArrayPool<byte>.Shared.Return(buf, true)
  }

let internal zeroCharMemory = new Memory<byte>(Encoding.ASCII.GetBytes "0")

let transferStreamChunked (conn : Connection) (from : Stream) =
  task {
    let buf = ArrayPool<byte>.Shared.Rent(1024)
    try
      let mutable reading = true
      while reading do
        let! read = from.AsyncRead (buf, 0, 1024)
        if read <= 0 then
          let! _ = send conn zeroCharMemory
          let! _ = send conn Bytes.eolMemory
          let! _ = send conn Bytes.eolMemory
          reading <- false
        else
          let readHex = read.ToString("X")

          let bytes = Encoding.ASCII.GetBytes readHex
          let! _ = send conn (new Memory<byte>(bytes))
          let! _ = send conn Bytes.eolMemory

          let! _ = send conn (new Memory<byte>(buf, 0, read))
          let! _ = send conn Bytes.eolMemory
          ()
    finally
      ArrayPool<byte>.Shared.Return(buf, true)
  }
