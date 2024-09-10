[<AutoOpen>]
module Suave.Sockets.AsyncSocket

open Suave.Utils
open Suave.Sockets
open Suave.Sockets.Connection
open Suave.Sockets.Control

open System
open System.IO
open System.Text

let transferStreamWithBuffer (buf: ArraySegment<_>) (toStream : Connection) (from : Stream) =
  task {
      let reading = ref true
      while !reading do
        let! read = from.ReadAsync (buf.Array, 0, buf.Array.Length)
        if read <= 0 then
          reading := false
        else
          let! a = send toStream (new Memory<_>(buf.Array, 0, read))
          ()
    }

/// Asynchronously write from the 'from' stream to the 'to' stream.
let transferStream (toStream : Connection) (from : Stream) =
  task {
    let buf = new ArraySegment<_>(Array.zeroCreate 8192)
    do! transferStreamWithBuffer buf toStream from
  }

let internal zeroCharMemory = new Memory<byte>(Encoding.ASCII.GetBytes "0")

let transferStreamChunked (conn : Connection) (from : Stream) =
    task {
      let buf = new ArraySegment<_>(Array.zeroCreate 1024)
      let reading = ref true
      while !reading do
        let! read = from.AsyncRead (buf.Array, buf.Offset, buf.Count)
        if read <= 0 then
          let! _ = send conn zeroCharMemory
          let! _ = send conn Bytes.eolMemory
          let! _ = send conn Bytes.eolMemory
          reading := false
        else
          let readHex = read.ToString("X")

          let! _ = send conn (Memory<_>(Encoding.ASCII.GetBytes readHex))
          let! _ = send conn Bytes.eolMemory

          let! _ = send conn (Memory<_>(buf.Array, buf.Offset, read))
          let! _ = send conn Bytes.eolMemory
          ()
    }
