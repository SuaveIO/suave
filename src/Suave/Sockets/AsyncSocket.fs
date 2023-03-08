[<AutoOpen>]
module Suave.Sockets.AsyncSocket

open Suave.Utils
open Suave.Sockets
open Suave.Sockets.Connection
open Suave.Sockets.Control

open System
open System.IO
open System.Text

/// A TCP Worker is a thing that takes a TCP client and returns an asynchronous
/// workflow thereof.
type TcpWorker<'a> = Connection -> Async<'a>

/// Flush out whatever is in the lineBuffer
let flush (connection : Connection) : SocketOp<Connection> =
  socket {
    let buff = connection.lineBuffer
    let lineBufferCount = connection.lineBufferCount
    if lineBufferCount> 0  then
      do! send connection (new Memory<_>(buff,0,lineBufferCount))
    return { connection with lineBufferCount = 0 }
  }

let chunkBoundaries maxChunkBytes (s : string) =
  if maxChunkBytes < 6 then failwith "Cannot split into chunks of smaller than 6 bytes"
  // if maxChunkBytes < 6 then Seq.empty else
  seq {
    // One UTF-16 char can represent at most 3 UTF-8 bytes, because any 4-byte
    // UTF-8 sequences are represented by two UTF-16 chars (a surrogate pair).
    let maxChars = maxChunkBytes / 3
    let mutable charsLeft = s.Length
    let mutable charsSoFar = 0

    while charsLeft > 0 do
      let charsThisTime = min maxChars charsLeft
      let lastCharIdx = charsSoFar + charsThisTime - 1
      // Don't split a surrogate pair; if this segments ends on a
      let charsThisTime =
        if Char.IsHighSurrogate s.[lastCharIdx] then
          charsThisTime - 1
        else
          charsThisTime
      yield charsSoFar, charsThisTime

      charsSoFar <- charsSoFar + charsThisTime
      charsLeft <- charsLeft - charsThisTime
  }

let asyncWrite (str: string) (connection: Connection) : SocketOp<unit * Connection> =
  socket {
    if str.Length = 0 then
      return (), connection
    else
      let buff = connection.lineBuffer
      let lineBufferCount = connection.lineBufferCount
      let maxByteCount = Encoding.UTF8.GetMaxByteCount(str.Length)
      if maxByteCount > buff.Length then
        do! send connection (new Memory<_>(buff, 0, lineBufferCount))
        for offset, charCount in chunkBoundaries buff.Length str do
          let byteCount = Encoding.UTF8.GetBytes(str, offset, charCount, buff, 0)
          // don't waste time buffering here
          do! send connection (new Memory<_>(buff, 0, byteCount))
        return (), { connection with lineBufferCount = 0 }

      elif lineBufferCount + maxByteCount > buff.Length then
        do! send connection (new Memory<_>(buff, 0, lineBufferCount))
        // the string, char index, char count, bytes, byte index
        let c = Encoding.UTF8.GetBytes(str, 0, str.Length, buff, 0)
        return (), { connection with lineBufferCount = c }

      else
        let c = Encoding.UTF8.GetBytes(str, 0, str.Length, buff, lineBufferCount )
        return (), { connection with lineBufferCount = lineBufferCount + c }
  }

let asyncWriteLn (s : string) (connection : Connection) : SocketOp<unit*Connection> =
  socket {
    return! asyncWrite (s + Bytes.eol) connection
  }

/// Write the string s to the stream asynchronously from a byte array
let asyncWriteBytes (connection : Connection) (b : byte[]) : SocketOp<unit> = async {
  if b.Length > 0 then return! send connection (new Memory<_>(b, 0, b.Length))
  else return Choice1Of2 ()
}

let asyncWriteBufferedBytes (b : byte[]) (connection : Connection) : SocketOp<unit*Connection> =
  socket {
    if b.Length > 0 then
      let buff = connection.lineBuffer
      let lineBufferCount = connection.lineBufferCount
      if lineBufferCount + b.Length > buff.Length then
        // flush lineBuffer
        if lineBufferCount > 0 then
          do! send connection (new Memory<_>(buff, 0, lineBufferCount))
        // don't waste time buffering here
        do! send connection (new Memory<_>(b, 0, b.Length))
        return (), { connection with lineBufferCount = 0 }
      else
        Buffer.BlockCopy(b, 0, buff,0 + lineBufferCount, b.Length)
        return (),{ connection with lineBufferCount = lineBufferCount + b.Length }
    else return (),connection
  }

let asyncWriteBufferedArrayBytes (xxs:(byte[])[]) (connection : Connection) : SocketOp<unit*Connection> =
  let rec loop index cnn =
    socket{
      if index >= xxs.Length then
        return (),cnn
      else
        let! (_, cnn') = asyncWriteBufferedBytes xxs.[index] cnn
        return! loop (index + 1) cnn'
        }
  loop 0 connection

let transferStreamWithBuffer (buf: ArraySegment<_>) (toStream : Connection) (from : Stream) : SocketOp<unit> =
  let rec doBlock () = socket {
    let! read = SocketOp.ofAsync <| from.AsyncRead (buf.Array, 0, buf.Array.Length)
    if read <= 0 then
      return ()
    else
      do! send toStream (new Memory<_>(buf.Array, 0, read))
      return! doBlock () }
  doBlock ()

/// Asynchronously write from the 'from' stream to the 'to' stream.
let transferStream (toStream : Connection) (from : Stream) : SocketOp<unit> =
  socket {
    let buf = new ArraySegment<_>(Array.zeroCreate 1024)
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
