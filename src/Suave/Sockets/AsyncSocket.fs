[<AutoOpen>]
module Suave.Sockets.AsyncSocket

open Suave.Utils
open Suave.Sockets
open Suave.Sockets.Connection
open Suave.Sockets.Control

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Net.Sockets
open System.Threading.Tasks
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
      do! send connection (new ArraySegment<_>(buff.Array, buff.Offset, lineBufferCount))
    return { connection with lineBufferCount = 0 }
  }

let asyncWrite (s : string) (connection : Connection) : SocketOp<unit*Connection> = 
  socket {
    if s.Length > 0 then
      let buff = connection.lineBuffer
      let lineBufferCount = connection.lineBufferCount
      assert (s.Length < buff.Count - lineBufferCount)
      if lineBufferCount + s.Length > buff.Count then
        do! send connection (new ArraySegment<_>(buff.Array, buff.Offset, lineBufferCount))
        let c = Encoding.UTF8.GetBytes(s, 0, s.Length, buff.Array, buff.Offset)
        return (),{ connection with lineBufferCount = c }
      else
        let c = Encoding.UTF8.GetBytes(s, 0, s.Length, buff.Array, buff.Offset + lineBufferCount )
        return (),{ connection with lineBufferCount = lineBufferCount + c }
    else return (),connection
  }

let asyncWriteLn (s : string) (connection : Connection) : SocketOp<unit*Connection> = 
  socket {
    return! asyncWrite (s + Bytes.eol) connection
  }

/// Write the string s to the stream asynchronously from a byte array
let asyncWriteBytes (connection : Connection) (b : byte[]) : SocketOp<unit> = async {
  if b.Length > 0 then return! send connection (new ArraySegment<_>(b, 0, b.Length))
  else return Choice1Of2 ()
}

let asyncWriteBufferedBytes (b : byte[]) (connection : Connection) : SocketOp<unit*Connection> = 
  socket {
    if b.Length > 0 then
      let buff = connection.lineBuffer
      let lineBufferCount = connection.lineBufferCount
      assert (b.Length < buff.Count - lineBufferCount)
      if lineBufferCount + b.Length > buff.Count then
        do! send connection (new ArraySegment<_>(buff.Array, buff.Offset, lineBufferCount))
        Array.Copy(b, 0, buff.Array, buff.Offset,b.Length)
        return (),{ connection with lineBufferCount = b.Length }
      else
        Array.Copy(b, 0, buff.Array, buff.Offset + lineBufferCount, b.Length)
        return (),{ connection with lineBufferCount = lineBufferCount + b.Length }
    else return (),connection
  }

let transferStreamWithBufer (buf: ArraySegment<_>) (toStream : Connection) (from : Stream) : SocketOp<unit> =
  let rec doBlock () = socket {
    let! read = SocketOp.ofAsync <| from.AsyncRead (buf.Array, buf.Offset, buf.Count)
    if read <= 0 then
      return ()
    else
      do! send toStream (new ArraySegment<_>(buf.Array, buf.Offset, read))
      return! doBlock () }
  doBlock ()

/// Asynchronously write from the 'from' stream to the 'to' stream.
let transferStream (toStream : Connection) (from : Stream) : SocketOp<unit> =
  socket {
    let buf = toStream.bufferManager.PopBuffer()
    do! finalize (transferStreamWithBufer buf toStream from) (fun () -> toStream.bufferManager.FreeBuffer buf)
  }