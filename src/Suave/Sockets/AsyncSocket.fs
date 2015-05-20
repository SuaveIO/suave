[<AutoOpen>]
module Suave.Sockets.AsyncSocket

open Suave.Utils.Bytes
open Suave.Utils.Async
open Suave.Sockets
open Suave.Sockets.Connection
open Suave.Sockets.Control

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Net.Sockets
open System.Threading.Tasks

/// A TCP Worker is a thing that takes a TCP client and returns an asynchronous
/// workflow thereof.
type TcpWorker<'a> = Connection -> Async<'a>

/// Write the string s to the stream asynchronously as ASCII encoded text
let asyncWrite (connection : Connection) (s : string) : SocketOp<unit> = 
  async {
    if s.Length > 0 then
      let buff = connection.lineBuffer
      let c = bytesToBuffer s buff.Array buff.Offset
      return! send connection (new ArraySegment<_>(buff.Array, buff.Offset, c))
    else return Choice1Of2 ()
  }

let asyncWriteNewLine (connection : Connection) = 
  send connection eolArraySegment

let asyncWriteLn (connection : Connection) (s : string) : SocketOp<unit> = 
  socket {
    do! asyncWrite connection s
    do! asyncWriteNewLine connection
  }

/// Write the string s to the stream asynchronously from a byte array
let asyncWriteBytes (connection : Connection) (b : byte[]) : SocketOp<unit> = async {
  if b.Length > 0 then return! send connection (new ArraySegment<_>(b, 0, b.Length))
  else return Choice1Of2 ()
}

/// Asynchronously write from the 'from' stream to the 'to' stream.
let transferStream (toStream : Connection) (from : Stream) : SocketOp<unit> =
  let buf = Array.zeroCreate<byte> 0x2000
  let rec doBlock () = socket {
    let! read = SocketOp.ofAsync <| from.AsyncRead buf
    if read <= 0 then
      return ()
    else
      do! send toStream (new ArraySegment<_>(buf,0,read))
      return! doBlock () }
  doBlock ()

/// Asynchronously write from the 'from' stream to the 'to' stream, with an upper bound on
/// amount to transfer by len
let transferStreamBounded (toStream : Connection) (from : Stream) len =
  let bufSize = 0x2000
  let buf = Array.zeroCreate<byte> bufSize
  let rec doBlock left = socket {
    let! read = SocketOp.ofAsync <| from.AsyncRead(buf, 0, Math.Min(bufSize, left))
    if read <= 0 || left - read = 0 then
      return ()
    else
      do! send toStream (new ArraySegment<_>(buf,0,read))
      return! doBlock (left - read) }
  doBlock len

[<System.Obsolete("Use SocketOp.ofAsync")>]
let lift_async a = SocketOp.ofAsync a
[<System.Obsolete("Use liftTask")>]
let lift_task a = SocketOp.ofTask a
[<System.Obsolete("Use asyncWrite")>]
let async_write connection s = asyncWrite connection s
[<System.Obsolete("Use asyncWriteNewLine")>]
let async_write_nl connection = asyncWriteNewLine connection
[<System.Obsolete("Use asyncWriteLn")>]
let async_writeln  connection s = asyncWriteLn connection s
[<System.Obsolete("Use asyncWriteBytes")>]
let async_writebytes  connection b = asyncWriteBytes connection b
[<System.Obsolete("Use transferStream")>]
let transfer_x toStream from = transferStream toStream from
[<System.Obsolete("Use transferStreamBounded")>]
let transfer_len_x toStream from len = transferStreamBounded toStream from len
