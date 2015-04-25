[<AutoOpen>]
module Suave.AsyncSocket

open Suave.Utils.Bytes
open Suave.Utils.Async
open Suave.Sockets
open Suave.Sockets.Connection

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Net.Sockets
open System.Threading.Tasks

/// A TCP Worker is a thing that takes a TCP client and returns an asynchronous workflow thereof
type TcpWorker<'a> = Connection -> SocketOp<'a>

/// lift a Async<'a> type to the Socket monad
let liftAsync (a : Async<'a>) : SocketOp<'a> = 
  async { 
    let! s = a
    return Choice1Of2 s 
  }

/// lift a Task type to the Socket monad
let liftTask (a : Task) : SocketOp<unit>  = 
  async {
    let! s = a
    return Choice1Of2 s 
  }

/// from the Socket monad to Async
let toAsync f = fun ctx -> async {
  let! o = f ctx
  match o with
  | Choice1Of2 option -> return option
  | Choice2Of2 error -> return failwith (sprintf "socket error: %A" error)
 }

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
  let rec do_block () = socket {
    let! read = liftAsync <| from.AsyncRead buf
    if read <= 0 then
      return ()
    else
      do! send toStream (new ArraySegment<_>(buf,0,read))
      return! do_block () }
  do_block ()

/// Asynchronously write from the 'from' stream to the 'to' stream, with an upper bound on
/// amount to transfer by len
let transferStreamBounded (toStream : Connection) (from : Stream) len =
  let buf_size = 0x2000
  let buf = Array.zeroCreate<byte> 0x2000
  let rec doBlock left = socket {
    let! read = liftAsync <| from.AsyncRead(buf, 0, Math.Min(buf_size, left))
    if read <= 0 || left - read = 0 then
      return ()
    else
      do! send toStream (new ArraySegment<_>(buf,0,read))
      return! doBlock (left - read) }
  doBlock len

[<System.Obsolete("Use liftAsync")>]
/// Obsolete
let lift_async a = liftAsync a
[<System.Obsolete("Use liftTask")>]
/// Obsolete
let lift_task a = liftTask a
[<System.Obsolete("Use toAsync")>]
/// Obsolete
let to_async f = toAsync f
[<System.Obsolete("Use asyncWrite")>]
/// Obsolete
let async_write connection s = asyncWrite connection s
[<System.Obsolete("Use asyncWriteNewLine")>]
/// Obsolete
let async_write_nl connection = asyncWriteNewLine connection
[<System.Obsolete("Use asyncWriteLn")>]
/// Obsolete
let async_writeln  connection s = asyncWriteLn connection s
[<System.Obsolete("Use asyncWriteBytes")>]
/// Obsolete
let async_writebytes  connection b = asyncWriteBytes connection b
[<System.Obsolete("Use transferStream")>]
/// Obsolete
let transfer_x toStream from = transferStream toStream from
[<System.Obsolete("Use transferStreamBounded")>]
/// Obsolete
let transfer_len_x toStream from len = transferStreamBounded toStream from len
