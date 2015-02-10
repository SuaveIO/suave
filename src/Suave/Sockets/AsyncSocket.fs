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
let lift_async (a : Async<'a>) : SocketOp<'a> = 
  async { 
    let! s = a
    return Choice1Of2 s 
  }

/// lift a Task type to the Socket monad
let lift_task (a : Task) : SocketOp<unit>  = 
  async {
    let! s = a
    return Choice1Of2 s 
  }

/// from the Socket monad to Async
let to_async f = fun ctx -> async {
  let! o = f ctx
  match o with
  | Choice1Of2 option -> return option
  | Choice2Of2 error -> return failwith (sprintf "socket error: %A" error)
 }

/// Write the string s to the stream asynchronously as ASCII encoded text
let async_write (connection : Connection) (s : string) : SocketOp<unit> = 
  async {
    if s.Length > 0 then
      let buff = connection.lineBuffer
      let c = bytes_to_buffer s buff.Array buff.Offset
      return! send connection (new ArraySegment<_>(buff.Array, buff.Offset, c))
    else return Choice1Of2 ()
  }

let async_write_nl (connection : Connection) = 
  send connection eol_array_segment

let async_writeln (connection : Connection) (s : string) : SocketOp<unit> = 
  socket {
    do! async_write connection s
    do! async_write_nl connection
  }

/// Write the string s to the stream asynchronously from a byte array
let async_writebytes (connection : Connection) (b : byte[]) : SocketOp<unit> = async {
  if b.Length > 0 then return! send connection (new ArraySegment<_>(b, 0, b.Length))
  else return Choice1Of2 ()
}

/// Asynchronously write from the 'from' stream to the 'to' stream.
let transfer_x (to_stream : Connection) (from : Stream) : SocketOp<unit> =
  let buf = Array.zeroCreate<byte> 0x2000
  let rec do_block () = socket {
    let! read = lift_async <| from.AsyncRead buf
    if read <= 0 then
      return ()
    else
      do! send to_stream (new ArraySegment<_>(buf,0,read))
      return! do_block () }
  do_block ()

/// Asynchronously write from the 'from' stream to the 'to' stream, with an upper bound on
/// amount to transfer by len
let transfer_len_x (to_stream : Connection) (from : Stream) len =
  let buf_size = 0x2000
  let buf = Array.zeroCreate<byte> 0x2000
  let rec do_block left = socket {
    let! read = lift_async <| from.AsyncRead(buf, 0, Math.Min(buf_size, left))
    if read <= 0 || left - read = 0 then
      return ()
    else
      do! send to_stream (new ArraySegment<_>(buf,0,read))
      return! do_block (left - read) }
  do_block len
