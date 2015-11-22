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

/// A TCP Worker is a thing that takes a TCP client and returns an asynchronous
/// workflow thereof.
type TcpWorker<'a> = Connection -> Async<'a>

/// Write the string s to the stream asynchronously as ASCII encoded text
let asyncWrite (connection : Connection) (s : string) : SocketOp<unit> = 
  async {
    if s.Length > 0 then
      let buff = connection.lineBuffer
      let c = ASCII.bytesToBuffer s buff.Array buff.Offset
      return! send connection (new ArraySegment<_>(buff.Array, buff.Offset, c))
    else return Choice1Of2 ()
  }

let asyncWriteNewLine (connection : Connection) = 
  send connection Bytes.eolArraySegment

let asyncWriteLn (connection : Connection) (s : string) : SocketOp<unit> = 
  socket {
    do! asyncWrite connection (s + Bytes.eol)
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
