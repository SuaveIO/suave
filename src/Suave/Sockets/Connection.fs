namespace Suave.Sockets

open System.Net
open System.IO.Pipelines
open Suave.Sockets.Control
open System.Text
open System
open Suave.Utils
open Suave

/// A connection (TCP implied) is a thing that can read and write from a socket
/// and that can be closed.
type Connection =
  { mutable socketBinding : SocketBinding
    transport     : TcpTransport
    reader     : HttpReader
    pipe : Pipe
    lineBuffer    : byte array
    mutable lineBufferCount : int }

  member x.ipAddr : IPAddress =
    x.socketBinding.ip

  member x.port : Port =
    x.socketBinding.port

  /// Flush out whatever is in the lineBuffer
  member inline this.flush () : SocketOp<unit> =
    socket {
      if this.lineBufferCount> 0  then
        do! this.transport.write (new Memory<_>(this.lineBuffer,0,this.lineBufferCount))
        this.lineBufferCount <- 0
    }

  member inline this.asyncWrite (str: string) : SocketOp<unit> =
    socket {
      if str.Length = 0 then
        return ()
      else
        let maxByteCount = Encoding.UTF8.GetMaxByteCount(str.Length)
        if maxByteCount > this.lineBuffer.Length then
          do! this.transport.write (new Memory<_>(this.lineBuffer, 0, this.lineBufferCount))
          let byteCount = Encoding.UTF8.GetBytes(str, 0, str.Length, this.lineBuffer, 0)
          // don't waste time buffering here
          do! this.transport.write (new Memory<_>(this.lineBuffer, 0, byteCount))
          this.lineBufferCount <- 0
          return ()

        elif this.lineBufferCount + maxByteCount > this.lineBuffer.Length then
          do! this.transport.write (new Memory<_>(this.lineBuffer, 0, this.lineBufferCount))
          // the string, char index, char count, bytes, byte index
          let c = Encoding.UTF8.GetBytes(str, 0, str.Length, this.lineBuffer, 0)
          this.lineBufferCount <- 0
          return ()

        else
          let c = Encoding.UTF8.GetBytes(str, 0, str.Length, this.lineBuffer, this.lineBufferCount )
          this.lineBufferCount <- this.lineBufferCount + c
          return ()
    }

  member inline this.asyncWriteLn (s : string) : SocketOp<unit> =
    socket {
      return! this.asyncWrite (s + Bytes.eol)
    }

  /// Write the string s to the stream asynchronously from a byte array
  member inline this.asyncWriteBytes (b : byte[]) : SocketOp<unit> =
    task {
    if b.Length > 0 then
      return! this.transport.write (new Memory<_>(b, 0, b.Length))
    else
      return Ok ()
  }

  member inline this.asyncWriteBufferedBytes (b : byte[])  : SocketOp<unit> =
    socket {
      if this.lineBufferCount + b.Length > this.lineBuffer.Length then
        // flush lineBuffer
        if this.lineBufferCount > 0 then
          do! this.transport.write (new Memory<_>(this.lineBuffer, 0, this.lineBufferCount))
        // don't waste time buffering here
        do! this.transport.write (new Memory<_>(b, 0, b.Length))
        this.lineBufferCount <- 0
      else
        Buffer.BlockCopy(b, 0, this.lineBuffer,this.lineBufferCount, b.Length)
        this.lineBufferCount <- this.lineBufferCount + b.Length
    }

  member inline this.asyncWriteBufferedArrayBytes (xxs:(byte[])[]) : SocketOp<unit> =
    let rec loop index =
      socket{
        if index >= xxs.Length then
          return ()
        else
          do! this.asyncWriteBufferedBytes xxs.[index]
          return! loop (index + 1)
          }
    loop 0

  member inline this.writeChunk (chunk : byte []) = socket {
    let chunkLength = chunk.Length.ToString("X")
    do! this.asyncWriteLn chunkLength
    do! this.asyncWriteLn (System.Text.Encoding.UTF8.GetString(chunk))
    do! this.flush()
    ()
  }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Connection =

  let empty : Connection =
    { socketBinding = SocketBinding.create IPAddress.IPv6Loopback 8080us
      transport     = null
      reader     = null
      pipe = null
      lineBuffer    = [||]
      lineBufferCount = 0 }

  let inline receive (cn : Connection) (buf : ByteSegment) =
    cn.transport.read buf

  let inline send (cn :Connection) (buf : ByteSegment) =
    cn.transport.write buf

