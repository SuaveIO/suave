namespace Suave.Sockets

open System.Net
open System.IO.Pipelines
open Suave.Sockets.Control
open System.Text
open System
open Suave.Utils

/// A connection (TCP implied) is a thing that can read and write from a socket
/// and that can be closed.
type Connection =
  { mutable socketBinding : SocketBinding
    transport     : ITransport
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
      return ()
    }

  // what is this used for ?
  member inline this.chunkBoundaries maxChunkBytes (s : string) =
    if maxChunkBytes < 6 then failwith "Cannot split into chunks of smaller than 6 bytes"
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

  member inline this.asyncWrite (str: string) : SocketOp<unit> =
    socket {
      if str.Length = 0 then
        return ()
      else
        let maxByteCount = Encoding.UTF8.GetMaxByteCount(str.Length)
        if maxByteCount > this.lineBuffer.Length then
          do! this.transport.write (new Memory<_>(this.lineBuffer, 0, this.lineBufferCount))
          for offset, charCount in this.chunkBoundaries this.lineBuffer.Length str do
            let byteCount = Encoding.UTF8.GetBytes(str, offset, charCount, this.lineBuffer, 0)
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

  member inline this.asyncWriteLn (s : string)  : SocketOp<unit> =
    socket {
      return! this.asyncWrite (s + Bytes.eol)
    }

  /// Write the string s to the stream asynchronously from a byte array
  member inline this.asyncWriteBytes (b : byte[]) : SocketOp<unit> = async {
    if b.Length > 0 then return! this.transport.write (new Memory<_>(b, 0, b.Length))
    else return Ok ()
  }

  member inline this.asyncWriteBufferedBytes (b : byte[])  : SocketOp<unit> =
    socket {
      if b.Length > 0 then
        if this.lineBufferCount + b.Length > this.lineBuffer.Length then
          // flush lineBuffer
          if this.lineBufferCount > 0 then
            do! this.transport.write (new Memory<_>(this.lineBuffer, 0, this.lineBufferCount))
          // don't waste time buffering here
          do! this.transport.write (new Memory<_>(b, 0, b.Length))
          this.lineBufferCount <- 0
          return ()
        else
          Buffer.BlockCopy(b, 0, this.lineBuffer,this.lineBufferCount, b.Length)
          this.lineBufferCount <- this.lineBufferCount + b.Length
          return ()
      else return ()
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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Connection =

  let empty : Connection =
    { socketBinding = SocketBinding.create IPAddress.IPv6Loopback 8080us
      transport     = null
      pipe = null
      lineBuffer    = [||]
      lineBufferCount = 0 }

  let inline receive (cn : Connection) (buf : ByteSegment) =
    cn.transport.read buf

  let inline send (cn :Connection) (buf : ByteSegment) =
    cn.transport.write buf

  let transport_ =
    (fun x -> x.transport),
    fun v x -> { x with transport = v }

  let pipe =
    (fun x -> x.pipe),
    fun v x -> { x with pipe = v }

  let lineBuffer_ =
    (fun x -> x.lineBuffer),
    fun v x -> { x with lineBuffer = v }
