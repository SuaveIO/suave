namespace Suave.Sockets

open System
open System.Net
open System.IO.Pipelines
open System.Threading.Tasks
open Suave.Sockets.Control
open System.Text
open System.Buffers
open System.Runtime.InteropServices
open Suave.Utils
open Suave

/// A connection (TCP implied) is a thing that can read and write from a socket
/// and that can be closed.
type Connection =
  { mutable socketBinding : SocketBinding
    transport     : ITransport
    reader     : HttpReader
    pipe : Pipe
    lineBuffer    : byte array
    mutable lineBufferCount : int
    utf8Encoder   : Encoder
    mutable isLongLived : bool }  // Flag for long-lived protocols (WebSocket, SSE, etc.)

  member x.ipAddr : IPAddress =
    x.socketBinding.ip

  member x.port : Port =
    x.socketBinding.port

  /// Flush out whatever is in the lineBuffer - returns ValueTask for allocation-free synchronous completions
  member inline this.flush () =
    if this.lineBufferCount = 0 then
      // Buffer empty - synchronous completion with zero allocations
      ValueTask.CompletedTask
    else
      // Buffer has data - write it asynchronously
      let vt = this.transport.write (new Memory<byte>(this.lineBuffer, 0, this.lineBufferCount))
      if vt.IsCompletedSuccessfully then
        // Write completed synchronously - clear buffer and return
        Array.Clear(this.lineBuffer, 0, this.lineBufferCount)
        this.lineBufferCount <- 0
        ValueTask.CompletedTask
      else
        // Async path
        ValueTask(task {
            let! writeRes = vt
            match writeRes with
            | Ok () -> ()
            | Result.Error _ -> ()
            // Clear the buffer to prevent information leakage
            Array.Clear(this.lineBuffer, 0, this.lineBufferCount)
            this.lineBufferCount <- 0
        })

  member inline this.asyncWrite (str: string) =
    task {
      if str.Length = 0 then
        return ()
      else
        // Use UTF8.GetBytes directly on string instead of ToCharArray to avoid intermediate allocation
        let strBytes = Encoding.UTF8.GetBytes str
        if strBytes.Length > this.lineBuffer.Length then
          // Flush current buffer first
          if this.lineBufferCount > 0 then
            let! r1 = this.transport.write (new Memory<byte>(this.lineBuffer, 0, this.lineBufferCount))
            match r1 with
            | Ok () -> ()
            | Result.Error _ -> ()
            this.lineBufferCount <- 0
          
          // Write the bytes directly
          let! r2 = this.transport.write (new Memory<byte>(strBytes, 0, strBytes.Length))
          match r2 with
          | Ok () -> ()
          | Result.Error _ -> ()
          return ()

        elif this.lineBufferCount + strBytes.Length > this.lineBuffer.Length then
          // Flush buffer first
          let! r3 = this.transport.write (new Memory<byte>(this.lineBuffer, 0, this.lineBufferCount))
          match r3 with
          | Ok () -> ()
          | Result.Error _ -> ()
          // Copy bytes into line buffer
          Buffer.BlockCopy(strBytes, 0, this.lineBuffer, 0, strBytes.Length)
          this.lineBufferCount <- strBytes.Length
          return ()

        else
          // Copy directly into the line buffer
          Buffer.BlockCopy(strBytes, 0, this.lineBuffer, this.lineBufferCount, strBytes.Length)
          this.lineBufferCount <- this.lineBufferCount + strBytes.Length
          return ()
    }  
    
  member inline this.asyncWriteLn (s : string) = task {
    do! this.asyncWrite s
    do! this.asyncWrite Bytes.eol
  }

  /// Write the string s to the stream asynchronously from a byte array
  member inline this.asyncWriteBytes (b : byte[]) =
    task {
    if b.Length > 0 then
      let! r4 = this.transport.write (new Memory<_>(b, 0, b.Length))
      match r4 with
      | Ok () -> ()
      | Result.Error _ -> ()
  }

  member inline this.asyncWriteBufferedBytes (b : byte[]) =
    task {
      if this.lineBufferCount + b.Length > this.lineBuffer.Length then
        // flush lineBuffer
        if this.lineBufferCount > 0 then
          let! r5 = this.transport.write (new Memory<_>(this.lineBuffer, 0, this.lineBufferCount))
          match r5 with
          | Ok () -> ()
          | Result.Error _ -> ()
        // don't waste time buffering here
        let! r6 = this.transport.write (new Memory<_>(b, 0, b.Length))
        match r6 with
        | Ok () -> ()
        | Result.Error _ -> ()
        this.lineBufferCount <- 0
      else
        Buffer.BlockCopy(b, 0, this.lineBuffer, this.lineBufferCount, b.Length)
        this.lineBufferCount <- this.lineBufferCount + b.Length
    }

  /// Overload to accept Memory<byte> directly, avoiding wrapper allocation
  member inline this.asyncWriteBufferedBytes (b : Memory<byte>) =
    task {
      if this.lineBufferCount + b.Length > this.lineBuffer.Length then
        // flush lineBuffer using Memory wrapper
        if this.lineBufferCount > 0 then
          let! r5 = this.transport.write (new Memory<byte>(this.lineBuffer, 0, this.lineBufferCount))
          match r5 with
          | Ok () -> ()
          | Result.Error _ -> ()
        // don't waste time buffering here
        let! r6 = this.transport.write b
        match r6 with
        | Ok () -> ()
        | Result.Error _ -> ()
        this.lineBufferCount <- 0
      else
        // Copy from Memory<byte> into line buffer using span
        b.Span.CopyTo(new Span<_>(this.lineBuffer, this.lineBufferCount, b.Length))
        this.lineBufferCount <- this.lineBufferCount + b.Length
    }

  /// Overload to accept ReadOnlyMemory<byte> directly, avoiding wrapper allocation
  member inline this.asyncWriteBufferedBytes (b : ReadOnlyMemory<byte>) =
    task {
      if this.lineBufferCount + b.Length > this.lineBuffer.Length then
        // flush lineBuffer using Memory wrapper
        if this.lineBufferCount > 0 then
          let! r5 = this.transport.write (new Memory<byte>(this.lineBuffer, 0, this.lineBufferCount))
          match r5 with
          | Ok () -> ()
          | Result.Error _ -> ()
        // don't waste time buffering here - convert ReadOnlyMemory to Memory once and write
        let memB = MemoryMarshal.AsMemory(b)
        let! r6 = this.transport.write memB
        match r6 with
        | Ok () -> ()
        | Result.Error _ -> ()
        this.lineBufferCount <- 0
      else
        // Copy from ReadOnlyMemory<byte> into line buffer using span
        b.Span.CopyTo(new Span<_>(this.lineBuffer, this.lineBufferCount, b.Length))
        this.lineBufferCount <- this.lineBufferCount + b.Length
    }

  member inline this.asyncWriteBufferedArrayBytes (xxs:(byte[])[])  =
    let rec loop index =
      task{
        if index >= xxs.Length then
          return ()
        else
          do! this.asyncWriteBufferedBytes xxs.[index]
          return! loop (index + 1)
          }
    loop 0

  member inline this.writeChunk (chunk : byte []) = task {
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
      lineBufferCount = 0
      utf8Encoder = Encoding.UTF8.GetEncoder()
      isLongLived = false }

  let inline receive (cn : Connection) (buf : ByteSegment) =
    cn.transport.read buf

  let inline send (cn :Connection) (buf : ByteSegment) =
    task {
      let! r8 = cn.transport.write buf
      match r8 with
      | Ok () -> return Ok ()
      | Result.Error e -> return Result.Error e
    }

  let inline shutdown (cn : Connection) =
    cn.transport.shutdown()
