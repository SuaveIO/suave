namespace Suave.Sockets

open System
open System.Net
open System.IO.Pipelines
open System.Threading.Tasks
open Suave.Sockets.Control
open System.Text
open System.Buffers
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
    utf8Encoder   : Encoder }

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
      // Buffer has data - write it
      let vt = this.transport.write (new Memory<_>(this.lineBuffer,0,this.lineBufferCount))
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
        let maxByteCount = Encoding.UTF8.GetMaxByteCount(str.Length)
        if maxByteCount > this.lineBuffer.Length then
          // Flush current buffer first
          if this.lineBufferCount > 0 then
            let! r1 = this.transport.write (new Memory<_>(this.lineBuffer, 0, this.lineBufferCount))
            match r1 with
            | Ok () -> ()
            | Result.Error _ -> ()
            this.lineBufferCount <- 0

          // Use ArrayPool for large strings
          let tempBuffer = ArrayPool<byte>.Shared.Rent(maxByteCount)
          try
            let mutable charsUsed = 0
            let mutable bytesUsed = 0
            let mutable completed = false
            this.utf8Encoder.Convert(str.ToCharArray(), 0, str.Length, tempBuffer, 0, maxByteCount, true, &charsUsed, &bytesUsed, &completed)
            let! r2 = this.transport.write (new Memory<_>(tempBuffer, 0, bytesUsed))
            match r2 with
            | Ok () -> ()
            | Result.Error _ -> ()
            return ()
          finally
            ArrayPool<byte>.Shared.Return(tempBuffer)

        elif this.lineBufferCount + maxByteCount > this.lineBuffer.Length then
          // Flush buffer and encode into it
          let! r3 = this.transport.write (new Memory<_>(this.lineBuffer, 0, this.lineBufferCount))
          match r3 with
          | Ok () -> ()
          | Result.Error _ -> ()
          let mutable charsUsed = 0
          let mutable bytesUsed = 0
          let mutable completed = false
          this.utf8Encoder.Convert(str.ToCharArray(), 0, str.Length, this.lineBuffer, 0, this.lineBuffer.Length, true, &charsUsed, &bytesUsed, &completed)
          this.lineBufferCount <- 0
          return ()

        else
          // Encode directly into the line buffer
          let mutable charsUsed = 0
          let mutable bytesUsed = 0
          let mutable completed = false
          this.utf8Encoder.Convert(str.ToCharArray(), 0, str.Length, this.lineBuffer, this.lineBufferCount, this.lineBuffer.Length - this.lineBufferCount, true, &charsUsed, &bytesUsed, &completed)
          this.lineBufferCount <- this.lineBufferCount + bytesUsed
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
        Buffer.BlockCopy(b, 0, this.lineBuffer,this.lineBufferCount, b.Length)
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
      utf8Encoder = Encoding.UTF8.GetEncoder() }

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
    task {
      let! r9 = cn.transport.shutdown()
      match r9 with
      | Ok () -> ()
      | Result.Error _ -> ()
    }
