namespace Suave.Utils

#nowarn "864"

open System
open System.IO

/// An implementation of an "open" stream in that calls to Dispose and Close
/// don't close the delegatee stream, taken as a parameter in the c'tor.
type internal OpenStream(stream : MemoryStream) =
  inherit Stream()

  member internal x.stream = stream

  member x.Dispose() =
    ()

  interface IDisposable with
    member x.Dispose() =
      ()

  member x.RealDispose() =
    stream.Dispose()

  #if !NETSTANDARD1_5

  override x.Close() =
    ()

  override x.BeginRead(buffer, offset, count, callback, state) =
    stream.BeginRead(buffer, offset, count, callback, state)

  override x.EndRead ar =
    stream.EndRead ar

  override x.BeginWrite(buffer, offset, count, callback, state) =
    stream.BeginWrite(buffer, offset, count, callback, state)

  override x.EndWrite ar =
    stream.EndWrite ar

  #endif

  override x.CanRead =
    stream.CanRead

  override x.CanSeek =
    stream.CanSeek

  override x.CanTimeout =
    stream.CanTimeout

  override x.CanWrite =
    stream.CanWrite

  override x.CopyToAsync(other, bufferSize, ct) =
    stream.CopyToAsync(other, bufferSize, ct)

  override x.Flush() =
    stream.Flush()

  override x.FlushAsync ct =
    stream.FlushAsync(ct)

  override x.Read(buffer, offset, size) =
    stream.Read(buffer, offset, size)

  override x.ReadAsync(buffer, offset, size, ct) =
    stream.ReadAsync(buffer, offset, size, ct)

  override x.ReadByte() =
    stream.ReadByte()

  override x.ReadTimeout
    with get() = stream.ReadTimeout
    and set t = stream.ReadTimeout <- t

  override x.Seek(offset, dir) =
    stream.Seek(offset, dir)

  override x.SetLength l =
    stream.SetLength l

  override x.Length =
    stream.Length

  override x.Position
    with get() = stream.Position
    and set p = stream.Position <- p

  override x.Write(buffer, offset, size) =
    stream.Write(buffer, offset, size)

  override x.WriteAsync(buffer, offset, size, ct) =
    stream.WriteAsync(buffer, offset, size, ct)

  override x.WriteByte b =
    stream.WriteByte b

  override x.WriteTimeout
    with get() = stream.WriteTimeout
    and set t = stream.WriteTimeout <- t

type internal OpenMemoryStream() =
  inherit OpenStream(new MemoryStream())

  member x.ToArray() =
    base.stream.ToArray()
  
  #if !NETSTANDARD1_5
  member x.GetBuffer() =
    base.stream.GetBuffer()
  #endif

  member x.WriteTo(other : Stream) =
    base.stream.WriteTo other