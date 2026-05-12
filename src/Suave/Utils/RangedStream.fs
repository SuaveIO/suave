namespace Suave.Utils

open System.IO

/// An implementation of Stream that returns only a subset of
/// the inner Stream
type RangedStream(stream:Stream, start, limit, ?disposeInner) =
  inherit Stream()

  do stream.Position <- start

  let endPosition =
    match limit with
    | Some limit -> min stream.Length (stream.Position + limit)
    | None -> stream.Length

  let maxRead count =
    let maxCount = endPosition - stream.Position
    if count > maxCount then maxCount else count

  override __.CanRead = 
    stream.CanRead

  override __.CanSeek =
    stream.CanSeek

  override __.CanWrite =
    false

  override __.Flush() =
    stream.Flush()

  override __.Length
    with get () = int64 (endPosition - start)

  override __.Position
    with get () = stream.Position 
    and set value = stream.Position <- value

  override __.Seek(offset, origin) = stream.Seek(offset, origin)

  override __.SetLength(value) =
    failwith "Stream doesn't support truncating"

  override __.Read(buffer, offset, count) = 
    stream.Read(buffer, offset, int <| maxRead (int64 count))

  override __.Write(buffer, offset, count) = 
    failwith "Stream is read-only"

  override __.CanTimeout =
    stream.CanTimeout

  override __.ReadTimeout 
    with get () = stream.ReadTimeout 
    and set value = stream.ReadTimeout <- value

  override __.WriteTimeout 
    with get () = failwith "Stream is read-only" 
    and set value = failwith "Stream is read-only"

  override __.ReadAsync(buffer, offset, count, token) =
    stream.ReadAsync(buffer, offset, int <| maxRead (int64 count), token)

  override __.Dispose(disposing) =
    disposeInner
    |> Option.iter (fun streamOwner -> if disposing && streamOwner then stream.Dispose())
