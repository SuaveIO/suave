namespace Suave.Sockets

open System
open System.Collections.Generic
open System.Collections.Concurrent
open Suave
open Suave.Logging

/// This class creates a single large buffer which can be divided up
/// and assigned to SocketAsyncEventArgs objects for use with each
/// socket I/O operation.
/// This enables bufffers to be easily reused and guards against
/// fragmenting heap memory.
///
/// The operations exposed on the BufferManager class are not thread safe.
[<AllowNullLiteral>]
type BufferManager(totalBytes, bufferSize, autoGrow) =
  static let logger = Log.create "Suave.Sockets.BufferManager"

  do if bufferSize < 6 then failwith "bufferSize must be greater than six bytes"

  do logger.debug (
       Message.eventX "Initialising BufferManager with {totalBytes}"
       >> Message.setFieldValue "totalBytes" totalBytes)

  /// underlying list of byte arrays maintained by the Buffer Manager
  let segments = new ConcurrentBag<ArraySegment<byte>>()

  /// something to lock on when creating a new buffer
  let creatingSegment = obj()

  let chunksPerSegment = totalBytes / bufferSize

  /// Initialise a segment of memory
  member x.createBuffer() =
    lock creatingSegment (fun _ ->
      if segments.Count < chunksPerSegment / 2 then
        logger.verbose (
          Message.eventX "Creating buffer bank, total {totalBytes} bytes"
          >> Message.setFieldValue "totalBytes" totalBytes)
        let buffer = Array.zeroCreate totalBytes
        let mutable runningOffset = 0
        while runningOffset < totalBytes - bufferSize do
          segments.Add (ArraySegment(buffer, runningOffset, bufferSize))
          runningOffset <- runningOffset + bufferSize)

  /// Pops a buffer from the buffer pool
  member x.PopBuffer(?caller : string) : ArraySegment<byte> =
    let caller = defaultArg caller "no-caller-specified"

    let rec loop tries =
      if tries = 0 then
        let msg = "Could not adquire a buffer, too many retries."
        logger.log Error (Message.eventX msg >> Message.setFieldValue "segmentCount" segments.Count)
        raise (Exception msg)
      else
        match segments.TryTake() with
        | true, segment ->
          logger.verbose (
            Message.eventX "Reserving buffer at {offset}, with {freeCount} segments from call from {caller}"
            >> Message.setFieldValue "offset" segment.Offset
            >> Message.setFieldValue "freeCount" segments.Count
            >> Message.setFieldValue "caller" caller)
          segment

        | false, _ ->
          logger.verbose (Message.eventX "Ran out of buffers")
          if autoGrow then x.createBuffer ()
          loop (tries - 1)

    loop 100

  /// Initialise the memory required to use this BufferManager.
  member x.Init() =
    x.createBuffer()

  /// Frees the buffer back to the buffer pool.
  member x.FreeBuffer(args : ArraySegment<_>, ?caller : string) =
    // Prevent information leaking
    Array.Clear(args.Array,args.Offset,args.Count)
    segments.Add args
    logger.verbose (
      Message.eventX "Freeing buffer at {offset} from {caller}. Free segments {segmentCount}."
      >> Message.setFieldValue "offset" args.Offset
      >> Message.setFieldValue "caller" (defaultArg caller "no-caller-specified")
      >> Message.setFieldValue "segmentCount" segments.Count)
