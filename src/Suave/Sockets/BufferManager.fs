namespace Suave.Sockets

open System
open System.Collections.Generic
open System.Collections.Concurrent

open Suave

/// This class creates a single large buffer which can be divided up
/// and assigned to SocketAsyncEventArgs objects for use with each
/// socket I/O operation.
/// This enables bufffers to be easily reused and guards against
/// fragmenting heap memory.
///
/// The operations exposed on the BufferManager class are not thread safe.
[<AllowNullLiteral>]
type BufferManager(totalBytes, bufferSize, logger, autoGrow) =

  do Log.internf logger "Suave.Socket.BufferManager" (fun fmt ->
    fmt "initialising BufferManager with %d bytes" totalBytes)

  /// underlying list of byte arrays maintained by the Buffer Manager
  let segments = new ConcurrentBag<ArraySegment<byte>>()

  /// something to lock on when creating a new buffer
  let creatingSegment = obj()

  let chunksPerSegment = totalBytes/bufferSize

  /// Initialise a segment of memory
  member x.createBuffer() =
    lock creatingSegment (fun _ ->
      if segments.Count < chunksPerSegment/2 then 
        let buffer = Array.zeroCreate totalBytes
        let mutable runningOffset = 0
        while runningOffset < totalBytes - bufferSize do
          segments.Add (ArraySegment(buffer, runningOffset, bufferSize))
          runningOffset <- runningOffset + bufferSize)

  /// Pops a buffer from the buffer pool
  member x.PopBuffer(?context : string) : ArraySegment<byte> =
    let rec loop tries =
      if tries = 0 then
        raise (Exception "Could not adquire a buffer, too many tries.")
      else
        match segments.TryTake() with
        | true, s ->
          Log.internf logger "Suave.Socket.BufferManager" (fun fmt ->
            fmt "reserving buffer: %d, free count: %d [%s]" s.Offset segments.Count (defaultArg context "no-ctx"))
          s
        | false, _ ->
          if autoGrow then x.createBuffer ()
          loop (tries - 1)
    loop 100

  /// Initialise the memory required to use this BufferManager
  member x.Init() = x.createBuffer()

  /// Frees the buffer back to the buffer pool
  member x.FreeBuffer(args : ArraySegment<_>, ?context : string) =
    // Not trivial to check for double frees now
    //if segments. args then failwithf "double free buffer %d" args.Offset
    segments.Add args
    Log.internf logger "Suave.Socket.BufferManager" (fun fmt ->
      fmt "freeing buffer: %d, free count: %d [%s]" args.Offset segments.Count (defaultArg context "no-ctx"))
