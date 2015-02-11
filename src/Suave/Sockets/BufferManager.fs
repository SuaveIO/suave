namespace Suave.Sockets

open System
open System.Collections.Generic

open Suave

/// This class creates a single large buffer which can be divided up
/// and assigned to SocketAsyncEventArgs objects for use with each
/// socket I/O operation.
/// This enables bufffers to be easily reused and guards against
/// fragmenting heap memory.
///
/// The operations exposed on the BufferManager class are not thread safe.
[<AllowNullLiteral>]
type BufferManager(totalBytes, bufferSize, logger) =
  do Log.internf logger "Suave.Socket.BufferManager" (fun fmt ->
    fmt "initialising BufferManager with %d bytes" totalBytes)

  /// the underlying byte array maintained by the Buffer Manager
  let buffer = Array.zeroCreate totalBytes
  let freeOffsets = new Stack<int>()

  /// Pops a buffer from the buffer pool
  member x.PopBuffer(?context : string) : ArraySegment<byte> =
    let offset, free_count = lock freeOffsets (fun _ ->
      freeOffsets.Pop(), freeOffsets.Count)
    Log.internf logger "Suave.Socket.BufferManager" (fun fmt ->
      fmt "reserving buffer: %d, free count: %d [%s]" offset free_count (defaultArg context "no-ctx"))
    ArraySegment(buffer, offset, bufferSize)

  /// Initialise the memory required to use this BufferManager
  member x.Init() =
    lock freeOffsets (fun _ ->
      let mutable runningOffset = 0
      while runningOffset < totalBytes - bufferSize do
        freeOffsets.Push runningOffset
        runningOffset <- runningOffset + bufferSize)

  /// Frees the buffer back to the buffer pool
  member x.FreeBuffer(args : ArraySegment<_>, ?context : string) =
    let freeCount = lock freeOffsets (fun _ ->
      if freeOffsets.Contains args.Offset then failwithf "double free buffer %d" args.Offset
      freeOffsets.Push args.Offset
      freeOffsets.Count)
    Log.internf logger "Suave.Socket.BufferManager" (fun fmt ->
      fmt "freeing buffer: %d, free count: %d [%s]" args.Offset freeCount (defaultArg context "no-ctx"))
