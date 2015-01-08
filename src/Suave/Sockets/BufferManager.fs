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
type BufferManager(total_bytes, buffer_size, logger) =
  do Log.internf logger "Suave.Socket.BufferManager" (fun fmt ->
    fmt "initialising BufferManager with %d bytes" total_bytes)

  /// the underlying byte array maintained by the Buffer Manager
  let buffer = Array.zeroCreate total_bytes
  let free_offsets = new Stack<int>()

  /// Pops a buffer from the buffer pool
  member x.PopBuffer(?context : string) : ArraySegment<byte> =
    let offset, free_count = lock free_offsets (fun _ ->
      free_offsets.Pop(), free_offsets.Count)
    Log.internf logger "Suave.Socket.BufferManager" (fun fmt ->
      fmt "reserving buffer: %d, free count: %d [%s]" offset free_count (defaultArg context "no-ctx"))
    ArraySegment(buffer, offset, buffer_size)

  /// Initialise the memory required to use this BufferManager
  member x.Init() =
    lock free_offsets (fun _ ->
      let mutable running_offset = 0
      while running_offset < total_bytes - buffer_size do
        free_offsets.Push running_offset
        running_offset <- running_offset + buffer_size)

  /// Frees the buffer back to the buffer pool
  member x.FreeBuffer(args : ArraySegment<_>, ?context : string) =
    let free_count = lock free_offsets (fun _ ->
      if free_offsets.Contains args.Offset then failwithf "double free buffer %d" args.Offset
      free_offsets.Push args.Offset
      free_offsets.Count)
    Log.internf logger "Suave.Socket.BufferManager" (fun fmt ->
      fmt "freeing buffer: %d, free count: %d [%s]" args.Offset free_count (defaultArg context "no-ctx"))
