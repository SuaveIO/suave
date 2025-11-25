namespace Suave.Sockets

open System
open System.Net
open System.Net.Sockets
open System.Threading
open System.Threading.Tasks
open Suave

[<AllowNullLiteral>]
type TcpTransport(listenSocket : Socket, cancellationToken:CancellationToken) =

  [<DefaultValue>]
  val mutable acceptSocket :  Socket
  
  let socketLock = obj()

  let shutdownSocket (acceptSocket:Socket) =
    if acceptSocket <> null then
      try
        try
          acceptSocket.Shutdown(SocketShutdown.Both)
        with
          _ -> () // Socket may already be closed
      finally
        try
          acceptSocket.Dispose ()
        with
          _ -> () // Already disposed, ignore

  let remoteBinding (socket : Socket) : SocketBinding =
    let rep = socket.RemoteEndPoint :?> IPEndPoint
    { ip = rep.Address; port = uint16 rep.Port }

  member this.accept() =
    task{
      let! a = listenSocket.AcceptAsync(cancellationToken)
      this.acceptSocket <- a
      // Set socket receive timeout at TCP level (60 seconds = 60000 ms)
      // This ensures that if no data arrives within this period, the read will timeout
      a.ReceiveTimeout <- 60000
      a.SendTimeout <- 60000  // Also set send timeout for symmetry
      return Ok(remoteBinding a)
    }

  member this.readInternal (buf : ByteSegment) =
    let socket = lock socketLock (fun () -> this.acceptSocket)
    if socket = null then
      raise (ObjectDisposedException("Socket has been disposed"))
    socket.ReceiveAsync(buf,cancellationToken)

  member this.writeInternal (buf : ByteSegment) =
    let socket = lock socketLock (fun () -> this.acceptSocket)
    if socket = null then
      raise (ObjectDisposedException("Socket has been disposed"))
    socket.SendAsync(buf,cancellationToken)

  member this.shutdown() =
    lock socketLock (fun () ->
      let socket = this.acceptSocket
      if socket <> null then
        this.acceptSocket <- null
        shutdownSocket socket
    )

  interface ITransport with
    member this.read (buf : Memory<byte>) : SocketOp<int> =
      // Use ValueTask directly for zero-allocation when synchronous
      let vt = this.readInternal buf
      if vt.IsCompletedSuccessfully then
        // Synchronous completion - zero allocations!
        ValueTask<Result<int,Error>>(Ok vt.Result)
      else
        // Async path - wrap in task
        ValueTask<Result<int,Error>>(
          task {
            try
              let! bytesRead = vt
              return Ok bytesRead
            with
            | :? SocketException as ex ->
              return Result.Error(Error.SocketError(ex.SocketErrorCode))
            | ex ->
              return Result.Error(Error.ConnectionError(ex.Message))
          })

    member this.write (buf : Memory<byte>) : SocketOp<unit> =
      // Use ValueTask directly for zero-allocation when synchronous
      let vt = this.writeInternal buf
      if vt.IsCompletedSuccessfully then
        // Synchronous completion - zero allocations!
        ValueTask<Result<unit,Error>>(Ok())
      else
        // Async path - wrap in task
        ValueTask<Result<unit,Error>>(
          task {
            try
              let! _ = vt
              return Ok()
            with
            | :? SocketException as ex ->
              return Result.Error(Error.SocketError(ex.SocketErrorCode))
            | ex ->
              return Result.Error(Error.ConnectionError(ex.Message))
          })

    member this.shutdown()  =
      this.shutdown()
