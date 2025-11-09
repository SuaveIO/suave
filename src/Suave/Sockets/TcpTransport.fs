namespace Suave.Sockets

open System.Net
open System.Net.Sockets
open System.Threading
open Suave
open System

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
          | :? SocketException -> () // Socket may already be closed
          | :? ObjectDisposedException -> () // Socket already disposed
      finally
        try
          acceptSocket.Dispose ()
        with
          | :? ObjectDisposedException -> () // Already disposed, ignore

  let remoteBinding (socket : Socket) : SocketBinding =
    let rep = socket.RemoteEndPoint :?> IPEndPoint
    { ip = rep.Address; port = uint16 rep.Port }

  member this.accept() =
    task{
      let! a = listenSocket.AcceptAsync(cancellationToken)
      this.acceptSocket <- a
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
      task {
        try
          let! bytesRead = this.readInternal buf
          return Ok bytesRead
        with
        | :? SocketException as ex ->
          return Result.Error(Error.SocketError(ex.SocketErrorCode))
        | ex ->
          return Result.Error(Error.ConnectionError(ex.Message))
      }

    member this.write (buf : Memory<byte>) : SocketOp<unit> =
      task {
        try
          let! _ = this.writeInternal buf
          return Ok()
        with
        | :? SocketException as ex ->
          return Result.Error(Error.SocketError(ex.SocketErrorCode))
        | ex ->
          return Result.Error(Error.ConnectionError(ex.Message))
      }

    member this.shutdown() : SocketOp<unit> =
      task {
        this.shutdown()
        return Ok()
      }
