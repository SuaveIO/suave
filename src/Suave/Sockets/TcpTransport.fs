namespace Suave.Sockets

open System.Net
open System.Net.Sockets
open System.Threading
open Suave

[<AllowNullLiteral>]
type TcpTransport(listenSocket : Socket, cancellationToken:CancellationToken) =

  [<DefaultValue>]
  val mutable acceptSocket :  Socket

  let shutdownSocket (acceptSocket:Socket) =
    try
      if acceptSocket <> null then
        try
          acceptSocket.Shutdown(SocketShutdown.Both)
        with _ ->
        acceptSocket.Dispose ()
    with _ -> ()

  let remoteBinding (socket : Socket) : SocketBinding =
    let rep = socket.RemoteEndPoint :?> IPEndPoint
    { ip = rep.Address; port = uint16 rep.Port }

  member this.accept() =
    task{
      let! a = listenSocket.AcceptAsync(cancellationToken)
      this.acceptSocket <- a
      return Ok(remoteBinding a)
    }

  member this.read (buf : ByteSegment) =
    task{
      let! result = this.acceptSocket.ReceiveAsync(buf,cancellationToken)
      return Ok(result)
      }

  member this.write (buf : ByteSegment) =
    task{
      let! result = this.acceptSocket.SendAsync(buf,cancellationToken)
      return Ok()
    }

  member this.shutdown() =
      shutdownSocket (this.acceptSocket)
      this.acceptSocket <- null
