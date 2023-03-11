namespace Suave.Sockets

open System.Net
open System.Net.Sockets
open System.Threading

[<AllowNullLiteral>]
type TcpTransport(listenSocket : Socket, cancellationToken:CancellationToken) =

  let mutable acceptSocket :  Socket = null

  let shutdownSocket _ =
    try
      if acceptSocket <> null then
        try
          acceptSocket.Shutdown(SocketShutdown.Both)
        with _ ->
        acceptSocket.Dispose ()
    with _ -> ()

  let remoteBinding (socket : Socket) =
    let rep = socket.RemoteEndPoint :?> IPEndPoint
    { ip = rep.Address; port = uint16 rep.Port }

  member this.accept() =
    task{
      let! a = listenSocket.AcceptAsync(cancellationToken)
      acceptSocket <- a
      return Ok(remoteBinding a)
    }

  member this.read (buf : ByteSegment) =
    task{
      let! result = acceptSocket.ReceiveAsync(buf,cancellationToken)
      return Ok(result)
      }

  member this.write (buf : ByteSegment) =
    task{
      let! result = acceptSocket.SendAsync(buf,cancellationToken)
      return Ok()
    }

  member this.shutdown() =
      shutdownSocket ()
      acceptSocket <- null
