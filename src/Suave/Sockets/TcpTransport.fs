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

  let shutdownSocket (acceptSocket:Socket) =
    // do not like this if here
    if acceptSocket <> null then
      try
        acceptSocket.Shutdown(SocketShutdown.Both)
        //Console.WriteLine("Socket shutdown smoothly")
      finally
        acceptSocket.Dispose ()

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
    this.acceptSocket.ReceiveAsync(buf,cancellationToken)
    //task{
      //let! result = this.acceptSocket.ReceiveAsync(buf,cancellationToken)
      //return Ok(result)
      //}

  member this.write (buf : ByteSegment) =
    this.acceptSocket.SendAsync(buf,cancellationToken)
    //task{
      //let! result = this.acceptSocket.SendAsync(buf,cancellationToken)
      //return Ok()
    //}

  member this.shutdown() =
      shutdownSocket (this.acceptSocket)
      this.acceptSocket <- null
