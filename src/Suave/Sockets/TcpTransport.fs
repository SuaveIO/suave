namespace Suave.Sockets

open System.Net
open System.Net.Sockets

[<AllowNullLiteral>]
type TcpTransport(acceptArgs     : SocketAsyncEventArgs,
                  readArgs     : SocketAsyncEventArgs,
                  writeArgs     : SocketAsyncEventArgs,
                  listenSocket : Socket) =

  let shutdownSocket _ =
    try
      if acceptArgs.AcceptSocket <> null then
        try
          acceptArgs.AcceptSocket.Shutdown(SocketShutdown.Both)
        with _ ->
          ()

        acceptArgs.AcceptSocket.Dispose ()
    with _ -> ()

  let remoteBinding (socket : Socket) =
    let rep = socket.RemoteEndPoint :?> IPEndPoint
    { ip = rep.Address; port = uint16 rep.Port }

  member this.accept() =
      asyncDo listenSocket.AcceptAsync ignore (fun a -> remoteBinding a.AcceptSocket) acceptArgs

  interface ITransport with
    member this.read (buf : ByteSegment) =
      async{
        if acceptArgs.AcceptSocket = null then
          return Result.Error (ConnectionError "read error: acceptArgs.AcceptSocket = null") 
        else
          return! asyncDo acceptArgs.AcceptSocket.ReceiveAsync (setBuffer buf) (fun a -> a.BytesTransferred) readArgs
        }

    member this.write (buf : ByteSegment) =
      async{
        if acceptArgs.AcceptSocket = null then
          return Result.Error (ConnectionError "write error: acceptArgs.AcceptSocket = null") 
        else
          return! asyncDo acceptArgs.AcceptSocket.SendAsync (setBuffer buf) ignore writeArgs
      }

    member this.shutdown() = async {
      shutdownSocket ()
      acceptArgs.AcceptSocket <- null
      return ()
      }