namespace Suave.Sockets

open System
open System.Net.Sockets

type TcpTransport(acceptArgs     : SocketAsyncEventArgs,
                  acceptArgsPool : ConcurrentPool<SocketAsyncEventArgs>,
                  readArgsPool   : ConcurrentPool<SocketAsyncEventArgs>,
                  writeArgsPool  : ConcurrentPool<SocketAsyncEventArgs>) =
  let socket = acceptArgs.AcceptSocket
  let readArgs = readArgsPool.Pop()
  let writeArgs = writeArgsPool.Pop()
  let shutdownSocket _ =
    try
      if socket <> null then
        try
          socket.Shutdown(SocketShutdown.Both)
        with _ ->
          ()

        socket.Dispose ()
    with _ -> ()

  interface ITransport with
    member this.read (buf : ByteSegment) =
      async{
       if acceptArgs.AcceptSocket = null then
         return Choice2Of2 (ConnectionError "read error: acceptArgs.AcceptSocket = null") 
       else
         return! asyncDo acceptArgs.AcceptSocket.ReceiveAsync (setBuffer buf) (fun a -> a.BytesTransferred) readArgs
       }

    member this.write (buf : ByteSegment) =
      async{
        if acceptArgs.AcceptSocket = null then
         return Choice2Of2 (ConnectionError "write error: acceptArgs.AcceptSocket = null") 
       else
         return! asyncDo acceptArgs.AcceptSocket.SendAsync (setBuffer buf) ignore writeArgs
      }

    member this.shutdown() = async {
      shutdownSocket ()
      acceptArgs.AcceptSocket <- null
      acceptArgsPool.Push acceptArgs
      readArgsPool.Push readArgs
      writeArgsPool.Push writeArgs
      return ()
      }