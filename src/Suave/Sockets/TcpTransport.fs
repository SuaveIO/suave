namespace Suave.Sockets

open System
open System.Net.Sockets

type TcpTransport(acceptArgs: SocketAsyncEventArgs, a : ConcurrentPool<SocketAsyncEventArgs>,b : ConcurrentPool<SocketAsyncEventArgs>,c : ConcurrentPool<SocketAsyncEventArgs>) =
  let socket = acceptArgs.AcceptSocket
  let readArgs = b.Pop()
  let writeArgs = c.Pop()
  let shutdownSocket _ =
    try
      if socket <> null then
        try
          socket.Shutdown(SocketShutdown.Both)
        with _ -> ()
        socket.Close ()
        socket.Dispose ()
    with _ -> ()
  interface ITransport with
    member this.read(buf : ByteSegment) =
      asyncDo socket.ReceiveAsync (setBuffer buf) (fun a -> a.BytesTransferred) readArgs
    member this.write(buf : ByteSegment) =
      asyncDo socket.SendAsync (setBuffer buf) ignore writeArgs
    member this.shutdown() = async {
      shutdownSocket ()
      acceptArgs.AcceptSocket <- null
      a.Push acceptArgs
      b.Push readArgs
      c.Push writeArgs
      return ()
      }