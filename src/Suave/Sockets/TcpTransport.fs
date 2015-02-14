namespace Suave.Sockets

open System
open System.Net.Sockets

type TcpTransport =
  { socket      : Socket
    readArgs    : SocketAsyncEventArgs
    writeArgs   : SocketAsyncEventArgs
  }
  interface ITransport with
    member this.read(buf : ByteSegment) =
      asyncDo this.socket.ReceiveAsync (setBuffer buf)  (fun a -> a.BytesTransferred) this.readArgs
    member this.write(buf : ByteSegment) =
      asyncDo this.socket.SendAsync (setBuffer buf) ignore this.writeArgs