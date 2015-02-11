namespace Suave.Sockets

open System
open System.Net.Sockets

type TcpTransport =
  { socket       : Socket
    read_args    : SocketAsyncEventArgs
    write_args   : SocketAsyncEventArgs
  }
  interface ITransport with
    member this.read(buf : ByteSegment) =
      asyncDo this.socket.ReceiveAsync (setBuffer buf)  (fun a -> a.BytesTransferred) this.read_args
    member this.write(buf : ByteSegment) =
      asyncDo this.socket.SendAsync (setBuffer buf) ignore this.write_args