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
      async_do this.socket.ReceiveAsync (set_buffer buf)  (fun a -> a.BytesTransferred) this.read_args
    member this.write(buf : ByteSegment) =
      async_do this.socket.SendAsync (set_buffer buf) ignore this.write_args