namespace Suave

open Suave.Sockets
open Suave.Tcp

module TcpServerFactory =

  type TcpServerFactory =
    /// This is the factory's factory method. It's almost like OOP again!
    abstract member create : maxOps : int
                           * bufferSize : int
                           * autoGrow : bool
                           * binding : SocketBinding
                           * runtime : HttpRuntime
                          -> TcpServer

  type DefaultTcpServerFactory() =
    interface TcpServerFactory with
      member this.create (maxOps, bufferSize, autoGrow, binding, runtime) =
        Tcp.runServer maxOps binding runtime

  let tcpServerFactory = new DefaultTcpServerFactory()

