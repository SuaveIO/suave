namespace Suave

open Suave.Logging
open Suave.Sockets
open Suave.Tcp

type TcpServerFactory =
  /// This is the factory's factory method. It's almost like OOP again!
  abstract member create : logger:Logger * maxOps:int * bufferSize:int * binding:SocketBinding
                        -> TcpServer

type DefaultTcpServerFactory() =
  interface TcpServerFactory with
    member this.create (logger, maxOps, bufferSize, binding) =
      Tcp.runServer logger maxOps bufferSize binding