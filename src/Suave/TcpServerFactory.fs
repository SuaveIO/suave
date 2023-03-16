namespace Suave

open Suave.Sockets
open Suave.Tcp
open System.Threading

module TcpServerFactory =

  type TcpServerFactory =
    /// This is the factory's factory method. It's almost like OOP again!
    abstract member create : maxOps : int
                           * bufferSize : int
                           * binding : SocketBinding
                           * runtime : HttpRuntime
                           * cancellationToken : CancellationToken
                           * webPart : WebPart
                          -> TcpServer

  type DefaultTcpServerFactory() =
    interface TcpServerFactory with
      member this.create (maxOps, bufferSize, binding, runtime,cancellationToken,webPart) =
        Tcp.runServer maxOps bufferSize binding runtime cancellationToken webPart

  let tcpServerFactory = new DefaultTcpServerFactory()

