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
                           * healthCheckEnabled : bool
                           * healthCheckIntervalMs : int
                           * maxConnectionAgeSeconds : int
                           * webPart : WebPart
                          -> TcpServer

    /// Overload that allows the caller to request more than one accept loop
    /// per binding (kernel SO_REUSEPORT load-balancing). Implementations may
    /// silently fall back to a single acceptor on platforms without
    /// SO_REUSEPORT support.
    abstract member create : maxOps : int
                           * bufferSize : int
                           * binding : SocketBinding
                           * runtime : HttpRuntime
                           * cancellationToken : CancellationToken
                           * healthCheckEnabled : bool
                           * healthCheckIntervalMs : int
                           * maxConnectionAgeSeconds : int
                           * acceptorCount : int
                           * webPart : WebPart
                          -> TcpServer

  type DefaultTcpServerFactory() =
    interface TcpServerFactory with
      member this.create (maxOps, bufferSize, binding, runtime, cancellationToken, healthCheckEnabled, healthCheckIntervalMs, maxConnectionAgeSeconds, webPart) =
        Tcp.runServer maxOps bufferSize binding runtime cancellationToken webPart healthCheckEnabled healthCheckIntervalMs maxConnectionAgeSeconds
      member this.create (maxOps, bufferSize, binding, runtime, cancellationToken, healthCheckEnabled, healthCheckIntervalMs, maxConnectionAgeSeconds, acceptorCount, webPart) =
        Tcp.runServerEx acceptorCount maxOps bufferSize binding runtime cancellationToken webPart healthCheckEnabled healthCheckIntervalMs maxConnectionAgeSeconds

  let tcpServerFactory = new DefaultTcpServerFactory()

