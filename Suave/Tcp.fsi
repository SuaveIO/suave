namespace Suave
  module Tcp = begin
    val log_str : (string -> unit)
    val MAX_BACK_LOG : int
    [<Class>]
    type TcpListener =
      member AsyncAcceptTcpClient : unit -> Async<System.Net.Sockets.TcpClient>
    type TcpWorker<'a> = System.Net.Sockets.TcpClient -> Async<'a>
    val close : d:System.Net.Sockets.TcpClient -> unit
    val stop_tcp : server:System.Net.Sockets.TcpListener -> unit
    val tcp_ip_server :
      sourceip:System.Net.IPAddress * sourceport:uint16 ->
        serve_client:TcpWorker<unit> -> Async<unit>
    val stream :
      client:System.Net.Sockets.TcpClient -> System.Net.Sockets.NetworkStream
    val mirror :
      clientStream:System.IO.Stream ->
        serverStream:System.IO.Stream -> Async<unit>
  end

