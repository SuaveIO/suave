namespace Suave.Sockets

open System
open System.Net
open Suave.Utils.Collections

/// A port is an unsigned short (uint16) structure
type Port = uint16

type SocketBinding(ip: IPAddress, port : Port) = 

  member x.ip = ip
  member x.port = port
  member x.end_point = new IPEndPoint(ip, int port)

  override x.ToString() =
    let isv6 = (ip.AddressFamily = Sockets.AddressFamily.InterNetworkV6)
    if isv6 then
      String.Concat [ "["; ip.ToString(); "]:"; port.ToString() ]
    else
      String.Concat [ ip.ToString(); ":"; port.ToString() ]

  static member ip_ = Property<SocketBinding,_> (fun x -> x.ip) (fun v x -> SocketBinding(ip=v,port=x.port))
  static member port_ = Property<SocketBinding,_> (fun x -> x.port) (fun v x -> SocketBinding(ip=x.ip, port=x.port))
