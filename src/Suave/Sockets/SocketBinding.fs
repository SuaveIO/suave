namespace Suave.Sockets

open System
open System.Net

/// A port is an unsigned short (uint16) structure
type Port = uint16

module IPAddress =
  let is_ipv6 (x : IPAddress) =
    x.AddressFamily = Sockets.AddressFamily.InterNetworkV6

type SocketBinding =
  { ip   : IPAddress
    port : Port }
  member x.end_point =
    new IPEndPoint(x.ip, int x.port)
  override x.ToString() =
    if IPAddress.is_ipv6 x.ip then
      String.Concat [ "["; x.ip.ToString(); "]:"; x.port.ToString() ]
    else
      String.Concat [ x.ip.ToString(); ":"; x.port.ToString() ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SocketBinding =

  let mk ip port =
    { ip   = ip
      port = port }