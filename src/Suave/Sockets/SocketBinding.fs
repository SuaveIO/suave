namespace Suave.Sockets

open System
open System.Net
open System.Net.Sockets
open Suave.Utils

/// A port is an unsigned short (uint16) structure
type Port = uint16

type SocketBinding =
  { ip   : IPAddress
    port : Port }

  member x.endpoint =
    new IPEndPoint(x.ip, int x.port)

  override x.ToString() =
    if x.ip.AddressFamily = AddressFamily.InterNetworkV6 then
      String.Concat [ "["; x.ip.ToString(); "]:"; x.port.ToString() ]
    else
      String.Concat [ x.ip.ToString(); ":"; x.port.ToString() ]

  static member ip_ = Property<SocketBinding,_> (fun x -> x.ip) (fun v x -> { x with ip=v })
  static member port_ = Property<SocketBinding,_> (fun x -> x.port) (fun v x -> { x with port=v })

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SocketBinding =

  let create ip port =
    { ip = ip; port = port }
