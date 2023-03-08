namespace Suave.Sockets

open System.Net
open System
open System.Collections.Generic

open Suave.Utils
open Suave.Utils.Bytes
open System.IO.Pipelines

/// A connection (TCP implied) is a thing that can read and write from a socket
/// and that can be closed.
type Connection =
  { socketBinding : SocketBinding
    transport     : ITransport
    pipe : Pipe
    lineBuffer    : byte array
    lineBufferCount : int }

  member x.ipAddr : IPAddress =
    x.socketBinding.ip

  member x.port : Port =
    x.socketBinding.port

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Connection =

  let empty : Connection =
    { socketBinding = SocketBinding.create IPAddress.IPv6Loopback 8080us
      transport     = null
      pipe = null
      lineBuffer    = [||]
      lineBufferCount = 0 }

  let inline receive (cn : Connection) (buf : ByteSegment) =
    cn.transport.read buf

  let inline send (cn :Connection) (buf : ByteSegment) =
    cn.transport.write buf

  let transport_ =
    (fun x -> x.transport),
    fun v x -> { x with transport = v }

  let pipe =
    (fun x -> x.pipe),
    fun v x -> { x with pipe = v }

  let lineBuffer_ =
    (fun x -> x.lineBuffer),
    fun v x -> { x with lineBuffer = v }

  //let segments_ =
    //(fun x -> x.segments),
    //fun v x -> { x with segments = v }