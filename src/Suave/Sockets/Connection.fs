﻿namespace Suave.Sockets

open System.Net
open System
open Suave.Utils
open Suave.Utils.Bytes

/// A connection (TCP implied) is a thing that can read and write from a socket
/// and that can be closed.
type Connection =
  { socketBinding : SocketBinding
    transport     : ITransport
    bufferManager : BufferManager
    lineBuffer    : ArraySegment<byte>
    segments      : BufferSegment list }

  member x.ipAddr : IPAddress =
    x.socketBinding.ip

  member x.port : Port =
    x.socketBinding.port

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Connection =

  let empty : Connection =
    { socketBinding = SocketBinding.mk IPAddress.IPv6Loopback 8083us
      transport     = null
      bufferManager = null
      lineBuffer    = ArraySegment<byte>()
      segments      = [] }

  let inline receive (cn : Connection) (buf : ByteSegment) =
    cn.transport.read buf
  
  let inline send (cn :Connection) (buf : ByteSegment) =
    cn.transport.write buf

  let transport_ =
    (fun x -> x.transport),
    fun v x -> { x with transport = v }

  let bufferManager_ =
    (fun x -> x.bufferManager),
    fun v x -> { x with bufferManager = v }

  let lineBuffer_ =
    (fun x -> x.lineBuffer),
    fun v x -> { x with lineBuffer = v }

  let segments_ =
    (fun x -> x.segments),
    fun v x -> { x with segments = v }