namespace Suave.Sockets

open System.Net
open System
open Suave.Utils
open Suave.Utils.Bytes

/// A connection (TCP implied) is a thing that can read and write from a socket
/// and that can be closed.
type Connection =
  { ipaddr         : IPAddress
    transport      : ITransport
    bufferManager  : BufferManager
    lineBuffer     : ArraySegment<byte>
    segments       : BufferSegment list }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Connection =

  let empty : Connection =
    { ipaddr     = IPAddress.Loopback
      transport       =  { socket = null; readArgs = null; writeArgs   = null }
      bufferManager = null
      lineBuffer  =  ArraySegment<byte>()
      segments     = []  }
      
  let inline receive (cn : Connection) (buf : ByteSegment) =
    cn.transport.read buf
  
  let inline send (cn :Connection) (buf : ByteSegment) =
    cn.transport.write buf

  let ipaddr_ =
    (fun x -> x.ipaddr),
    fun v x -> { x with ipaddr = v }

  let transport_ =
    (fun x -> x.transport),
    fun v x -> { x with transport = v }

  let buffer_manager_ =
    (fun x -> x.bufferManager),
    fun v x -> { x with bufferManager = v }

  let line_buffer_ =
    (fun x -> x.lineBuffer),
    fun v x -> { x with lineBuffer = v }

  let segments_ =
    (fun x -> x.segments),
    fun v x -> { x with segments = v }