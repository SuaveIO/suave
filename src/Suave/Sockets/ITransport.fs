namespace Suave.Sockets

[<AllowNullLiteral>]
type ITransport =
  abstract member read  : ByteSegment -> SocketOp<int>
  abstract member write : ByteSegment -> SocketOp<unit>
  abstract member shutdown : unit -> Async<unit>