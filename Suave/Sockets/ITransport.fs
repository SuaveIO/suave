namespace Suave.Sockets

type ITransport =
  abstract member read  : ByteSegment -> SocketOp<int>
  abstract member write : ByteSegment -> SocketOp<unit>