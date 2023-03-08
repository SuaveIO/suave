namespace Suave.Sockets

open System

[<AllowNullLiteral>]
type ITransport =
  abstract member read  : Memory<byte> -> SocketOp<int>
  abstract member write : Memory<byte> -> SocketOp<unit>
  abstract member shutdown : unit -> Async<unit>