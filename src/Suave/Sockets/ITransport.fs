namespace Suave.Sockets

open System

[<AllowNullLiteral>]
type ITransport =
  abstract member read  : Memory<byte> -> SocketOp<int>
  abstract member write : Memory<byte> -> SocketOp<unit>
  /// Flush any data still buffered above the kernel send queue. For
  /// transports that write straight to the socket (`TcpTransport`) this is
  /// a no-op; for buffered transports such as `SslTransport` it awaits the
  /// underlying stream's `FlushAsync` so callers can be sure their bytes
  /// have left the application layer before the connection is torn down
  /// (see `Http2.bestEffortGoAway` for the motivating case).
  abstract member flush : unit -> SocketOp<unit>
  abstract member shutdown : unit -> unit