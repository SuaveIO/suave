namespace Suave.Sockets

open System
open System.Net
open System.Net.Sockets
open System.Threading
open System.Threading.Tasks
open Suave

[<AllowNullLiteral>]
type TcpTransport(listenSocket : Socket, cancellationToken:CancellationToken) =

  [<DefaultValue>]
  val mutable acceptSocket :  Socket
  
  let socketLock = obj()

  let shutdownSocket (acceptSocket:Socket) =
    if acceptSocket <> null then
      try
        try
          // Graceful close (RFC 793 / Stevens, UNIX Network Programming):
          //   1. Shutdown(Send) sends a FIN once the send buffer has drained,
          //      so any final bytes we just wrote (e.g. an HTTP/2 GOAWAY
          //      written by `bestEffortGoAway` before connection teardown)
          //      reach the peer reliably.
          //   2. Non-blocking drain of whatever the peer left in the receive
          //      buffer. If `Dispose` runs while there is still unread data
          //      on the socket, Linux emits TCP RST on `close(2)` and the
          //      peer observes "unexpected EOF" — which is exactly what
          //      h2spec http2/3.5/2 reported when we wrote a GOAWAY after
          //      reading only the first 24 of 30 invalid-preface bytes.
          //      Using `Socket.Available` keeps this O(buffered bytes) and
          //      adds zero latency on the common case (empty recv buffer).
          //   3. Shutdown(Receive) + Dispose then closes the socket
          //      cleanly. Any failure in step 1 or 2 falls through to the
          //      original `Shutdown(Both)` / `Dispose` path below.
          try acceptSocket.Shutdown(SocketShutdown.Send) with _ -> ()
          let buf : byte[] = Array.zeroCreate 1024
          let mutable draining = acceptSocket.Available > 0
          while draining do
            try
              let n = acceptSocket.Receive(buf, 0, min buf.Length acceptSocket.Available, SocketFlags.None)
              draining <- n > 0 && acceptSocket.Available > 0
            with _ -> draining <- false
          try acceptSocket.Shutdown(SocketShutdown.Receive) with _ -> ()
        with
          _ -> // Fallback to the unconditional Shutdown(Both) path on any
               // unexpected failure of the graceful sequence above.
            try acceptSocket.Shutdown(SocketShutdown.Both) with _ -> ()
      finally
        try
          acceptSocket.Dispose ()
        with
          _ -> () // Already disposed, ignore

  let remoteBinding (socket : Socket) : SocketBinding =
    let rep = socket.RemoteEndPoint :?> IPEndPoint
    { ip = rep.Address; port = uint16 rep.Port }

  member this.accept() =
    task{
      let! a = listenSocket.AcceptAsync(cancellationToken)
      this.acceptSocket <- a
      // Set socket receive timeout at TCP level (60 seconds = 60000 ms)
      // This ensures that if no data arrives within this period, the read will timeout
      a.ReceiveTimeout <- 60000
      a.SendTimeout <- 60000  // Also set send timeout for symmetry
      return Ok(remoteBinding a)
    }

  member this.readInternal (buf : ByteSegment) =
    let socket = lock socketLock (fun () -> this.acceptSocket)
    if socket = null then
      raise (ObjectDisposedException("Socket has been disposed"))
    socket.ReceiveAsync(buf,cancellationToken)

  member this.writeInternal (buf : ByteSegment) =
    let socket = lock socketLock (fun () -> this.acceptSocket)
    if socket = null then
      raise (ObjectDisposedException("Socket has been disposed"))
    socket.SendAsync(buf,cancellationToken)

  member this.shutdown() =
    lock socketLock (fun () ->
      let socket = this.acceptSocket
      if socket <> null then
        this.acceptSocket <- null
        shutdownSocket socket
    )

  interface ITransport with
    member this.read (buf : Memory<byte>) : SocketOp<int> =
      // Use ValueTask directly for zero-allocation when synchronous
      let vt = this.readInternal buf
      if vt.IsCompletedSuccessfully then
        // Synchronous completion - zero allocations!
        ValueTask<Result<int,Error>>(Ok vt.Result)
      else
        // Async path - wrap in task
        ValueTask<Result<int,Error>>(
          task {
            try
              let! bytesRead = vt
              return Ok bytesRead
            with
            | :? SocketException as ex ->
              return Result.Error(Error.SocketError(ex.SocketErrorCode))
            | ex ->
              return Result.Error(Error.ConnectionError(ex.Message))
          })

    member this.write (buf : Memory<byte>) : SocketOp<unit> =
      // Use ValueTask directly for zero-allocation when synchronous
      let vt = this.writeInternal buf
      if vt.IsCompletedSuccessfully then
        // Synchronous completion - zero allocations!
        ValueTask<Result<unit,Error>>(Ok())
      else
        // Async path - wrap in task
        ValueTask<Result<unit,Error>>(
          task {
            try
              let! _ = vt
              return Ok()
            with
            | :? SocketException as ex ->
              return Result.Error(Error.SocketError(ex.SocketErrorCode))
            | ex ->
              return Result.Error(Error.ConnectionError(ex.Message))
          })

    member this.flush () : SocketOp<unit> =
      // TCP sockets do not buffer above the kernel send queue: every byte
      // handed to `SendAsync` is already in the OS buffer by the time the
      // write task completes, so there is nothing for us to flush here.
      ValueTask<Result<unit,Error>>(Ok())

    member this.shutdown()  =
      this.shutdown()
