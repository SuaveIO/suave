namespace Suave.Sockets

open System
open System.Net
open System.Net.Sockets
open System.Net.Security
open System.Security.Authentication
open System.Security.Cryptography.X509Certificates
open System.Threading
open System.Threading.Tasks
open System.IO
open Suave

[<AllowNullLiteral>]
type SslTransport(listenSocket: Socket, certificate: X509Certificate, cancellationToken: CancellationToken) =

  [<DefaultValue>]
  val mutable acceptSocket: Socket
  
  [<DefaultValue>]
  val mutable sslStream: SslStream
  
  [<DefaultValue>]
  val mutable networkStream: NetworkStream
  
  let socketLock = obj()

  let shutdownSocket (acceptSocket: Socket) =
    if acceptSocket <> null then
      try
        try
          acceptSocket.Shutdown(SocketShutdown.Both)
        with
          | :? SocketException -> () // Socket may already be closed
          | :? ObjectDisposedException -> () // Socket already disposed
      finally
        try
          acceptSocket.Dispose()
        with
          | :? ObjectDisposedException -> () // Already disposed, ignore

  let remoteBinding (socket: Socket): SocketBinding =
    let rep = socket.RemoteEndPoint :?> IPEndPoint
    { ip = rep.Address; port = uint16 rep.Port }

  member this.accept() =
    task {
      try
        let! socket = listenSocket.AcceptAsync(cancellationToken)
        this.acceptSocket <- socket
        this.networkStream <- new NetworkStream(socket, true)
        this.sslStream <- new SslStream(this.networkStream, false)
        
        // Perform SSL handshake
        do! this.sslStream.AuthenticateAsServerAsync(
          certificate,
          clientCertificateRequired = false,
          enabledSslProtocols = (SslProtocols.Tls12 ||| SslProtocols.Tls13),
          checkCertificateRevocation = true)
        
        return Ok(remoteBinding socket)
      with
        | :? AuthenticationException as ex ->
          return Result.Error(ConnectionError(sprintf "SSL authentication failed: %s" ex.Message))
        | :? IOException as ex ->
          return Result.Error(ConnectionError(sprintf "SSL IO error: %s" ex.Message))
        | ex ->
          return Result.Error(ConnectionError(sprintf "SSL error: %s" ex.Message))
    }

  member this.readInternal(buf: ByteSegment) =
    let stream = lock socketLock (fun () -> this.sslStream)
    if stream = null then
      raise (ObjectDisposedException("SSL stream has been disposed"))
    stream.ReadAsync(buf, cancellationToken)

  member this.writeInternal(buf: ByteSegment) =
    let stream = lock socketLock (fun () -> this.sslStream)
    if stream = null then
      raise (ObjectDisposedException("SSL stream has been disposed"))
    stream.WriteAsync(buf, cancellationToken)

  member this.shutdown() =
    lock socketLock (fun () ->
      if this.sslStream <> null then
        try
          this.sslStream.Dispose()
          this.sslStream <- null
        with _ -> ()
      
      if this.networkStream <> null then
        try
          this.networkStream.Dispose()
          this.networkStream <- null
        with _ -> ()
      
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
            | :? IOException as ex ->
              return Result.Error(Error.ConnectionError(sprintf "SSL read error: %s" ex.Message))
            | :? SocketException as ex ->
              return Result.Error(Error.SocketError(ex.SocketErrorCode))
            | :? ObjectDisposedException ->
              return Result.Error(Error.ConnectionError("SSL stream has been disposed"))
            | ex ->
              return Result.Error(Error.ConnectionError(sprintf "Unexpected SSL read error: %s" ex.Message))
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
              do! vt
              return Ok()
            with
            | :? IOException as ex ->
              return Result.Error(Error.ConnectionError(sprintf "SSL write error: %s" ex.Message))
            | :? SocketException as ex ->
              return Result.Error(Error.SocketError(ex.SocketErrorCode))
            | :? ObjectDisposedException ->
              return Result.Error(Error.ConnectionError("SSL stream has been disposed"))
            | ex ->
              return Result.Error(Error.ConnectionError(sprintf "Unexpected SSL write error: %s" ex.Message))
          })

    member this.shutdown() : SocketOp<unit> =
      this.shutdown()
      ValueTask<Result<unit,Error>>(Ok())
