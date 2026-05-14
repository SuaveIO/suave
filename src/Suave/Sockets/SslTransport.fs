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

  /// The ALPN protocol negotiated during the TLS handshake. When ALPN was not
  /// used (or the client did not advertise a known protocol), this is
  /// `SslApplicationProtocol()` (i.e. its `Protocol` byte sequence is empty).
  /// Callers may inspect this after `AuthenticateAsServerAsync` completes to
  /// dispatch to an HTTP/2 reader for "h2", or fall back to HTTP/1.1 otherwise.
  [<DefaultValue>]
  val mutable negotiatedApplicationProtocol: SslApplicationProtocol

  let socketLock = obj()

  /// ALPN protocols advertised by the server, in preference order: HTTP/2 first,
  /// then HTTP/1.1. RFC 7301 / RFC 7540 §3.3.
  static let alpnProtocols =
    ResizeArray<SslApplicationProtocol>(
      [| SslApplicationProtocol.Http2; SslApplicationProtocol.Http11 |])

  /// Build the server authentication options used for every TLS handshake on
  /// this transport. Centralised so the inline handshake in `Tcp.runAcceptor`
  /// and `SslTransport.accept` stay in sync.
  static member buildServerAuthenticationOptions
      (certificate: X509Certificate, checkCertificateRevocation: bool) =
    let opts = SslServerAuthenticationOptions()
    opts.ServerCertificate <- certificate
    opts.ClientCertificateRequired <- false
    opts.EnabledSslProtocols <- SslProtocols.Tls12 ||| SslProtocols.Tls13
    opts.CertificateRevocationCheckMode <-
      if checkCertificateRevocation then X509RevocationMode.Online
      else X509RevocationMode.NoCheck
    opts.ApplicationProtocols <- alpnProtocols
    opts

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
        socket.ReceiveTimeout <- 60000  // Receive timeout
        socket.SendTimeout <- 60000     // Send timeout
        this.networkStream <- new NetworkStream(socket, true)
        this.sslStream <- new SslStream(this.networkStream, false)

        // Perform SSL handshake (with ALPN advertising h2, http/1.1).
        let opts =
          SslTransport.buildServerAuthenticationOptions(
            certificate, checkCertificateRevocation = true)
        do! this.sslStream.AuthenticateAsServerAsync(opts, cancellationToken)
        this.negotiatedApplicationProtocol <- this.sslStream.NegotiatedApplicationProtocol
        
        return Ok(remoteBinding socket)
      with
        | :? AuthenticationException as ex ->
          return Result.Error(ConnectionError($"SSL authentication failed: {ex.Message}"))
        | :? IOException as ex ->
          return Result.Error(ConnectionError($"SSL IO error: {ex.Message}"))
        | ex ->
          return Result.Error(ConnectionError($"SSL error: {ex.Message}"))
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
              return Result.Error(Error.ConnectionError($"SSL read error: {ex.Message}"))
            | :? SocketException as ex ->
              return Result.Error(Error.SocketError(ex.SocketErrorCode))
            | :? ObjectDisposedException ->
              return Result.Error(Error.ConnectionError("SSL stream has been disposed"))
            | ex ->
              return Result.Error(Error.ConnectionError($"Unexpected SSL read error: {ex.Message}"))
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
              return Result.Error(Error.ConnectionError($"SSL write error: {ex.Message}"))
            | :? SocketException as ex ->
              return Result.Error(Error.SocketError(ex.SocketErrorCode))
            | :? ObjectDisposedException ->
              return Result.Error(Error.ConnectionError("SSL stream has been disposed"))
            | ex ->
              return Result.Error(Error.ConnectionError($"Unexpected SSL write error: {ex.Message}"))
          })

    member this.shutdown() =
      this.shutdown()

    member this.flush () : SocketOp<unit> =
      let stream = lock socketLock (fun () -> this.sslStream)
      if stream = null then
        ValueTask<Result<unit,Error>>(Ok())
      else
        ValueTask<Result<unit,Error>>(
          task {
            try
              do! stream.FlushAsync(cancellationToken)
              return Ok()
            with
            | :? IOException as ex ->
              return Result.Error(Error.ConnectionError($"SSL flush error: {ex.Message}"))
            | :? SocketException as ex ->
              return Result.Error(Error.SocketError(ex.SocketErrorCode))
            | :? ObjectDisposedException ->
              return Result.Error(Error.ConnectionError("SSL stream has been disposed"))
            | ex ->
              return Result.Error(Error.ConnectionError($"Unexpected SSL flush error: {ex.Message}"))
          })
