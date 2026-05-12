module Suave.Tcp

open System
open System.Threading
open System.Net
open System.Net.Sockets
open System.Net.Security
open System.Security.Authentication
open Suave.Sockets
open Suave.Utils
open Suave
open System.Threading.Tasks

/// The max backlog of number of requests
[<Literal>]
let MaxBacklog = Int32.MaxValue

/// A TCP Worker is a thing that takes a TCP client and returns an asynchronous
/// workflow thereof.
type TcpWorker<'a> = ConnectionFacade -> Task<'a>

type StartedData =
  { startCalledUtc : DateTimeOffset
    socketBoundUtc : DateTimeOffset option
    binding        : SocketBinding }

  member x.GetStartedListeningElapsedMilliseconds() =
    ((x.socketBoundUtc |> Option.fold (fun _ t -> t) x.startCalledUtc) - x.startCalledUtc).TotalMilliseconds
  override x.ToString() =
    (x.GetStartedListeningElapsedMilliseconds()).ToString() + " ms with binding " + x.binding.ip.ToString() + ":"  + x.binding.port.ToString()

/// Stop the TCP listener server
let stopTcp (reason:string) (socket : Socket) =
  Console.WriteLine("Stopping TCP server due to {0}", reason)
  try
    socket.Dispose()
  with ex ->
    ()

open System.IO.Pipelines
open System.Runtime.InteropServices
open Native
open Microsoft.FSharp.NativeInterop

let createTransport listenSocket binding cancellationToken : ITransport =
  match binding.scheme with
  | HTTP -> 
      new TcpTransport(listenSocket, cancellationToken) :> ITransport
  | HTTPS certificate ->
      new SslTransport(listenSocket, certificate, cancellationToken) :> ITransport

let createReader (transport: ITransport) pipe cancellationToken =
  new HttpReader(transport, pipe, cancellationToken)

// Per-connection inbound Pipe options.
// - Inline schedulers eliminate threadpool hops between the readLoop (writer side)
//   and the request processor (reader side); since each connection is logically a
//   single async sequence on a HTTP/1.1 keep-alive socket, running continuations
//   inline on the completing thread avoids both a context switch and the Pipe's
//   internal monitor contention that arises when two threads race on the lock.
// - useSynchronizationContext=false avoids capturing/posting back to a SyncContext.
let private pipeOptions =
  new PipeOptions(
    readerScheduler = PipeScheduler.Inline,
    writerScheduler = PipeScheduler.Inline,
    useSynchronizationContext = false)

let createConnection listenSocket binding cancellationToken bufferSize =
  let transport = createTransport listenSocket binding cancellationToken
  // Rent from Suave's private BufferPool to avoid contention on the process-wide
  // ArrayPool<byte>.Shared (used by the BCL, JSON, gzip, ASP.NET, etc.).
  let lineBuffer = Globals.BufferPool.rent bufferSize
  let pipe = new Pipe(pipeOptions)
  let reader = createReader transport pipe cancellationToken
  { socketBinding = SocketBinding.create IPAddress.IPv6Loopback 8080us;
      transport     = transport;
      reader = reader;
      pipe = pipe;
      lineBuffer    = lineBuffer;
      lineBufferCount = 0;
      utf8Encoder = System.Text.Encoding.UTF8.GetEncoder();
      isLongLived = false }

let createConnectionFacade tracker connectionPool listenSocket binding (runtime: HttpRuntime) cancellationToken bufferSize webpart =
  let connection = createConnection listenSocket binding cancellationToken bufferSize
  let facade = new ConnectionFacade(connection, runtime, connectionPool, tracker, cancellationToken, webpart)
  facade

let createPools listenSocket binding maxOps runtime cancellationToken bufferSize (webpart:WebPart) healthCheckEnabled healthCheckIntervalMs maxConnectionAgeSeconds =

  // Wire the h2c upgrade handler into ConnectionFacade so HTTP/1.1 → HTTP/2
  // cleartext upgrade requests are recognised on this server. This is
  // idempotent and intentionally lives here (rather than at module load) so
  // that `ConnectionFacade.fs` does not depend on `Http2.fs` directly.
  Suave.Http2.H2cUpgrade.register ()

  let connectionPool = new ConcurrentPool<ConnectionFacade>()
  
  // Create active connection tracker for health checking
  let tracker = new Suave.ConnectionHealthChecker.ActiveConnectionTracker<ConnectionFacade>()
  
  connectionPool.ObjectGenerator <- (fun _ -> createConnectionFacade tracker connectionPool listenSocket binding runtime cancellationToken bufferSize webpart)
  
  // Set up pool callbacks to track connection lifecycle
  connectionPool.OnConnectionAcquired <- Some (fun conn -> tracker.TrackConnection(conn))
  connectionPool.OnConnectionReturned <- Some (fun conn -> tracker.UntrackConnection(conn))
  connectionPool.OnConnectionReset <- Some (fun conn -> conn.Connection.isLongLived <- false)

  // Configure health checking on the pool
  connectionPool.HealthCheckEnabled <- healthCheckEnabled
  connectionPool.HealthCheckIntervalMs <- healthCheckIntervalMs

  //Pre-allocate a set of reusable transportObjects
  for x = 0 to maxOps - 1 do
    let connection = createConnectionFacade tracker connectionPool listenSocket binding runtime cancellationToken bufferSize webpart
    connectionPool.Push connection

  // Launch background health checker if enabled
  if healthCheckEnabled then
    let healthCheckerConfig : Suave.ConnectionHealthChecker.HealthCheckerConfig = {
      checkIntervalMs = healthCheckIntervalMs
      enabled = true
      verbose = false
      maxConnectionAgeSeconds = maxConnectionAgeSeconds
    }
    
    let getSocket (conn: ConnectionFacade) : Socket option =
      try
        match conn.Connection.transport with
        | :? Suave.Sockets.TcpTransport as tcp -> 
          if tcp.acceptSocket <> null then Some (tcp.acceptSocket) else None
        | :? Suave.Sockets.SslTransport as ssl -> 
          if ssl.acceptSocket <> null then Some (ssl.acceptSocket) else None
        | _ -> None
      with _ -> None
    
    let isLongLived (conn: ConnectionFacade) : bool =
      conn.Connection.isLongLived
    
    let closeConnection (conn: ConnectionFacade) : unit =
      try
        conn.shutdown()
      with _ -> ()
    
    let healthCheckerTask = 
      Suave.ConnectionHealthChecker.startHealthChecker tracker healthCheckerConfig getSocket isLongLived closeConnection
    
    connectionPool.HealthCheckTask <- Some healthCheckerTask

  connectionPool

// NOTE: performance tip, on mono set nursery-size with a value larger than MAX_CONCURRENT_OPS * BUFFER_SIZE
// i.e: export MONO_GC_PARAMS=nursery-size=128m
// The nursery size must be a power of two in bytes

let private aFewTimes f = task{
  let mutable tries = 3
  while tries > 0 do
    if tries < 2 then
      f (); tries <- -1
    else
      try f (); tries <- -1
      with ex ->
        tries <- tries - 1
        do! Task.Delay 10
  }

let private aFewTimesDeterministic f =
  let mutable tries = 4
  while tries > 0 do
    if tries < 2 then
      f (); tries <- -1
    else
      try f (); tries <- -1
      with ex ->
        tries <- tries - 1
        do Thread.Sleep 10

// consider:
// echo 5 > /proc/sys/net/ipv4/tcp_fin_timeout
// echo 1 > /proc/sys/net/ipv4/tcp_tw_recycle
// custom kernel with shorter TCP_TIMEWAIT_LEN in include/net/tcp.h

#nowarn "9"

type TcpServer = StartedData -> AsyncResultCell<StartedData> -> Task

let enableRebinding (listenSocket: Socket) =
  let mutable optionValue = 1
  let mutable setsockoptStatus = 0

  if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
    setsockoptStatus <- setsockopt(listenSocket.Handle, SOL_SOCKET_LINUX, SO_REUSEADDR_LINUX, NativePtr.toNativeInt<int> &&optionValue, uint32(sizeof<int>))
  else if RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
    setsockoptStatus <- setsockopt(listenSocket.Handle, SOL_SOCKET_OSX, SO_REUSEADDR_OSX, NativePtr.toNativeInt<int> &&optionValue, uint32(sizeof<int>))

/// Enable SO_REUSEPORT on the listen socket so that multiple sockets can be
/// bound to the same address+port and the kernel will load-balance incoming
/// connections across them. Returns true on platforms that support it (Linux
/// 3.9+, macOS, BSD) and on which setsockopt succeeded; false otherwise
/// (notably Windows, where the semantically-similar SO_REUSEADDR is not a
/// safe substitute and we fall back to a single acceptor).
let tryEnableReusePort (listenSocket: Socket) : bool =
  let mutable optionValue = 1
  if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
    setsockopt(listenSocket.Handle, SOL_SOCKET_LINUX, SO_REUSEPORT_LINUX, NativePtr.toNativeInt<int> &&optionValue, uint32(sizeof<int>)) = 0
  elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX)
       || RuntimeInformation.IsOSPlatform(OSPlatform.FreeBSD) then
    setsockopt(listenSocket.Handle, SOL_SOCKET_OSX, SO_REUSEPORT_OSX, NativePtr.toNativeInt<int> &&optionValue, uint32(sizeof<int>)) = 0
  else
    false

/// Run a single accept loop on an already-bound, already-listening socket.
/// `announce` is invoked once at start to report the bound endpoint to the
/// orchestrator (only the first acceptor's announcement is forwarded to the
/// caller via `acceptingConnections`).
let private runAcceptor
      (listenSocket: Socket)
      (connectionPool: Suave.Sockets.ConcurrentPool<ConnectionFacade>)
      (runtime: HttpRuntime)
      (cancellationToken: CancellationToken) : Task =
  task {
    let remoteBinding (socket : Socket) =
      let rep = socket.RemoteEndPoint :?> IPEndPoint
      { ip = rep.Address; port = uint16 rep.Port }
    try
      while not(cancellationToken.IsCancellationRequested) do
          try
            let! acceptedSocket = listenSocket.AcceptAsync(cancellationToken)

            // Only pop a connection AFTER we have an accepted socket
            let connection : ConnectionFacade = connectionPool.Pop()

            // Set the accepted socket and perform SSL handshake if needed
            let! remoteBindingResult = task {
              match connection.Connection.transport with
              | :? TcpTransport as tcpTransport ->
                  tcpTransport.acceptSocket <- acceptedSocket
                  return Ok(remoteBinding acceptedSocket)
              | :? SslTransport as sslTransport ->
                  sslTransport.acceptSocket <- acceptedSocket
                  sslTransport.networkStream <- new NetworkStream(acceptedSocket, true)
                  sslTransport.sslStream <- new SslStream(sslTransport.networkStream, false)

                  // Perform SSL handshake
                  try
                    let certificate =
                      match runtime.matchedBinding.scheme with
                      | HTTPS cert -> cert
                      | HTTP -> failwith "Expected HTTPS binding for SSL transport"

                    do! sslTransport.sslStream.AuthenticateAsServerAsync(
                      certificate,
                      clientCertificateRequired = false,
                      enabledSslProtocols = (SslProtocols.Tls12 ||| SslProtocols.Tls13),
                      checkCertificateRevocation = false)
                    return Ok(remoteBinding acceptedSocket)
                  with ex ->
                    return Result.Error(Error.ConnectionError($"SSL handshake failed: {ex.Message}"))
              | _ ->
                  return Ok(remoteBinding acceptedSocket)
            }

            match remoteBindingResult with
            | Ok binding ->
                // Fire and forget the connection handling using Task.Run
                let _connectionTask = Task.Run<unit>(Func<Task<unit>>(fun () -> connection.accept(binding)), cancellationToken)
                ()
            | Result.Error error ->
                // SSL handshake failed, return connection to pool
                connectionPool.Push(connection)
          with ex ->
            if Globals.verbose then
              Console.WriteLine("TCP server accept exception: {0}", ex)
    with
      | :? AggregateException
      | :? OperationCanceledException
      | :? TaskCanceledException -> ()
  } :> Task

let runServerEx acceptorCount maxConcurrentOps bufferSize (binding: SocketBinding) (runtime:HttpRuntime) (cancellationToken: CancellationToken) (webpart: WebPart) healthCheckEnabled healthCheckIntervalMs maxConnectionAgeSeconds startData
              (acceptingConnections: AsyncResultCell<StartedData>) : Task =
  Task.Run(Func<Task>(fun () -> task {
    // Decide effective acceptor count.
    //   acceptorCount = 0  -> auto: min(Environment.ProcessorCount, 16) on platforms
    //                        with SO_REUSEPORT, otherwise 1
    //   acceptorCount > 1  -> requires SO_REUSEPORT (Linux/macOS/BSD); falls back to 1
    //                        on platforms without it (notably Windows)
    let autoAcceptorCap = 16
    let canReusePort =
      RuntimeInformation.IsOSPlatform(OSPlatform.Linux)
      || RuntimeInformation.IsOSPlatform(OSPlatform.OSX)
      || RuntimeInformation.IsOSPlatform(OSPlatform.FreeBSD)
    let effective =
      if acceptorCount = 0 then
        if canReusePort then min Environment.ProcessorCount autoAcceptorCap
        else 1
      else
        let requested = max 1 acceptorCount
        if requested > 1 && not canReusePort then 1
        else requested

    // Open all listen sockets first so we can fail fast if any bind fails.
    let listenSockets = Array.zeroCreate<Socket> effective
    try
      for i = 0 to effective - 1 do
        let s = new Socket(binding.endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
        listenSockets.[i] <- s
        enableRebinding(s)
        s.NoDelay <- true
        if effective > 1 then
          // Best-effort. If this fails we abort: bind would otherwise succeed
          // only on the first socket and the rest would EADDRINUSE.
          if not (tryEnableReusePort s) then
            failwith "SO_REUSEPORT setsockopt failed; cannot run multiple acceptors"
        aFewTimesDeterministic (fun () -> s.Bind binding.endpoint)
        s.Listen MaxBacklog

      let leader = listenSockets.[0]
      let _binding = { startData.binding with port = uint16((leader.LocalEndPoint :?> IPEndPoint).Port) }

      let startData =
        { startData with socketBoundUtc = Some (Globals.utcNow()); binding = _binding }

      acceptingConnections.complete startData |> ignore

      let startedListeningMilliseconds = startData.GetStartedListeningElapsedMilliseconds()
      let ipAddress = startData.binding.ip.ToString()
      let port = startData.binding.port

      if effective > 1 then
        Console.WriteLine($"Smooth! Suave v{Globals.SuaveVersion} listener started in {startedListeningMilliseconds} ms with binding {ipAddress}:{port} ({effective} acceptors)")
      else
        Console.WriteLine($"Smooth! Suave v{Globals.SuaveVersion} listener started in {startedListeningMilliseconds} ms with binding {ipAddress}:{port}")

      // Create one connection pool per acceptor so each accept loop pops from
      // its own thread-local-ish pool, removing cross-acceptor contention.
      let pools =
        listenSockets
        |> Array.map (fun s ->
            createPools s runtime.matchedBinding maxConcurrentOps runtime cancellationToken bufferSize webpart healthCheckEnabled healthCheckIntervalMs maxConnectionAgeSeconds)

      let acceptorTasks =
        Array.init effective (fun i ->
          let s = listenSockets.[i]
          let pool = pools.[i]
          // Spin each acceptor on the threadpool independently so they don't
          // serialise on the orchestrator's thread.
          Task.Run(Func<Task>(fun () -> runAcceptor s pool runtime cancellationToken)))

      try
        do! Task.WhenAll(acceptorTasks)
      with
        | :? AggregateException
        | :? OperationCanceledException
        | :? TaskCanceledException -> ()

      for s in listenSockets do
        stopTcp "cancellation requested" s
    with
      | :? AggregateException
      | :? OperationCanceledException
      | :? TaskCanceledException ->
        for s in listenSockets do
          if not (isNull (box s)) then stopTcp "The operation was canceled" s
      | ex ->
        Console.WriteLine("TCP server runtime exception: {0}", ex)
        for s in listenSockets do
          if not (isNull (box s)) then stopTcp "runtime exception" s
        }))

/// Backwards-compatible single-acceptor entry point.
let runServer maxConcurrentOps bufferSize (binding: SocketBinding) (runtime:HttpRuntime) (cancellationToken: CancellationToken) (webpart: WebPart) healthCheckEnabled healthCheckIntervalMs maxConnectionAgeSeconds startData
              (acceptingConnections: AsyncResultCell<StartedData>) : Task =
  runServerEx 1 maxConcurrentOps bufferSize binding runtime cancellationToken webpart healthCheckEnabled healthCheckIntervalMs maxConnectionAgeSeconds startData acceptingConnections

/// Start a new TCP server with a specific IP, Port and with a serve_client worker
/// returning an async workflow whose result can be awaited (for when the tcp server has started
/// listening to its address/port combination), and an asynchronous workflow that
/// yields when the full server is cancelled. If the 'has started listening' workflow
/// returns None, then the start timeout expired.
let startTcpIpServerAsync (binding : SocketBinding) (runServer : TcpServer) =
  let acceptingConnections = new AsyncResultCell<StartedData>()
  let startData =
        { startCalledUtc = Globals.utcNow ()
          socketBoundUtc = None
          binding        = binding }

  acceptingConnections.awaitResult()
    , runServer startData acceptingConnections

let startTcpIpServer (binding : SocketBinding) (runServer : TcpServer) =
  let acceptingConnections = new AsyncResultCell<StartedData>()
  let startData =
        { startCalledUtc = Globals.utcNow ()
          socketBoundUtc = None
          binding        = binding }

  let listening = acceptingConnections.awaitResult()
  let serverTask = runServer startData acceptingConnections
  listening, serverTask
