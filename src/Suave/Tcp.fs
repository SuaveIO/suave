module Suave.Tcp

open System
open System.Threading
open System.Net
open System.Net.Sockets
open Suave.Logging
open Suave.Logging.Message
open Suave.Sockets
open Suave.Utils

let private logger = Log.create "Suave.Tcp"

/// The max backlog of number of requests
[<Literal>]
let MaxBacklog = Int32.MaxValue

/// A TCP Worker is a thing that takes a TCP client and returns an asynchronous
/// workflow thereof.
type TcpWorker<'a> = ConnectionFacade -> Async<'a>

type StartedData =
  { startCalledUtc : DateTimeOffset
    socketBoundUtc : DateTimeOffset option
    binding        : SocketBinding }

  member x.GetStartedListeningElapsedMilliseconds() =
    ((x.socketBoundUtc |> Option.fold (fun _ t -> t) x.startCalledUtc) - x.startCalledUtc).TotalMilliseconds
  override x.ToString() =
    (x.GetStartedListeningElapsedMilliseconds()).ToString() + " ms with binding " + x.binding.ip.ToString() + ":"  + x.binding.port.ToString()

/// Stop the TCP listener server
let stopTcp reason (socket : Socket) =
  try
    logger.debug (
      eventX "Stopping TCP server {because}"
      >> setFieldValue "because" reason)

    socket.Dispose()

    logger.debug (eventX "Stopped TCP server")
  with ex ->
    logger.debug (eventX "Failure stopping TCP server" >> addExn ex)

open System.IO.Pipelines

let createTransport listenSocket =
  let readEventArg = new SocketAsyncEventArgs()
  let userToken = new AsyncUserToken()
  readEventArg.UserToken <- userToken
  readEventArg.add_Completed(fun a b -> userToken.Continuation b)

  let writeEventArg = new SocketAsyncEventArgs()
  let userToken = new AsyncUserToken()
  writeEventArg.UserToken <- userToken
  writeEventArg.add_Completed(fun a b -> userToken.Continuation b)

  let acceptArg = new SocketAsyncEventArgs()
  let userToken = new AsyncUserToken()
  acceptArg.UserToken <- userToken
  acceptArg.add_Completed(fun a b -> userToken.Continuation b)

  new TcpTransport(acceptArg,readEventArg,writeEventArg, listenSocket)

let createConnection listenSocket =
  { socketBinding = SocketBinding.create IPAddress.IPv6Loopback 8080us;
      transport     = createTransport listenSocket;
      pipe = new Pipe();
      lineBuffer    = Array.zeroCreate 8192;
      lineBufferCount = 0 }

let createConnectionFacade connectionPool listenSocket runtime =
  let connection = createConnection listenSocket 
  let facade = new ConnectionFacade(connection, runtime, logger, connectionPool)
  facade

let createPools listenSocket maxOps runtime =

  let connectionPool = new ConcurrentPool<ConnectionFacade>()
  connectionPool.ObjectGenerator <- (fun _ -> createConnectionFacade connectionPool listenSocket runtime)

  //Pre-allocate a set of reusable transportObjects
  for x = 0 to maxOps - 1 do
    let connection = createConnectionFacade connectionPool listenSocket runtime
    connectionPool.Push connection

  connectionPool

// NOTE: performance tip, on mono set nursery-size with a value larger than MAX_CONCURRENT_OPS * BUFFER_SIZE
// i.e: export MONO_GC_PARAMS=nursery-size=128m
// The nursery size must be a power of two in bytes

let private aFewTimes f =
  let s ms = System.Threading.Thread.Sleep (ms : int)
  let rec run = function
    | 0us | 1us -> f ()
    | n -> try f () with e -> s 10; run (n - 1us)
  run 3us

// consider:
// echo 5 > /proc/sys/net/ipv4/tcp_fin_timeout
// echo 1 > /proc/sys/net/ipv4/tcp_tw_recycle
// custom kernel with shorter TCP_TIMEWAIT_LEN in include/net/tcp.h
let job (serveClient : TcpWorker<unit>) binding (connection : ConnectionFacade) = async {

  Interlocked.Increment Globals.numberOfClients |> ignore

  logger.debug (eventX "{client} connected. Now has {totalClients} connected"
    >> setFieldValue "client" (binding.ip.ToString())
    >> setFieldValue "totalClients" (!Globals.numberOfClients))

  connection.Connection.socketBinding <- binding

  try
    use! oo = Async.OnCancel (fun () ->
      logger.debug (eventX "Disconnected client (async cancel)")
      Async.RunSynchronously (connection.shutdown()))

    do! serveClient connection
  with
    | :? System.IO.EndOfStreamException ->
      logger.debug (eventX "Disconnected client (end of stream)")

    | ex ->
      logger.warn (eventX "TCP request processing failed" >> addExn ex)

  logger.debug (eventX "Shutting down transport")
  do! connection.shutdown()
  Interlocked.Decrement(Globals.numberOfClients) |> ignore
  logger.debug (
    eventX "Disconnected {client}. {totalClients} connected."
    >> setFieldValue "client" (binding.ip.ToString())
    >> setFieldValue "totalClients" (!Globals.numberOfClients))
  }

type TcpServer = StartedData -> AsyncResultCell<StartedData> -> TcpWorker<unit> -> Async<unit>

#if NETSTANDARD2_0
#nowarn "9"

let enableRebinding (listenSocket: Socket) =
  let mutable optionValue = 1
  let mutable setsockoptStatus = 0

  if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
    setsockoptStatus <- setsockopt(listenSocket.Handle, SOL_SOCKET_LINUX, SO_REUSEADDR_LINUX, NativePtr.toNativeInt<int> &&optionValue, uint32(sizeof<int>))
  else if RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
    setsockoptStatus <- setsockopt(listenSocket.Handle, SOL_SOCKET_OSX, SO_REUSEADDR_OSX, NativePtr.toNativeInt<int> &&optionValue, uint32(sizeof<int>))

  if setsockoptStatus <> 0 then
    logger.warn(
          eventX "Setting SO_REUSEADDR failed with errno '{errno}'."
          >> setFieldValue "errno" (Marshal.GetLastWin32Error()))
#endif

let runServer maxConcurrentOps (binding: SocketBinding) (runtime:HttpRuntime) startData
              (acceptingConnections: AsyncResultCell<StartedData>) serveClient = async {
  try
    use listenSocket = new Socket(binding.endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
    #if NETSTANDARD2_0
    enableRebinding(listenSocket)
    #endif
    listenSocket.NoDelay <- true

    let connectionPool =
      createPools listenSocket maxConcurrentOps runtime

    aFewTimes (fun () -> listenSocket.Bind binding.endpoint)
    listenSocket.Listen MaxBacklog

    use! disposable = Async.OnCancel(fun () ->
      stopTcp "runServer async cancelled" listenSocket)

    // get the actual assigned port from listeSocket
    let _binding = { startData.binding with port = uint16((listenSocket.LocalEndPoint :?> IPEndPoint).Port) }

    let startData =
      { startData with socketBoundUtc = Some (Globals.utcNow()); binding = _binding }

    acceptingConnections.complete startData |> ignore

    logger.info (
      eventX "Smooth! Suave listener started in {startedListeningMilliseconds:#.###}ms with binding {ipAddress}:{port}"
      >> setFieldValue "startedListeningMilliseconds" (startData.GetStartedListeningElapsedMilliseconds())
      // .Address can throw exceptions, just log its string representation
      >> setFieldValue "ipAddress" (startData.binding.ip.ToString())
      >> setFieldValue "port" startData.binding.port
      >> setSingleName "Suave.Tcp.runServer")

    let! token = Async.CancellationToken

    while not (token.IsCancellationRequested) do
      try
        let connection : ConnectionFacade = connectionPool.Pop()
        let! r = (connection.Connection.transport :?> TcpTransport).accept()
        match r with
        | Ok remoteBinding ->
          // start a new async worker for each accepted TCP client
          //connection.socketBinding <- remoteBinding
          Async.Start (job serveClient remoteBinding connection, token)
        | Result.Error e ->
          failwithf "Socket failed to accept client, error: %A" e

      with ex ->
        logger.error (eventX "Socket failed to accept a client" >> addExn ex)

  with ex ->
    logger.fatal (eventX "TCP server failed" >> addExn ex)
    return raise ex
}

/// Start a new TCP server with a specific IP, Port and with a serve_client worker
/// returning an async workflow whose result can be awaited (for when the tcp server has started
/// listening to its address/port combination), and an asynchronous workflow that
/// yields when the full server is cancelled. If the 'has started listening' workflow
/// returns None, then the start timeout expired.
let startTcpIpServerAsync (serveClient : TcpWorker<unit>)
                          (binding     : SocketBinding)
                          (runServer   : TcpServer) =

  let acceptingConnections = new AsyncResultCell<StartedData>()

  let startData =
        { startCalledUtc = Globals.utcNow ()
          socketBoundUtc = None
          binding        = binding }

  acceptingConnections.awaitResult()
    , runServer startData acceptingConnections serveClient
