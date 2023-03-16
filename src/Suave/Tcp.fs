module Suave.Tcp

open System
open System.Threading
open System.Net
open System.Net.Sockets
open Suave.Logging
open Suave.Logging.Message
open Suave.Sockets
open Suave.Utils
open System.Threading.Tasks

let private logger = Log.create "Suave.Tcp"

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
let stopTcp reason (socket : Socket) =
  try
    logger.debug (eventX "Stopping TCP server {because}" >> setFieldValue "because" reason)
    socket.Dispose()
    logger.debug (eventX "Stopped TCP server")
  with ex ->
    logger.debug (eventX "Failure stopping TCP server" >> addExn ex)

open System.IO.Pipelines
open System.Runtime.InteropServices
open Native
open Microsoft.FSharp.NativeInterop

let createTransport listenSocket cancellationToken =
  new TcpTransport(listenSocket,cancellationToken)

let createConnection listenSocket cancellationToken bufferSize =
  { socketBinding = SocketBinding.create IPAddress.IPv6Loopback 8080us;
      transport     = createTransport listenSocket cancellationToken;
      pipe = new Pipe();
      lineBuffer    = Array.zeroCreate bufferSize;
      lineBufferCount = 0 }

let createConnectionFacade connectionPool listenSocket (runtime: HttpRuntime) cancellationToken bufferSize webpart =
  let connection = createConnection listenSocket cancellationToken bufferSize
  let facade = new ConnectionFacade(connection, runtime, logger, connectionPool,webpart)
  facade

let createPools listenSocket maxOps runtime cancellationToken bufferSize (webpart:WebPart) =

  let connectionPool = new ConcurrentPool<ConnectionFacade>()
  connectionPool.ObjectGenerator <- (fun _ -> createConnectionFacade connectionPool listenSocket runtime cancellationToken bufferSize webpart)

  //Pre-allocate a set of reusable transportObjects
  for x = 0 to maxOps - 1 do
    let connection = createConnectionFacade connectionPool listenSocket runtime cancellationToken bufferSize webpart
    connectionPool.Push connection

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

// consider:
// echo 5 > /proc/sys/net/ipv4/tcp_fin_timeout
// echo 1 > /proc/sys/net/ipv4/tcp_tw_recycle
// custom kernel with shorter TCP_TIMEWAIT_LEN in include/net/tcp.h

#nowarn "9"

type TcpServer = StartedData -> AsyncResultCell<StartedData> -> Task<unit>

let enableRebinding (listenSocket: Socket) =
  let mutable optionValue = 1
  let mutable setsockoptStatus = 0

  if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
    setsockoptStatus <- setsockopt(listenSocket.Handle, SOL_SOCKET_LINUX, SO_REUSEADDR_LINUX, NativePtr.toNativeInt<int> &&optionValue, uint32(sizeof<int>))
  else if RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
    setsockoptStatus <- setsockopt(listenSocket.Handle, SOL_SOCKET_OSX, SO_REUSEADDR_OSX, NativePtr.toNativeInt<int> &&optionValue, uint32(sizeof<int>))

  if setsockoptStatus <> 0 then
    logger.warn(eventX "Setting SO_REUSEADDR failed with errno '{errno}'." >> setFieldValue "errno" (Marshal.GetLastWin32Error()))


let runServer maxConcurrentOps bufferSize (binding: SocketBinding) (runtime:HttpRuntime) (cancellationToken: CancellationToken) (webpart: WebPart) startData
              (acceptingConnections: AsyncResultCell<StartedData>) = task {

  use listenSocket = new Socket(binding.endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
  try

    enableRebinding(listenSocket)
    listenSocket.NoDelay <- true

    let connectionPool = createPools listenSocket maxConcurrentOps runtime cancellationToken bufferSize webpart

    do! aFewTimes (fun () -> listenSocket.Bind binding.endpoint)
    listenSocket.Listen MaxBacklog

    // Get the actual assigned port from listeSocket
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

    while not(cancellationToken.IsCancellationRequested) do
        let connection : ConnectionFacade = connectionPool.Pop()
        logger.verbose (eventX "Waiting for accept")
        let! r = connection.Connection.transport.accept()
        match r with
        | Ok remoteBinding ->
          // Start a new task for each accepted TCP client
          let task = Task.Factory.StartNew(fun () -> connection.accept(remoteBinding))
          ()
        | Result.Error e ->
          failwithf "Socket failed to accept client, error: %A" e

    stopTcp "cancellation requested" listenSocket
  with
    | :? System.OperationCanceledException
    | :? System.Threading.Tasks.TaskCanceledException ->
      stopTcp "The operation was canceled" listenSocket
    | ex ->
      stopTcp "runtime exception" listenSocket
      logger.fatal (eventX "TCP server failed" >> addExn ex)
      return raise ex
    
}

/// Start a new TCP server with a specific IP, Port and with a serve_client worker
/// returning an async workflow whose result can be awaited (for when the tcp server has started
/// listening to its address/port combination), and an asynchronous workflow that
/// yields when the full server is cancelled. If the 'has started listening' workflow
/// returns None, then the start timeout expired.
let startTcpIpServerAsync (binding : SocketBinding) (runServer : TcpServer) =

  let acceptingConnections = new AsyncResultCell<StartedData>()
  Globals.numberOfClients := 0
  let startData =
        { startCalledUtc = Globals.utcNow ()
          socketBoundUtc = None
          binding        = binding }

  acceptingConnections.awaitResult()
    , runServer startData acceptingConnections
