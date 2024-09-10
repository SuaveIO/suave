module Suave.Tcp

open System
open System.Threading
open System.Net
open System.Net.Sockets
open Suave.Sockets
open Suave.Utils
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
let stopTcp reason (socket : Socket) =
  try
    socket.Dispose()
  with ex ->
    ()

open System.IO.Pipelines
open System.Runtime.InteropServices
open Native
open Microsoft.FSharp.NativeInterop

let createTransport listenSocket cancellationToken =
  new TcpTransport(listenSocket,cancellationToken)

let createReader transport lineBuffer pipe cancellationToken =
  new HttpReader(transport, lineBuffer, pipe, cancellationToken)

let createConnection listenSocket cancellationToken bufferSize =
  let transport = createTransport listenSocket cancellationToken
  let lineBuffer = Array.zeroCreate bufferSize
  let pipe = new Pipe()
  let reader = createReader transport lineBuffer pipe cancellationToken
  { socketBinding = SocketBinding.create IPAddress.IPv6Loopback 8080us;
      transport     = transport;
      reader = reader;
      pipe = pipe;
      lineBuffer    = lineBuffer;
      lineBufferCount = 0 }

let createConnectionFacade connectionPool listenSocket (runtime: HttpRuntime) cancellationToken bufferSize webpart =
  let connection = createConnection listenSocket cancellationToken bufferSize
  let facade = new ConnectionFacade(connection, runtime, connectionPool,cancellationToken,webpart)
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

let runServer maxConcurrentOps bufferSize (binding: SocketBinding) (runtime:HttpRuntime) (cancellationToken: CancellationToken) (webpart: WebPart) startData
              (acceptingConnections: AsyncResultCell<StartedData>) =
  Task.Run(fun () ->
    use listenSocket = new Socket(binding.endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
    try

      enableRebinding(listenSocket)
      listenSocket.NoDelay <- true

      let connectionPool = createPools listenSocket maxConcurrentOps runtime cancellationToken bufferSize webpart

      aFewTimesDeterministic (fun () -> listenSocket.Bind binding.endpoint)
      listenSocket.Listen MaxBacklog

      let _binding = { startData.binding with port = uint16((listenSocket.LocalEndPoint :?> IPEndPoint).Port) }

      let startData =
        { startData with socketBoundUtc = Some (Globals.utcNow()); binding = _binding }

      acceptingConnections.complete startData |> ignore

      let startedListeningMilliseconds = (startData.GetStartedListeningElapsedMilliseconds())
      let ipAddress = (startData.binding.ip.ToString())
      let port = startData.binding.port

      Console.WriteLine($"Smooth! Suave listener started in {startedListeningMilliseconds} ms with binding {ipAddress}:{port}")

      let remoteBinding (socket : Socket) =
        let rep = socket.RemoteEndPoint :?> IPEndPoint
        { ip = rep.Address; port = uint16 rep.Port }

      while not(cancellationToken.IsCancellationRequested) do
          let connection : ConnectionFacade = connectionPool.Pop()

          let continuation (vt: Task<Socket>) =
            connection.Connection.transport.acceptSocket <- vt.Result
            ignore(Task.Factory.StartNew(fun () -> connection.accept(remoteBinding vt.Result),cancellationToken))

          let task = listenSocket.AcceptAsync(cancellationToken)

          // kestrel uses here  ThreadPool.UnsafeQueueUserWorkItem(kestrelConnection, preferLocal: false);

          if task.IsCompleted then
            connection.Connection.transport.acceptSocket <- task.Result
            ignore(Task.Factory.StartNew(fun () -> connection.accept(remoteBinding task.Result),cancellationToken))
          else
            let task2 = task.AsTask().ContinueWith(new Action<Task<_>>(continuation),cancellationToken)
            task2.Wait(cancellationToken)

      stopTcp "cancellation requested" listenSocket
    with
      | :? AggregateException
      | :? OperationCanceledException
      | :? TaskCanceledException ->
        stopTcp "The operation was canceled" listenSocket
      | ex ->
        stopTcp "runtime exception" listenSocket
        )

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

let startTcpIpServer (binding : SocketBinding) (runServer : TcpServer) =

  let acceptingConnections = new AsyncResultCell<StartedData>()
  Globals.numberOfClients := 0
  let startData =
        { startCalledUtc = Globals.utcNow ()
          socketBoundUtc = None
          binding        = binding }

  acceptingConnections.awaitResult()
    , runServer startData acceptingConnections
