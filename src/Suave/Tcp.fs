module Suave.Tcp

open System
open System.Collections.Generic
open System.Threading
open System.Net
open System.Net.Sockets
open Suave.Logging
open Suave.Sockets
open Suave.Utils.Async

/// The max backlog of number of requests
[<Literal>]
let MaxBacklog = Int32.MaxValue

type StartedData =
  { startCalledUtc : DateTimeOffset
    socketBoundUtc : DateTimeOffset option
    binding        : SocketBinding }

  override x.ToString() =
    sprintf "%.3f ms with binding %O:%d"
      ((x.socketBoundUtc |> Option.fold (fun _ t -> t) x.startCalledUtc) - x.startCalledUtc).TotalMilliseconds
      x.binding.ip x.binding.port

/// Disconnect a socket for reuse
let closeSocket (s : Socket) =
  try
    if s <> null then
      if s.Connected || s.IsBound then 
        s.Disconnect true
  with _ -> ()

/// Shoots down a socket for good
let shutdownSocket (s : Socket) =
  try
    if s <> null then
      try
        s.Shutdown(SocketShutdown.Both)
      with _ -> ()
      s.Close ()
      s.Dispose ()
  with _ -> ()

/// Stop the TCP listener server
let stopTcp (logger : Logger) reason (socket : Socket) =
  try
    Log.internf logger "Tcp.stopTcp" (fun fmt -> fmt "stopping tcp server, reason: '%s'" reason)
    socket.Close()
    "stopped tcp server" |> Log.intern logger "Tcp.stopTcp"
  with ex ->
    "failure stopping tcp server" |> Log.interne logger "Tcp.stopTcp" ex

let createPools logger maxOps bufferSize =

  let acceptAsyncArgsPool = new ConcurrentPool<SocketAsyncEventArgs>()
  let readAsyncArgsPool   = new ConcurrentPool<SocketAsyncEventArgs>()
  let writeAsyncArgsPool  = new ConcurrentPool<SocketAsyncEventArgs>()

  let bufferManager = new BufferManager(bufferSize * (maxOps + 1), bufferSize, logger)
  bufferManager.Init()

  for x = 0 to maxOps - 1 do
    //Pre-allocate a set of reusable SocketAsyncEventArgs
    let readEventArg = new SocketAsyncEventArgs()
    let userToken = new AsyncUserToken()
    readEventArg.UserToken <- userToken
    readEventArg.add_Completed(fun a b -> userToken.Continuation b)

    readAsyncArgsPool.Push readEventArg

    let writeEventArg = new SocketAsyncEventArgs()
    let userToken = new AsyncUserToken()
    writeEventArg.UserToken <- userToken
    writeEventArg.add_Completed(fun a b -> userToken.Continuation b)

    writeAsyncArgsPool.Push writeEventArg

    let acceptArg = new SocketAsyncEventArgs()
    let userToken = new AsyncUserToken()
    acceptArg.UserToken <- userToken
    acceptArg.add_Completed(fun a b -> userToken.Continuation b)

    acceptAsyncArgsPool.Push(acceptArg)

  (acceptAsyncArgsPool, readAsyncArgsPool, writeAsyncArgsPool, bufferManager)

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
let job logger
        (serveClient : TcpWorker<unit>)
        binding
        (transport : ITransport)
        (bufferManager : BufferManager) = async {
  let intern = Log.intern logger "Suave.Tcp.job"
  Interlocked.Increment Globals.numberOfClients |> ignore
  intern (binding.ip.ToString() + " connected, total: " + (!Globals.numberOfClients).ToString() + " clients")
  let connection =
    { socketBinding = binding
      transport     = transport
      bufferManager = bufferManager
      lineBuffer    = bufferManager.PopBuffer "Suave.Tcp.job" // buf allocate
      segments      = []
    }
  try
    use! oo = Async.OnCancel (fun () -> intern "disconnected client (async cancel)"
                                        Async.RunSynchronously (transport.shutdown()))
    do! serveClient connection
  with 
    | :? System.IO.EndOfStreamException ->
      intern "disconnected client (end of stream)"
    | ex ->
      logger.Log LogLevel.Warn <| fun _ ->
        LogLine.mk "Suave.Tcp.job"
                  LogLevel.Warn (TraceHeader.empty)
                  (Some ex)
                  "tcp request processing failed"
  bufferManager.FreeBuffer(connection.lineBuffer, "Suave.Tcp.job") // buf free OK
  intern "Shutting down transport."
  do! transport.shutdown()
  Interlocked.Decrement(Globals.numberOfClients) |> ignore
  intern (binding.ip.ToString() + " disconnected, total: " + (!Globals.numberOfClients).ToString() + " clients")
  }

type TcpServer = StartedData -> AsyncResultCell<StartedData> -> TcpWorker<unit> -> Async<unit>

let runServer logger maxConcurrentOps bufferSize (binding: SocketBinding) startData
              (acceptingConnections: AsyncResultCell<StartedData>) serveClient = async {
  try
    let a, b, c, bufferManager = createPools logger maxConcurrentOps bufferSize

    let listenSocket = new Socket(binding.endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
    aFewTimes (fun () -> listenSocket.Bind binding.endpoint)
    listenSocket.Listen MaxBacklog

    use! dd = Async.OnCancel(fun () -> stopTcp logger "tcpIpServer async cancelled" listenSocket)
    let! token = Async.CancellationToken

    let startData = { startData with socketBoundUtc = Some (Globals.utcNow()) }
    acceptingConnections.Complete startData |> ignore

    logger.Log LogLevel.Info <| fun _ ->
      { path          = "Suave.Tcp.tcpIpServer"
        trace         = TraceHeader.empty
        message       = sprintf "listener started in %O%s" startData (if token.IsCancellationRequested then ", cancellation requested" else "")
        level         = LogLevel.Info
        ``exception`` = None
        tsUTCTicks    = Globals.utcNow().Ticks }

    while not (token.IsCancellationRequested) do
      try
        let acceptArgs = a.Pop()
        let! r = accept listenSocket acceptArgs
        match r with
        | Choice1Of2 s ->
          // start a new async worker for each accepted TCP client
          let socket = acceptArgs.AcceptSocket
          let remoteBinding =
            let rep = socket.RemoteEndPoint :?> IPEndPoint
            { ip = rep.Address; port = uint16 rep.Port }
          let transport = new TcpTransport(acceptArgs, a, b, c)
          Async.Start (job logger serveClient remoteBinding transport bufferManager, token)
        | Choice2Of2 e -> failwith "failed to accept."
      with ex -> "failed to accept a client" |> Log.interne logger "Suave.Tcp.tcpIpServer" ex
    return ()
  with ex ->
    "tcp server failed" |> Log.interne logger "Suave.Tcp.tcpIpServer" ex
    return ()
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

  acceptingConnections.AwaitResult()
    , runServer startData acceptingConnections serveClient