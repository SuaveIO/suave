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
        s.Disconnect(true)
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
    Log.internf logger "Tcp.stop_tcp" (fun fmt -> fmt "stopping tcp server, reason: '%s'" reason)
    socket.Close()
    "stopped tcp server" |> Log.intern logger "Tcp.stop_tcp"
  with ex ->
    "failure stopping tcp server" |> Log.interne logger "Tcp.stop_tcp" ex

let createPools logger maxOps bufferSize =

  let acceptAsyncArgsPool = new SocketAsyncEventArgsPool()
  let readAsyncArgsPool   = new SocketAsyncEventArgsPool()
  let writeAsyncArgsPool  = new SocketAsyncEventArgsPool()

  let bufferManager = new BufferManager(bufferSize * (maxOps + 1), bufferSize, logger)
  bufferManager.Init()

  for x = 0 to maxOps - 1 do
    //Pre-allocate a set of reusable SocketAsyncEventArgs
    let readEventArg = new SocketAsyncEventArgs()
    let userToken =  new AsyncUserToken()
    readEventArg.UserToken <- userToken
    readEventArg.add_Completed(fun a b -> userToken.Continuation b)

    readAsyncArgsPool.Push readEventArg

    let writeEventArg = new SocketAsyncEventArgs()
    let userToken =  new AsyncUserToken()
    writeEventArg.UserToken <- userToken
    writeEventArg.add_Completed(fun a b -> userToken.Continuation b)

    writeAsyncArgsPool.Push writeEventArg

    let acceptArg = new SocketAsyncEventArgs()
    let userToken =  new AsyncUserToken()
    acceptArg.UserToken <- userToken
    acceptArg.add_Completed(fun a b -> userToken.Continuation b)

    acceptAsyncArgsPool.Push(acceptArg)

  (acceptAsyncArgsPool, readAsyncArgsPool, writeAsyncArgsPool, bufferManager)

// NOTE: performance tip, on mono set nursery-size with a value larger than MAX_CONCURRENT_OPS * BUFFER_SIZE
// i.e: export MONO_GC_PARAMS=nursery-size=128m
// The nursery size must be a power of two in bytes

let private a_few_times f =
  let s ms = System.Threading.Thread.Sleep (ms : int)
  let rec run = function
    | 0us | 1us -> f ()
    | n -> try f () with e -> s 10; run (n - 1us)
  run 3us

/// Start a new TCP server with a specific IP, Port and with a serve_client worker
/// returning an async workflow whose result can be awaited (for when the tcp server has started
/// listening to its address/port combination), and an asynchronous workflow that
/// yields when the full server is cancelled. If the 'has started listening' workflow
/// returns None, then the start timeout expired.
let startTcpIpServerAsync 
       (bufferSize        : int, 
        maxConcurrentOps : int) 
       (logger             : Logger)
       (serveClient        : TcpWorker<unit>)
       (binding            : SocketBinding) =

  let startData =
    { startCalledUtc = Globals.utcNow ()
      socketBoundUtc = None
      binding        = binding }

  let acceptingConnections = new AsyncResultCell<StartedData>()
  let a, b, c, bufferManager = createPools logger maxConcurrentOps bufferSize

  let listenSocket = new Socket(binding.endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
  a_few_times (fun () -> listenSocket.Bind binding.endpoint)
  listenSocket.Listen MaxBacklog

  // consider:
  // echo 5 > /proc/sys/net/ipv4/tcp_fin_timeout
  // echo 1 > /proc/sys/net/ipv4/tcp_tw_recycle
  // custom kernel with shorter TCP_TIMEWAIT_LEN in include/net/tcp.h
  let job (acceptArgs : SocketAsyncEventArgs) = async {
    let intern  = Log.intern logger "Suave.Tcp.tcp_ip_server.job"
    let socket = acceptArgs.AcceptSocket
    let ipaddr = (socket.RemoteEndPoint :?> IPEndPoint).Address
    Interlocked.Increment Globals.numberOfClients |> ignore

    Log.internf logger "Suave.Tcp.tcp_ip_server.job" (fun fmt -> fmt "%O connected, total: %d clients" ipaddr !Globals.numberOfClients)

    try
      let readArgs = b.Pop()
      let writeArgs = c.Pop()
      let connection =
        { ipaddr       = ipaddr
          transport    = { socket = socket; readArgs = readArgs; writeArgs = writeArgs}
          bufferManager = bufferManager
          lineBuffer  = bufferManager.PopBuffer "Suave.Tcp.tcp_ip_server.job" // buf allocate
          segments     = []
        }
      use! oo = Async.OnCancel (fun () -> intern "disconnected client (async cancel)"
                                          shutdownSocket socket)

      let! _ = serveClient connection
      shutdownSocket socket
      acceptArgs.AcceptSocket <- null
      a.Push acceptArgs
      b.Push readArgs
      c.Push writeArgs
      bufferManager.FreeBuffer(connection.lineBuffer, "Suave.Tcp.tcp_ip_server.job") // buf free OK
      Interlocked.Decrement(Globals.numberOfClients) |> ignore
      Log.internf logger "Suave.Tcp.tcp_ip_server.job" (fun fmt -> fmt "%O disconnected, total: %d clients" ipaddr !Globals.numberOfClients)
    with 
    | :? System.IO.EndOfStreamException ->
      intern "disconnected client (end of stream)"
    | ex -> "tcp request processing failed" |> Log.interne logger "Suave.Tcp.tcp_ip_server.job" ex
  }

  // start a new async worker for each accepted TCP client
  acceptingConnections.AwaitResult(), async {
    try
      use! dd = Async.OnCancel(fun () -> stopTcp logger "tcp_ip_server async cancelled" listenSocket)
      let! token = Async.CancellationToken

      let startData = { startData with socketBoundUtc = Some(Globals.utcNow()) }
      acceptingConnections.Complete startData |> ignore

      logger.Log LogLevel.Info <| fun _ ->
        { path          = "Suave.Tcp.tcp_ip_server"
          trace         = TraceHeader.empty
          message       = sprintf "listener started in %O%s" startData (if token.IsCancellationRequested then ", cancellation requested" else "")
          level         = LogLevel.Info
          ``exception`` = None
          ts_utc_ticks  = Globals.utcNow().Ticks }

      while not (token.IsCancellationRequested) do
        try
          let acceptArgs = a.Pop()
          let! _ = accept listenSocket acceptArgs
          Async.Start (job acceptArgs, token)
        with ex -> "failed to accept a client" |> Log.interne logger "Suave.Tcp.tcp_ip_server" ex
      return ()
    with ex ->
      "tcp server failed" |> Log.interne logger "Suave.Tcp.tcp_ip_server" ex
      return ()
  }

[<Obsolete("Renamed to closeSocket")>]
/// Obsolete
let close_socket s = closeSocket s
[<Obsolete("Renamed to shutdownSocket")>]
/// Obsolete
let shutdown_socket s = shutdownSocket s
[<Obsolete("Renamed to stopTcp")>]
/// Obsolete
let stop_tcp logger reason socket = stopTcp logger reason socket
[<Obsolete("Renamed to createPools")>]
/// Obsolete
let create_pools  logger maxOps bufferSize = createPools logger maxOps bufferSize
[<Obsolete("Renamed to startTcpIpServerAsync")>]
/// Obsolete
let tcp_ip_server  (bufferSize, maxConcurrentOps) logger serveClient binding = startTcpIpServerAsync (bufferSize, maxConcurrentOps) logger serveClient binding
