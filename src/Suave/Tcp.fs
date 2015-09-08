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

  let acceptAsyncArgsPool = new SocketAsyncEventArgsPool()
  let readAsyncArgsPool   = new SocketAsyncEventArgsPool()
  let writeAsyncArgsPool  = new SocketAsyncEventArgsPool()

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
let job logger (serveClient : TcpWorker<unit>) ipaddr (transport : ITransport) bufferManager shutdownTransport = async {
  let intern = Log.intern logger "Suave.Tcp.tcpIpServer.job"
  ///let socket = acceptArgs.AcceptSocket
  //let ipaddr = (socket.RemoteEndPoint :?> IPEndPoint).Address
  Interlocked.Increment Globals.numberOfClients |> ignore

  Log.internf logger "Suave.Tcp.tcpIpServer.job" (fun fmt -> fmt "%O connected, total: %d clients" ipaddr !Globals.numberOfClients)

  try
      try
      let connection =
        { ipaddr       = ipaddr
          transport    = transport
          bufferManager = bufferManager
          lineBuffer  = bufferManager.PopBuffer "Suave.Tcp.tcpIpServer.job" // buf allocate
          segments     = []
        }
      
      use! oo = Async.OnCancel (fun () -> intern "disconnected client (async cancel)"
                                          shutdownTransport())

      do! serveClient connection
      shutdownTransport()
      bufferManager.FreeBuffer(connection.lineBuffer, "Suave.Tcp.tcpIpServer.job") // buf free OK
      with 
        | :? System.IO.EndOfStreamException ->
      intern "disconnected client (end of stream)"
        | ex ->
        logger.Log LogLevel.Warn <| fun _ ->
          LogLine.mk "Suave.Tcp.tcpIpServer.job"
                      LogLevel.Warn (TraceHeader.empty)
                      (Some ex)
                      "tcp request processing failed"
  finally
      Interlocked.Decrement(Globals.numberOfClients) |> ignore
      Log.internf logger "Suave.Tcp.tcpIpServer.job" (fun fmt -> fmt "%O disconnected, total: %d clients" ipaddr !Globals.numberOfClients)
  }

let runServer logger maxConcurrentOps bufferSize (binding: SocketBinding) startData (acceptingConnections: AsyncResultCell<StartedData>) serveClient = async {
  try
      
    let a, b, c, bufferManager = createPools logger maxConcurrentOps bufferSize

    let listenSocket = new Socket(binding.endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
    aFewTimes (fun () -> listenSocket.Bind binding.endpoint)
    listenSocket.Listen MaxBacklog

    use! dd = Async.OnCancel(fun () -> stopTcp logger "tcpIpServer async cancelled" listenSocket)
    let! token = Async.CancellationToken
      
    let startData = { startData with socketBoundUtc = Some (Globals.utcNow()) }
    acceptingConnections.Complete startData |> ignore

    while not (token.IsCancellationRequested) do
      try
        let acceptArgs = a.Pop()
        let! r = accept listenSocket acceptArgs
        match r with
        | Choice1Of2 s ->
          // start a new async worker for each accepted TCP client
          let socket = acceptArgs.AcceptSocket
          let ipaddr = (socket.RemoteEndPoint :?> IPEndPoint).Address
          let readArgs = b.Pop()
          let writeArgs = c.Pop()
          let transport = { socket = socket; readArgs = readArgs; writeArgs = writeArgs }
          let shutdownTransport _ =
            shutdownSocket socket
            acceptArgs.AcceptSocket <- null
            a.Push acceptArgs
            b.Push readArgs
            c.Push writeArgs
          Async.Start (job logger serveClient ipaddr transport bufferManager shutdownTransport, token)
        | Choice2Of2 e -> failwith "failed to accept."
      with ex -> "failed to accept a client" |> Log.interne logger "Suave.Tcp.tcpIpServer" ex
    return ()
  with ex ->
    "tcp server failed" |> Log.interne logger "Suave.Tcp.tcpIpServer" ex
    return ()
}

open Native.LibUv
open System.Runtime.InteropServices

let runServerLibUv logger maxConcurrentOps bufferSize (binding: SocketBinding) startData (acceptingConnections: AsyncResultCell<StartedData>) serveClient =

  let bufferManager = new BufferManager(bufferSize * (maxConcurrentOps + 1), bufferSize, logger)
  bufferManager.Init()

  let loop = uv_default_loop()

  let on_new_connection token (server : IntPtr) (status: int) =
    if status < 0 then
          printfn "New connection error: %s" (new string (uv_strerror(status)))
    else
      let client = Marshal.AllocHGlobal(uv_handle_size(uv_handle_type.UV_STREAM))
      let _ = uv_tcp_init(loop, client)
      if (uv_accept(server, client) = 0) then
        let transport = new LibUvTransport(client)
        let shutdownTransport _ =
          uv_close (client,null)
        Async.Start (job logger serveClient binding.ip transport bufferManager shutdownTransport, token)
      else
        uv_close(client, null)

  let run token =
  
    let mutable server = Marshal.AllocHGlobal(sizeof<int>)
    let c = uv_tcp_init(loop, server)

    let mutable addr = sockaddr_in( a = 0, b = 0, c = 0, d = 0)
    let a = uv_ip4_addr(binding.ip.ToString(), int binding.port, &addr)

    let _ = uv_tcp_bind(server, &addr, 0)
  
    let startData = { startData with socketBoundUtc = Some (Globals.utcNow()) }
    acceptingConnections.Complete startData |> ignore
    let r = uv_listen(server, MaxBacklog, uv_connection_cb(on_new_connection token))
    if r<>0 then
      printfn "Listen error: %s" (new string(uv_strerror(r)))
    else
      uv_run(loop, UV_RUN_DEFAULT)      

  async{
    let! token = Async.CancellationToken
    do run token
  }

/// Start a new TCP server with a specific IP, Port and with a serve_client worker
/// returning an async workflow whose result can be awaited (for when the tcp server has started
/// listening to its address/port combination), and an asynchronous workflow that
/// yields when the full server is cancelled. If the 'has started listening' workflow
/// returns None, then the start timeout expired.
let startTcpIpServerAsync (bufferSize  : int, maxConcurrentOps : int)
                          (logger      : Logger)
                          (serveClient : TcpWorker<unit>)
                          (binding     : SocketBinding) =

  let acceptingConnections = new AsyncResultCell<StartedData>()

  let startData =
        { startCalledUtc = Globals.utcNow ()
          socketBoundUtc = None
          binding        = binding }
  async{ 
    let! r = acceptingConnections.AwaitResult()
    logger.Log LogLevel.Info <| fun _ ->
        { path          = "Suave.Tcp.tcpIpServer"
          trace         = TraceHeader.empty
          message       = match r with | Some startData -> sprintf "listener started in %O." startData | None -> "listener did not started."
          level         = LogLevel.Info
          ``exception`` = None
          tsUTCTicks    = Globals.utcNow().Ticks }
    return r }, runServer logger maxConcurrentOps bufferSize binding startData acceptingConnections serveClient