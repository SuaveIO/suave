module Suave.Tcp

open System
open System.Collections.Generic
open System.Threading
open System.Net
open System.Net.Sockets

/// The max backlog of number of requests
let MAX_BACK_LOG = Int32.MaxValue

type StartedData =
  { start_called_utc : DateTimeOffset
    socket_bound_utc : DateTimeOffset option
    source_ip        : IPAddress
    source_port      : uint16 }
with
  override x.ToString() =
    sprintf "%.3f ms with binding %O:%d"
      ((x.socket_bound_utc |> Option.fold (fun _ t -> t) x.start_called_utc) - x.start_called_utc).TotalMilliseconds
      x.source_ip x.source_port

open Socket 

/// Disconnect a socket for reuse
let close_socket (s : Socket) =
  try
    if s <> null then
      if s.Connected || s.IsBound then 
        s.Disconnect(true)
  with _ -> ()

/// Shoots down a socket for good
let shutdown_socket (s : Socket) =
  try
    if s <> null then
      try
        s.Shutdown(SocketShutdown.Both)
      with _ -> ()
      s.Close ()
      s.Dispose ()
  with _ -> ()

/// Stop the TCP listener server
let stop_tcp (logger : Log.Logger) reason (socket : Socket) =
  try
    Log.internf logger "Tcp.stop_tcp" (fun fmt -> fmt "stopping tcp server, reason: '%s'" reason)
    socket.Close()
    "stopped tcp server" |> Log.intern logger "Tcp.stop_tcp"
  with ex ->
    "failure stopping tcp server" |> Log.interne logger "Tcp.stop_tcp" ex

let create_pools logger max_ops buffer_size =

  let acceptAsyncArgsPool = new SocketAsyncEventArgsPool()
  let readAsyncArgsPool   = new SocketAsyncEventArgsPool()
  let writeAsyncArgsPool  = new SocketAsyncEventArgsPool()

  let bufferManager = new BufferManager(buffer_size * (max_ops + 1), buffer_size, logger)
  bufferManager.Init()

  for x = 0 to max_ops - 1 do
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

    let accept_arg = new SocketAsyncEventArgs()
    let userToken =  new AsyncUserToken()
    accept_arg.UserToken <- userToken
    accept_arg.add_Completed(fun a b -> userToken.Continuation b)

    acceptAsyncArgsPool.Push(accept_arg)

  (acceptAsyncArgsPool, readAsyncArgsPool, writeAsyncArgsPool, bufferManager)

// NOTE: performance tip, on mono set nursery-size with a value larger than MAX_CONCURRENT_OPS * BUFFER_SIZE
// i.e: export MONO_GC_PARAMS=nursery-size=128m
// The nursery size must be a power of two in bytes

/// Argh!! @ System.Net.Sockets.SocketException: Address already in use
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
let tcp_ip_server (source_ip : IPAddress,
                   source_port : uint16,
                   buffer_size : int,
                   max_concurrent_ops : int)
                  (logger : Log.Logger)
                  (serve_client : TcpWorker<unit>) =

  let start_data =
    { start_called_utc = Globals.utc_now ()
      socket_bound_utc = None
      source_ip        = source_ip
      source_port      = source_port }

  let accepting_connections = new AsyncResultCell<StartedData>()
  let a, b, c, bufferManager = create_pools logger max_concurrent_ops buffer_size

  let local_ep = new IPEndPoint(source_ip, int source_port)
  let listen_socket = new Socket(local_ep.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
  a_few_times (fun () -> listen_socket.Bind local_ep)
  listen_socket.Listen MAX_BACK_LOG

  // consider:
  // echo 5 > /proc/sys/net/ipv4/tcp_fin_timeout
  // echo 1 > /proc/sys/net/ipv4/tcp_tw_recycle
  // custom kernel with shorter TCP_TIMEWAIT_LEN in include/net/tcp.h
  let inline job (accept_args : A) = async {
    let intern  = Log.intern logger "Suave.Tcp.tcp_ip_server.job"
    let socket = accept_args.AcceptSocket
    let ip_address = (socket.RemoteEndPoint :?> IPEndPoint).Address
    Interlocked.Increment Globals.number_of_clients |> ignore

    Log.internf logger "Suave.Tcp.tcp_ip_server.job" (fun fmt -> fmt "%O connected, total: %d clients" ip_address !Globals.number_of_clients)

    try
      let read_args = b.Pop()
      let write_args = c.Pop()
      let connection =
        { ipaddr       = ip_address
          transport    = { socket = socket; read_args = read_args; write_args = write_args}
          buffer_manager = bufferManager
          line_buffer  = bufferManager.PopBuffer "Suave.Tcp.tcp_ip_server.job" // buf allocate
          segments     = []
        }
      use! oo = Async.OnCancel (fun () -> intern "disconnected client (async cancel)"
                                          shutdown_socket socket)

      let! _ = serve_client connection
      shutdown_socket socket
      accept_args.AcceptSocket <- null
      a.Push accept_args
      b.Push read_args
      c.Push write_args
      bufferManager.FreeBuffer(connection.line_buffer, "Suave.Tcp.tcp_ip_server.job") // buf free OK
      Interlocked.Decrement(Globals.number_of_clients) |> ignore
      Log.internf logger "Suave.Tcp.tcp_ip_server.job" (fun fmt -> fmt "%O disconnected, total: %d clients" ip_address !Globals.number_of_clients)
    with 
    | :? System.IO.EndOfStreamException ->
      intern "disconnected client (end of stream)"
    | ex -> "tcp request processing failed" |> Log.interne logger "Suave.Tcp.tcp_ip_server.job" ex
  }

  // start a new async worker for each accepted TCP client
  accepting_connections.AwaitResult(), async {
    try
      use! dd = Async.OnCancel(fun () -> stop_tcp logger "tcp_ip_server async cancelled" listen_socket)
      let! (token : Threading.CancellationToken) = Async.CancellationToken

      let start_data = { start_data with socket_bound_utc = Some(Globals.utc_now()) }
      accepting_connections.Complete start_data |> ignore

      logger.Log Log.LogLevel.Info <| fun _ ->
        { path          = "Suave.Tcp.tcp_ip_server"
          trace         = Log.TraceHeader.empty
          message       = sprintf "listener started in %O%s" start_data (if token.IsCancellationRequested then ", cancellation requested" else "")
          level         = Log.LogLevel.Info
          ``exception`` = None
          ts_utc_ticks  = Globals.utc_now().Ticks }

      while not (token.IsCancellationRequested) do
        try
          let accept_args = a.Pop()
          let! _ = accept listen_socket accept_args
          Async.Start (job accept_args, token)
        with ex -> "failed to accept a client" |> Log.interne logger "Suave.Tcp.tcp_ip_server" ex
      return ()
    with ex ->
      "tcp server failed" |> Log.interne logger "Suave.Tcp.tcp_ip_server" ex
      return ()
  }
