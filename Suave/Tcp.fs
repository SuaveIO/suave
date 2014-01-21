module Suave.Tcp

open System
open System.Collections.Generic
open System.Threading
open System.Net
open System.Net.Sockets

/// The max backlog of number of requests
let MAX_BACK_LOG = Int32.MaxValue

type StartedData =
  { start_called_utc : DateTime
  ; socket_bound_utc : DateTime option
  ; source_ip        : IPAddress
  ; source_port      : uint16 }
  override x.ToString() =
    sprintf "started %s <-> %s : %O:%d"
      (x.start_called_utc.ToString("o"))
      (x.socket_bound_utc |> Option.fold (fun _ t -> t.ToString("o")) "x")
      x.source_ip x.source_port

/// Asynchronous extension methods to TcpListener to make
/// it nicer to consume in F#
type TcpListener with
  member x.AsyncAcceptTcpClient() =
    Async.FromBeginEnd(x.BeginAcceptTcpClient, x.EndAcceptTcpClient)

open Socket 

/// A TCP Worker is a thing that takes a TCP client and returns an asynchronous workflow thereof
type TcpWorker<'a> = Connection -> Async<'a>

/// Close the TCP client by closing its stream and then closing the client itself
let inline close (d : Connection) = try d.shutdown() with _ -> ()

/// Stop the TCP listener server
let stop_tcp reason (socket : Socket) =
  Log.tracef(fun fmt -> fmt "tcp:stop_tcp - %s - stopping server .. " reason)
  socket.Close()
  Log.trace(fun () -> "tcp:stop_tcp - stopped")

let create_pools max_ops buffer_size =

  let acceptAsyncArgsPool = new SocketAsyncEventArgsPool()
  let readAsyncArgsPool   = new SocketAsyncEventArgsPool()
  let writeAsyncArgsPool  = new SocketAsyncEventArgsPool()

  let bufferManager = new BufferManager(buffer_size * (max_ops + 1) ,buffer_size)
  bufferManager.Init()

  [| 0 .. max_ops - 1|] 
  |> Array.iter (fun x ->
    //Pre-allocate a set of reusable SocketAsyncEventArgs
    let readEventArg = new SocketAsyncEventArgs()
    let userToken =  new AsyncUserToken()
    readEventArg.UserToken <- userToken
    readEventArg.add_Completed(fun a b -> userToken.Continuation b)

    //bufferManager.SetBuffer(readEventArg) |> ignore
    readAsyncArgsPool.Push(readEventArg)

    let writeEventArg = new SocketAsyncEventArgs()
    let userToken =  new AsyncUserToken()
    writeEventArg.UserToken <- userToken
    writeEventArg.add_Completed(fun a b -> userToken.Continuation b)

    writeAsyncArgsPool.Push(writeEventArg)

    let acceptEventArg = new SocketAsyncEventArgs()
    let userToken =  new AsyncUserToken()
    acceptEventArg.UserToken <- userToken
    acceptEventArg.add_Completed(fun a b -> userToken.Continuation b)
            
    acceptAsyncArgsPool.Push(acceptEventArg)

    )
  (acceptAsyncArgsPool, readAsyncArgsPool, writeAsyncArgsPool, bufferManager)

// NOTE: performance tip, on mono set nursery-size with a value larger than MAX_CONCURRENT_OPS * BUFFER_SIZE
// i.e: export MONO_GC_PARAMS=nursery-size=128m
// The nursery size must be a power of two in bytes

let inline receive (socket: Socket) (args : A)  (buf: B) = 
  async_do socket.ReceiveAsync (set_buffer buf)  (fun a -> a.BytesTransferred) args
 
let inline send (socket: Socket) (args : A) (buf: B) =
  async_do socket.SendAsync (set_buffer buf) ignore args

/// Start a new TCP server with a specific IP, Port and with a serve_client worker
/// returning an async workflow whose result can be awaited (for when the tcp server has started
/// listening to its address/port combination), and an asynchronous workflow that
/// yields when the full server is cancelled. If the 'has started listening' workflow
/// returns None, then the start timeout expired.
let tcp_ip_server (source_ip : IPAddress, source_port : uint16, buffer_size : int, mac_concurrent_ops : int) (serve_client : TcpWorker<unit>) =
  let start_data =
    { start_called_utc = DateTime.UtcNow
    ; socket_bound_utc = None
    ; source_ip        = source_ip
    ; source_port      = source_port }
  let accepting_connections = new AsyncResultCell<StartedData>()
  //log "tcp:tcp_ip_server - starting listener: %O" start_data

  let (a,b,c,bufferManager) = create_pools mac_concurrent_ops buffer_size

  let localEndPoint = new IPEndPoint(source_ip,int source_port)
  let listenSocket = new Socket(localEndPoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
  listenSocket.Bind(localEndPoint);
  listenSocket.Listen(MAX_BACK_LOG)

  //consider:
  //echo 5 > /proc/sys/net/ipv4/tcp_fin_timeout
  //echo 1 > /proc/sys/net/ipv4/tcp_tw_recycle
  //custom kernel with shorter TCP_TIMEWAIT_LEN in include/net/tcp.h
  let inline job (accept_args:A) (read_args : A) (write_args : A) (d : Connection) = async {
    use! oo = Async.OnCancel (fun () -> Log.trace(fun () -> "tcp:tcp_ip_server - disconnected client (async cancel)")
                                        close d)
    try
      try
        return! serve_client d
      with 
        | :? System.IO.EndOfStreamException ->
          Log.trace(fun () -> "tcp:tcp_ip_server - disconnected client (end of stream)")
          return ()
        | x ->
          Log.tracef(fun fmt -> fmt "tcp:tcp_ip_server - tcp request processing failed.\n%A" x)
          return ()
    finally
      if accept_args.AcceptSocket <> null then
        if(accept_args.AcceptSocket.Connected || accept_args.AcceptSocket.IsBound) then accept_args.AcceptSocket.Disconnect(true)

      a.Push(accept_args)
      b.Push(read_args)
      c.Push(write_args)

      close d
  }

  // start a new async worker for each accepted TCP client
  accepting_connections.AwaitResult(), async {
    try
      use! dd = Async.OnCancel(fun () -> stop_tcp "tcp_ip_server async cancelled" listenSocket)
      let! (token : Threading.CancellationToken) = Async.CancellationToken

      let start_data = { start_data with socket_bound_utc = Some(DateTime.UtcNow) }
      accepting_connections.Complete start_data |> ignore

      Log.tracef(fun fmt -> fmt "tcp:tcp_ip_server - started listener: %O%s" start_data
                              (if token.IsCancellationRequested then ", cancellation requested" else ""))

      while not (token.IsCancellationRequested) do
        let accept_args = a.Pop()
        let read_args = b.Pop()
        let write_args = c.Pop()
        let! (socket : Socket) = accept listenSocket accept_args
        let client = { 
          ipaddr =  (socket.RemoteEndPoint :?> IPEndPoint).Address;
          read  = receive socket read_args;
          write = send socket write_args;
          get_buffer = bufferManager.PopBuffer;
          free_buffer = bufferManager.FreeBuffer;
          shutdown = fun () -> if socket <> null && socket.Connected then socket.Shutdown(SocketShutdown.Both)
          }
        Async.Start (job accept_args read_args write_args client, token)

      return ()
    with x ->
      Log.tracef(fun fmt -> fmt "tcp:tcp_ip_server - tcp server failed.\n%A" x)
      return ()
  }

/// Get the stream from the TCP client
let inline stream (client : TcpClient) = client.GetStream()

open System.IO

/// Mirror the stream byte-by-byte, one byte at a time
let mirror (client_stream : Stream) (server_stream : Stream) = async {
  try
  while true do
    let! onebyte = client_stream.AsyncRead(1)
    do! server_stream.AsyncWrite onebyte
  with _ -> return ()
}
