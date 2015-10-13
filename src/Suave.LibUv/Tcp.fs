﻿module Suave.LibUv.Tcp

#nowarn "9"

open System
open System.Runtime.InteropServices

open Native

type Handle = IntPtr

let createHandle size : Handle =
  Marshal.AllocCoTaskMem(size)

let destroyHandle (handle : Handle) =
  Marshal.FreeCoTaskMem handle

let checkStatus(s : int) =
    if s < 0 then
      failwith (new string (uv_strerror(s)))

open System.Threading
open System.Threading.Tasks
open System.Collections.Concurrent
open System.Collections.Generic

type SingleThreadSynchronizationContext(loop,runOnThisThreadHandler) =

  let queue = new ConcurrentQueue<KeyValuePair<SendOrPostCallback, obj>>()

  [<DefaultValue>] val mutable uv_handle_cb : uv_handle_cb
 
  member this.Post( d : SendOrPostCallback, state : obj) =
    if (d = null) then
      raise( ArgumentNullException("d"))
    else queue.Enqueue(new KeyValuePair<SendOrPostCallback, obj>(d, state))

  member this.Send() =
    uv_async_send(runOnThisThreadHandler) |> ignore

  member this.RunOnCurrentThread( _ :IntPtr)  =
      let mutable workItem = KeyValuePair(null,null)
      while queue.TryDequeue( &workItem) do
        workItem.Key.Invoke(workItem.Value)

  member this.Init() =
    this.uv_handle_cb <- uv_handle_cb(this.RunOnCurrentThread)
    uv_async_init(loop,runOnThisThreadHandler,this.uv_handle_cb) |> checkStatus

open Suave.Sockets
open Suave.Utils.Async

type ReadOp() =

  [<DefaultValue>] val mutable buf : ArraySegment<byte>
  [<DefaultValue>] val mutable ok : Choice<int,Error> -> unit
  [<DefaultValue>] val mutable uv_alloc_cb : uv_alloc_cb
  [<DefaultValue>] val mutable uv_read_cb : uv_read_cb
  [<DefaultValue>] val mutable pin : GCHandle
  [<DefaultValue>] val mutable pin1 : GCHandle

  member this.end_read ((client : IntPtr), (nread : int), (buff : byref<uv_buf_t>)) =

    if (nread < 0) then
      if (nread <> UV_EOF) then
        this.ok (Choice2Of2 <| ConnectionError (new string(uv_err_name(nread))))
      else
        this.ok (Choice2Of2 <| ConnectionError("eof"))
    else
      let s = uv_read_stop(client)
      if (s < 0) then
        this.ok (Choice2Of2 <| ConnectionError (new string(uv_err_name(s))))
      else
        this.ok (Choice1Of2 nread)

  member this.alloc_buffer ((_ : IntPtr), (suggested_size: int), ([<Out>] buff : byref<uv_buf_t>)) =
    Console.WriteLine "alloc_buffer"
    buff.``base`` <- Marshal.UnsafeAddrOfPinnedArrayElement(this.buf.Array,this.buf.Offset)
    buff.len <- IntPtr(this.buf.Count)

  member this.uv_read_start cn buf ok =
    Console.WriteLine "uv_read_start"
    this.buf <- buf
    this.ok <- ok
    uv_read_start(cn, this.uv_alloc_cb, this.uv_read_cb) |> checkStatus

  member this.Initialize() =
    this.pin <- GCHandle.Alloc(this, GCHandleType.Pinned)
    this.pin1 <- GCHandle.Alloc(this.buf, GCHandleType.Pinned)
    this.uv_alloc_cb <- uv_alloc_cb(fun a b c -> this.alloc_buffer(a, b, &c))
    this.uv_read_cb <- uv_read_cb(fun a b c -> this.end_read (a, b, &c))

type WriteOp() =

  [<DefaultValue>] val mutable wrbuffArray : uv_buf_t[]
  [<DefaultValue>] val mutable uv_write_cb : uv_write_cb
  [<DefaultValue>] val mutable ok : Choice<unit,Error> -> unit

  member this.end_write (request : IntPtr) (status : int) =
    Marshal.FreeCoTaskMem request
    if (status < 0) then 
      let err = new string(uv_strerror(status))
      this.ok (Choice2Of2 <| ConnectionError err)
    else
      this.ok (Choice1Of2())

  member this.Initialize() =
    this.wrbuffArray <- [| new uv_buf_t() |]
    this.uv_write_cb <- uv_write_cb(this.end_write)

  member this.uv_write cn (buf : ArraySegment<byte>) ok =
    this.ok <- ok
    this.wrbuffArray.[0].``base`` <- Marshal.UnsafeAddrOfPinnedArrayElement(buf.Array,buf.Offset)
    this.wrbuffArray.[0].len <- IntPtr(buf.Count)
    let request = Marshal.AllocCoTaskMem(uv_req_size(uv_request_type.UV_WRITE))
    uv_write(request, cn, this.wrbuffArray, 1, this.uv_write_cb ) |> checkStatus

type OperationPair = ReadOp*WriteOp

open Suave.Logging

type LibUvTransport(pool : ConcurrentPool<OperationPair>,loop : IntPtr,client : Handle, synchronizationContext : SingleThreadSynchronizationContext,logger : Logger) =

  [<DefaultValue>] val mutable uv_close_cb : uv_close_cb
  [<DefaultValue>] val mutable pin : GCHandle
  [<DefaultValue>] val mutable cont : unit -> unit

  let (readOp,writeOp) = pool.Pop()
  let intern = Suave.Log.intern logger "Suave.Tcp.job"
 
  member this.uv_close_callback _ =
    destroyHandle client
    pool.Push (readOp,writeOp)
    this.pin.Free()
    this.cont ()

  member this.uv_close cont =
    this.cont <- cont
    uv_close(client, this.uv_close_cb)

  member this.initialize() =
    this.pin <- GCHandle.Alloc(this, GCHandleType.Normal)
    this.uv_close_cb <- uv_close_cb(this.uv_close_callback)

  member this.shutdown() =
    Async.FromContinuations <| fun (ok, _, _) ->
      synchronizationContext.Post(SendOrPostCallback(fun o -> this.uv_close ok),null)
      synchronizationContext.Send()

  interface ITransport with
    member this.read (buf : ByteSegment) =
      Async.FromContinuations <| fun (ok, _, _) ->
        synchronizationContext.Post(SendOrPostCallback(fun o -> readOp.uv_read_start client buf ok),null)
        synchronizationContext.Send()

    member this.write(buf : ByteSegment) =
      Async.FromContinuations <| fun (ok, _, _) ->
      synchronizationContext.Post(SendOrPostCallback(fun o -> writeOp.uv_write client buf ok),null)
      synchronizationContext.Send()

let createLibUvOpsPool maxOps =

  let opsPool = new ConcurrentPool<OperationPair>()

  for x = 0 to maxOps - 1 do

    let readOp = new ReadOp()
    let writeOp = new WriteOp()
    readOp.Initialize()
    writeOp.Initialize()
    opsPool.Push (readOp,writeOp)

  opsPool

open System.Runtime.InteropServices

/// The max backlog of number of requests
[<Literal>]
let MaxBacklog = Int32.MaxValue

let private aFewTimes f =
  let s ms = System.Threading.Thread.Sleep (ms : int)
  let rec run = function
    | 0us -> f ()
    | n -> try f () with e -> s 20000; run (n - 1us)
  run 5us

let bindSocket server bindCallback=
  let r = uv_listen(server, MaxBacklog, bindCallback)
  if r<>0 then
    failwith ("Listen error: " + (new string(uv_strerror(r))))

open Suave.Logging
open Suave.Types
open Suave.Tcp
open Suave

type LibUvSocket(pool : ConcurrentPool<OperationPair>,logger, serveClient, ip, loop , bufferManager, startData, acceptingConnections: AsyncResultCell<StartedData>,synchronizationContextCallback) =

  [<DefaultValue>] val mutable uv_connection_cb : uv_connection_cb
  [<DefaultValue>] val mutable synchronizationContext : SingleThreadSynchronizationContext

  member this.on_new_connection (server : IntPtr) (status: int) =

    if status < 0 then
          "New connection error: " +  (new string (uv_strerror(status))) |> Log.intern logger "Suave.Tcp.LibUvSocket.on_new_connection"
    else

      let client = createHandle <| uv_handle_size(uv_handle_type.UV_TCP)

      uv_tcp_init(loop, client) |> checkStatus

      if (uv_accept(server, client) = 0) then
        let transport = new LibUvTransport(pool,loop,client,this.synchronizationContext,logger)
        transport.initialize()
        Async.Start <| 
            job logger serveClient ip transport bufferManager (transport.shutdown())
      else
        destroyHandle client

  member this.Initialize() =
    this.uv_connection_cb <- uv_connection_cb(this.on_new_connection)
    this.synchronizationContext <- new SingleThreadSynchronizationContext(loop,synchronizationContextCallback)
    this.synchronizationContext.Init()

  member this.run(server) =

    aFewTimes(fun () -> bindSocket server this.uv_connection_cb)

    let startData = { startData with socketBoundUtc = Some (Globals.utcNow()) }
    acceptingConnections.Complete startData |> ignore

    logger.Log LogLevel.Info <| fun _ ->
        { path          = "Suave.Tcp.tcpIpServer"
          trace         = TraceHeader.empty
          message       = "listener started in " + (startData.ToString())
          level         = LogLevel.Info
          ``exception`` = None
          tsUTCTicks    = Globals.utcNow().Ticks }

    uv_run(loop, UV_RUN_DEFAULT) |> checkStatus
    let x = uv_loop_close(loop)
    Log.info logger "LibUvServer.destroyServerCallback" TraceHeader.empty ("uv_loop_close returned: " + x.ToString())
    assert(x = 0)
    Log.info logger "LibUvServer.destroyServerCallback" TraceHeader.empty "<--"

  member this.exit() =
    this.uv_connection_cb <- null

type LibUvServer(maxConcurrentOps,bufferManager,logger : Logger, binding, startData, serveClient, acceptingConnections: AsyncResultCell<StartedData>, event : ManualResetEvent) =

  [<DefaultValue>] val mutable thread : Thread
  [<DefaultValue>] val mutable uv_async_stop_loop_cb : uv_handle_cb

  let mutable addr = sockaddr_in( a = 0L, b= 0L, c = 0L, d = 0L)

  let loop = createHandle <| uv_loop_size()
  let server = createHandle <| uv_handle_size(uv_handle_type.UV_TCP)

  let ip = binding.ip.ToString()
  let port = int binding.port

  let stopLoopCallback = createHandle <| uv_handle_size(uv_handle_type.UV_ASYNC)
  let synchronizationContextCallback = createHandle <| uv_handle_size(uv_handle_type.UV_ASYNC)

  let closeEvent = new ManualResetEvent(false)

  let opsPool = createLibUvOpsPool maxConcurrentOps

  member this.run () =
    Thread.BeginThreadAffinity()
    this.initLoop()
    try
      uv_tcp_init(loop, server) |> checkStatus
      uv_ip4_addr(ip, port, &addr) |> checkStatus
      uv_tcp_bind(server, &addr, 0) |> checkStatus
      let s = new LibUvSocket(opsPool,logger, serveClient, binding.ip, loop, bufferManager, startData, acceptingConnections,synchronizationContextCallback)
      s.Initialize()
      s.run(server)
      s.exit()
    with ex ->
      Log.infoe logger "LibUvServer.run" TraceHeader.empty ex "could not start LibUvSocket"
    destroyHandle loop
    event.Set() |> ignore
    Log.info logger "LibUvServer.run" TraceHeader.empty "exiting server."

  member this.closeHandlerCallback (handle : IntPtr) =
    Marshal.FreeCoTaskMem handle

  member this.closeHandler (handle : IntPtr) (arg : IntPtr) =
    uv_close(handle,uv_close_cb(this.closeHandlerCallback))

  member this.uv_stop_loop (_ : IntPtr) =
    Log.info logger "LibUvServer.uv_stop_loop" TraceHeader.empty "-->"
    this.destroy()
    Log.info logger "LibUvServer.uv_stop_loop" TraceHeader.empty "<--"

  member this.init() = 
    this.thread <- new Thread(this.run)
    this.thread.Name <- "LibUvLoop"
    this.uv_async_stop_loop_cb <- uv_handle_cb(this.uv_stop_loop)

  member this.initLoop () =
    uv_loop_init(loop) |> checkStatus
    uv_async_init(loop, stopLoopCallback, this.uv_async_stop_loop_cb) |> checkStatus

  member this.start() = 
    this.thread.Start()

  member this.stopLoop() =
    Log.info logger "LibUvServer.stopLoop" TraceHeader.empty "-->"
    uv_async_send (stopLoopCallback) |> checkStatus
    closeEvent.WaitOne() |> ignore

    Log.info logger "LibUvServer.stopLoop" TraceHeader.empty "<--"

  member private this.destroyServerCallback _ =
    Log.info logger "LibUvServer.destroyServerCallback" TraceHeader.empty "-->"
    uv_walk(loop,uv_walk_cb(this.closeHandler), IntPtr.Zero)
    destroyHandle server
    Log.info logger "LibUvServer.destroy" TraceHeader.empty "<--"
    closeEvent.Set() |> ignore

  member private this.destroyRunOnThisThreadCallback _ =
    Log.info logger "LibUvServer.destroyRunOnThisThreadCallback" TraceHeader.empty "-->"
    destroyHandle synchronizationContextCallback
    uv_close(server, uv_close_cb(this.destroyServerCallback))
    Log.info logger "LibUvServer.destroyRunOnThisThreadCallback" TraceHeader.empty "<--"

  member private this.destroyLoopCallback _ =
    Log.info logger "LibUvServer.destroyLoopCallback" TraceHeader.empty "-->"
    destroyHandle stopLoopCallback
    uv_close(synchronizationContextCallback, uv_close_cb(this.destroyRunOnThisThreadCallback))
    Log.info logger "LibUvServer.destroyLoopCallback" TraceHeader.empty "<--"

  member this.destroy _ =
    Log.info logger "LibUvServer.destroy" TraceHeader.empty "-->"
    uv_close(stopLoopCallback, uv_close_cb(this.destroyLoopCallback))

let runServerLibUv logger maxConcurrentOps bufferSize (binding: SocketBinding) startData (acceptingConnections: AsyncResultCell<StartedData>) serveClient =
  let bufferManager = new BufferManager(bufferSize * (maxConcurrentOps + 1), bufferSize, logger)
  bufferManager.Init()

  let exitEvent = new ManualResetEvent(false)

  let libUvServer = new LibUvServer(maxConcurrentOps,bufferManager, logger, binding, startData, serveClient, acceptingConnections, exitEvent)

  libUvServer.init()
  async{
    use! disposable = Async.OnCancel(fun () -> libUvServer.stopLoop())
    do libUvServer.start()
    let! _ = Async.AwaitWaitHandle (exitEvent)
    return ()
  }

type LibUvServerFactory() =
  interface TcpServerFactory with
    member this.create (logger,maxOps, bufferSize,binding) =
      runServerLibUv logger maxOps bufferSize binding.socketBinding