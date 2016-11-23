module Suave.LibUv.Tcp

#nowarn "9"

open System
open System.Net.Sockets
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

type SingleThreadSynchronizationContext(loop, runOnThisThreadHandler) =

  let queue = new ConcurrentQueue<KeyValuePair<SendOrPostCallback, obj>>()

  [<DefaultValue>] val mutable running : bool
  [<DefaultValue>] val mutable uv_handle_cb : uv_handle_cb

  member this.Post(d : SendOrPostCallback, state : obj) =
    if (d = null) then
      raise( ArgumentNullException("d"))
    else queue.Enqueue(new KeyValuePair<SendOrPostCallback, obj>(d, state))

  member this.Send() =
    if this.running then
      uv_async_send(runOnThisThreadHandler) |> checkStatus

  member this.runOnCurrentThread( _ :IntPtr)  =
      let mutable workItem = KeyValuePair(null,null)
      while queue.TryDequeue( &workItem) do
        workItem.Key.Invoke(workItem.Value)

  member this.init() =
    this.uv_handle_cb <- uv_handle_cb(this.runOnCurrentThread)
    this.running <- true
    uv_async_init(loop, runOnThisThreadHandler, this.uv_handle_cb) |> checkStatus

open Suave.Sockets
open Suave.Utils

let isWinNT = Environment.OSVersion.Platform = PlatformID.Win32NT

let fork f = ThreadPool.QueueUserWorkItem(WaitCallback(f)) |> ignore

type ReadOp() =

  [<DefaultValue>] val mutable buf : ArraySegment<byte>
  [<DefaultValue>] val mutable ok : Choice<int,Error> -> unit
  [<DefaultValue>] val mutable uv_alloc_cb : uv_alloc_cb
  [<DefaultValue>] val mutable uv_read_cb : uv_read_cb

  member this.readEnd (client : IntPtr, nread : int, buff : byref<uv_buf_t>) =

    if (nread < 0) then
      if (nread <> UV_EOF) then
        fork(fun _ -> this.ok (Choice2Of2 <| ConnectionError (new string(uv_err_name(nread)))))
      else
        fork(fun _ -> this.ok (Choice2Of2 <| ConnectionError("eof")))
    else
      let s = uv_read_stop(client)
      if (s < 0) then
        fork(fun _ -> this.ok (Choice2Of2 <| ConnectionError (new string(uv_err_name(s)))))
      else
        fork(fun _ -> this.ok (Choice1Of2 nread))

  member this.allocBuffer (_ : IntPtr, suggested_size : int, [<Out>] buff : byref<uv_buf_t>) =
    if isWinNT then
      buff.``base`` <- IntPtr(this.buf.Count)
      buff.len <- Marshal.UnsafeAddrOfPinnedArrayElement(this.buf.Array,this.buf.Offset)
    else
      buff.``base`` <- Marshal.UnsafeAddrOfPinnedArrayElement(this.buf.Array,this.buf.Offset)
      buff.len <- IntPtr(this.buf.Count)

  member this.readStart cn buf ok =
    this.buf <- buf
    this.ok <- ok
    uv_read_start(cn, this.uv_alloc_cb, this.uv_read_cb) |> checkStatus

  member this.initialize() =
    this.uv_alloc_cb <- uv_alloc_cb(fun a b c -> this.allocBuffer(a, b, &c))
    this.uv_read_cb <- uv_read_cb(fun a b c -> this.readEnd (a, b, &c))

type WriteOp() =

  [<DefaultValue>] val mutable wrbuffArray : uv_buf_t[]
  [<DefaultValue>] val mutable uv_write_cb : uv_write_cb
  [<DefaultValue>] val mutable ok : Choice<unit,Error> -> unit

  member this.writeEnd (request : IntPtr) (status : int) =
    Marshal.FreeCoTaskMem request
    if (status < 0) then
      let err = new string(uv_strerror(status))
      fork(fun _ -> this.ok (Choice2Of2 <| ConnectionError err))
    else
      fork(fun _ -> this.ok (Choice1Of2()))

  member this.initialize() =
    this.wrbuffArray <- [| new uv_buf_t() |]
    this.uv_write_cb <- uv_write_cb(this.writeEnd)

  member this.writeStart cn (buf : ArraySegment<byte>) ok =
    this.ok <- ok
    if isWinNT then
      this.wrbuffArray.[0].``base`` <- IntPtr(buf.Count)
      this.wrbuffArray.[0].len <- Marshal.UnsafeAddrOfPinnedArrayElement(buf.Array,buf.Offset)
    else
      this.wrbuffArray.[0].``base`` <- Marshal.UnsafeAddrOfPinnedArrayElement(buf.Array,buf.Offset)
      this.wrbuffArray.[0].len <- IntPtr(buf.Count)
    let request = Marshal.AllocCoTaskMem(uv_req_size(uv_request_type.UV_WRITE))
    uv_write(request, cn, this.wrbuffArray, 1, this.uv_write_cb ) |> checkStatus

type OperationPair = ReadOp*WriteOp

open Suave.Logging

type LibUvTransport(pool : ConcurrentPool<OperationPair>,
                    loop : IntPtr,
                    client : Handle,
                    synchronizationContext : SingleThreadSynchronizationContext,
                    logger : Logger) =

  [<DefaultValue>] val mutable uv_close_cb : uv_close_cb
  [<DefaultValue>] val mutable cont : unit -> unit
  [<DefaultValue>] val mutable pin : GCHandle

  let (readOp,writeOp) = pool.Pop()

  member this.closeCallback _ =
    destroyHandle client
    pool.Push (readOp,writeOp)
    this.cont ()

  member this.close cont =
    this.cont <- cont
    uv_close(client, this.uv_close_cb)

  member this.initialize() =
    this.uv_close_cb <- uv_close_cb(this.closeCallback)
    this.pin <- GCHandle.Alloc(this)

  member this.shutdown () =
    Async.FromContinuations <| fun (ok, _, _) ->
      synchronizationContext.Post(SendOrPostCallback(fun o -> this.close ok),null)
      synchronizationContext.Send()

  interface ITransport with
    member this.read (buf : ByteSegment) =
      Async.FromContinuations <| fun (ok, _, _) ->
        synchronizationContext.Post(SendOrPostCallback(fun o -> readOp.readStart client buf ok),null)
        synchronizationContext.Send()

    member this.write(buf : ByteSegment) =
      Async.FromContinuations <| fun (ok, _, _) ->
        synchronizationContext.Post(SendOrPostCallback(fun o -> writeOp.writeStart client buf ok),null)
        synchronizationContext.Send()

    member this.shutdown() = async{
      do! this.shutdown()
      do this.pin.Free()
      }

let createPair _ =
  let readOp = new ReadOp()
  let writeOp = new WriteOp()
  readOp.initialize()
  writeOp.initialize()
  (readOp,writeOp)

let createLibUvOpsPool maxOps =

  let opsPool = new ConcurrentPool<OperationPair>()
  opsPool.ObjectGenerator <- fun _ -> createPair()

  for x = 0 to maxOps - 1 do
    opsPool.Push (createPair())

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
open Suave.Logging.Message
open Suave.Http
open Suave.Tcp
open Suave

type LibUvSocket(pool : ConcurrentPool<OperationPair>,
                 logger : Logger,
                 serveClient,
                 ip,
                 loop,
                 bufferManager,
                 startData,
                 acceptingConnections: AsyncResultCell<StartedData>,
                 synchronizationContext) =

  [<DefaultValue>] val mutable uv_connection_cb : uv_connection_cb

  member this.onNewConnection (server : IntPtr) (status: int) =

    if status < 0 then
      logger.verbose (
        eventX "New connection {error}"
        >> setSingleName "Suave.LibUv.Tcp.LibUvSocket.onNewConnection"
        >> setFieldValue "error" (new string (uv_strerror(status))))
    else
      try
        let client = createHandle <| uv_handle_size(uv_handle_type.UV_TCP)

        uv_tcp_init(loop, client) |> checkStatus



        if (uv_accept(server, client) = 0) then
          let transport = new LibUvTransport(pool,loop,client,synchronizationContext,logger)
          transport.initialize()
          Async.Start (job serveClient ip transport bufferManager)
        else
          destroyHandle client
      with ex ->
        logger.info (
          eventX "onNewConnection failed with:"
          >> addExn ex
          >> setSingleName "Suave.LibUv.Tcp.LibUvSocket.onNewConnection")

  member this.initialize() =
    this.uv_connection_cb <- uv_connection_cb(this.onNewConnection)

  member this.run(server) =
    let event message =
      eventX message >> setSingleName "Suave.LibUv.Tcp.LibUvSocket.run"

    aFewTimes (fun () -> bindSocket server this.uv_connection_cb)

    let startData = { startData with socketBoundUtc = Some (Globals.utcNow()) }
    acceptingConnections.complete startData |> ignore

    logger.info (
      event "Listener started in {startedListeningMilliseconds:#.###} with binding {ipAddress}:{port}"
      >> setFieldValue "startedListeningMilliseconds" (startData.GetStartedListeningElapsedMilliseconds())
      >> setFieldValue "ipAddress" startData.binding.ip
      >> setFieldValue "port" startData.binding.port)

    uv_run(loop, UV_RUN_DEFAULT) |> checkStatus
    let x = uv_loop_close(loop)
    logger.verbose (event "uv_loop_close returned {returnValue}"
                    >> setFieldValue "returnValue" x)
    assert(x = 0)
    logger.info (event "<--")

  member this.exit() =
    this.uv_connection_cb <- null

type LibUvServer(maxConcurrentOps, bufferManager,
                 binding,
                 startData, serveClient,
                 acceptingConnections: AsyncResultCell<StartedData>,
                 event : ManualResetEvent) =

  static let logger = Log.create "Suave.LibUv.Tcp.LibUvServer"

  [<DefaultValue>] val mutable thread : Thread
  [<DefaultValue>] val mutable synchronizationContext : SingleThreadSynchronizationContext
  [<DefaultValue>] val mutable uv_async_stop_loop_cb : uv_handle_cb
  [<DefaultValue>] val mutable uv_close_cb_destroy : uv_close_cb
  [<DefaultValue>] val mutable uv_close_cb_thread  : uv_close_cb
  [<DefaultValue>] val mutable uv_close_cb_loop    : uv_close_cb
  [<DefaultValue>] val mutable uv_close_cb_handler : uv_close_cb
  [<DefaultValue>] val mutable uv_walk_cb          : uv_walk_cb

  let mutable addr = sockaddr_in( a = 0L, b= 0L, c = 0L, d = 0L)
  let mutable addrV6 = sockaddr_in6( a = 0, b= 0, c = 0, d = 0, e = 0, f = 0, g = 0)

  let loop = createHandle <| uv_loop_size()
  let server = createHandle <| uv_handle_size(uv_handle_type.UV_TCP)

  let ip = binding.ip.ToString()
  let port = int binding.port

  let stopLoopCallbackHandle = createHandle <| uv_handle_size(uv_handle_type.UV_ASYNC)
  let synchronizationContextCallback = createHandle <| uv_handle_size(uv_handle_type.UV_ASYNC)

  let closeEvent = new ManualResetEvent(false)

  let opsPool = createLibUvOpsPool maxConcurrentOps

  member this.run () =
    Thread.BeginThreadAffinity()
    this.initLoop()
    try
      uv_tcp_init(loop, server) |> checkStatus
      uv_tcp_nodelay(server, 1) |> checkStatus
      if binding.ip.AddressFamily = AddressFamily.InterNetworkV6 then
        uv_ip6_addr(ip, port, &addrV6) |> checkStatus
        uv_tcp_bind6(server, &addrV6, 0u) |> checkStatus
      else
        uv_ip4_addr(ip, port, &addr) |> checkStatus
        uv_tcp_bind(server, &addr, 0) |> checkStatus
      let s = new LibUvSocket(opsPool, logger, serveClient, binding, loop,
                              bufferManager, startData, acceptingConnections,
                              this.synchronizationContext)
      s.initialize()
      s.run(server)
      s.exit()
    with ex ->
      logger.info (
        eventX "Could not start LibUvSocket"
        >> addExn ex
        >> setSingleName "Suave.LibUv.Tcp.LibUvServer.run")

    closeEvent.WaitOne() |> ignore
    destroyHandle loop
    event.Set() |> ignore
    logger.info (
      eventX "Exiting server"
      >> setSingleName "Suave.LibUv.Tcp.LibUvServer.run")

  member this.closeHandlerCallback (handle : IntPtr) =
    Marshal.FreeCoTaskMem handle

  member this.closeHandler (handle : IntPtr) (arg : IntPtr) =
    uv_close(handle,this.uv_close_cb_handler)

  member this.stopLoopCallback (_ : IntPtr) =
    logger.info (eventX "-->" >> setSingleName "Suave.LibUv.Tcp.LibUvServer.uv_stop_loop")
    uv_close(stopLoopCallbackHandle, this.uv_close_cb_loop)
    logger.info (eventX "<--" >> setSingleName "Suave.LibUv.Tcp.LibUvServer.uv_stop_loop")

  member this.init() =
    this.thread <- new Thread(this.run)
    this.thread.Name <- "LibUvLoop"
    this.uv_close_cb_destroy <- uv_close_cb(this.destroyServerCallback)
    this.uv_close_cb_thread  <- uv_close_cb(this.destroyRunOnThisThreadCallback)
    this.uv_close_cb_loop    <- uv_close_cb(this.destroyLoopCallback)
    this.uv_async_stop_loop_cb <- uv_handle_cb(this.stopLoopCallback)
    this.uv_close_cb_handler <- uv_close_cb(this.closeHandlerCallback)
    this.uv_walk_cb <- uv_walk_cb(this.closeHandler)

  member this.initLoop () =
    uv_loop_init(loop) |> checkStatus
    uv_async_init(loop, stopLoopCallbackHandle, this.uv_async_stop_loop_cb) |> checkStatus
    this.synchronizationContext <- new SingleThreadSynchronizationContext(loop, synchronizationContextCallback)
    this.synchronizationContext.init()

  member this.start() =
    this.thread.Start()

  member this.stopLoop() =
    logger.info (eventX "-->" >> setSingleName "Suave.LibUv.Tcp.LibUvServer.stopLoop")
    uv_async_send stopLoopCallbackHandle |> checkStatus
    closeEvent.WaitOne() |> ignore
    logger.info (eventX "<--" >> setSingleName "Suave.LibUv.Tcp.LibUvServer.stopLoop")

  member private this.destroyServerCallback _ =
    logger.info (eventX "-->" >> setSingleName "Suave.LibUv.Tcp.LibUvServer.destroyServerCallback")
    uv_walk(loop, this.uv_walk_cb, IntPtr.Zero)
    destroyHandle server
    logger.info (eventX "<--" >> setSingleName "Suave.LibUv.Tcp.LibUvServer.destroyServerCallback")
    closeEvent.Set() |> ignore

  member private this.destroyRunOnThisThreadCallback _ =
    logger.info (eventX "-->" >> setSingleName "Suave.LibUv.Tcp.LibUvServer.destroyRunOnThisThreadCallback")
    destroyHandle synchronizationContextCallback
    uv_close(server, this.uv_close_cb_destroy)
    logger.info (eventX "<--" >> setSingleName "Suave.LibUv.Tcp.LibUvServer.destroyRunOnThisThreadCallback")

  member private this.destroyLoopCallback _ =
    logger.info (eventX "-->" >> setSingleName "Suave.LibUv.Tcp.LibUvServer.destroyLoopCallback")
    destroyHandle stopLoopCallbackHandle
    this.synchronizationContext.running <- false
    uv_close(synchronizationContextCallback, this.uv_close_cb_thread)
    logger.info (eventX "<--" >> setSingleName "Suave.LibUv.Tcp.LibUvServer.destroyLoopCallback")

let runServerLibUv maxConcurrentOps bufferSize autoGrow (binding: SocketBinding) startData (acceptingConnections: AsyncResultCell<StartedData>) serveClient =
  let bufferManager = new BufferManager(bufferSize * (maxConcurrentOps + 1), bufferSize, autoGrow)
  bufferManager.Init()

  let exitEvent = new ManualResetEvent(false)

  let libUvServer = new LibUvServer(maxConcurrentOps, bufferManager,
                                    binding, startData, serveClient, acceptingConnections, exitEvent)

  libUvServer.init()
  async {
    use! disposable = Async.OnCancel(fun () -> libUvServer.stopLoop())
    do libUvServer.start()
    let! _ = Async.AwaitWaitHandle (exitEvent)
    return ()
  }
