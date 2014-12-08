module Suave.Socket

open Suave.Utils.Bytes
open Suave.Async

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Net.Sockets
open System.Threading.Tasks

/// This class creates a single large buffer which can be divided up
/// and assigned to SocketAsyncEventArgs objects for use with each
/// socket I/O operation.
/// This enables bufffers to be easily reused and guards against
/// fragmenting heap memory.
///
/// The operations exposed on the BufferManager class are not thread safe.
type BufferManager(total_bytes, buffer_size, logger) =
  do Log.internf logger "Suave.Socket.BufferManager" (fun fmt ->
    fmt "initialising BufferManager with %d bytes" total_bytes)

  /// the underlying byte array maintained by the Buffer Manager
  let buffer = Array.zeroCreate total_bytes
  let free_offsets = new Stack<int>()

  /// Pops a buffer from the buffer pool
  member x.PopBuffer(?context : string) : ArraySegment<byte> =
    let offset, free_count = lock free_offsets (fun _ ->
      free_offsets.Pop(), free_offsets.Count)
    Log.internf logger "Suave.Socket.BufferManager" (fun fmt ->
      fmt "reserving buffer: %d, free count: %d [%s]" offset free_count (defaultArg context "no-ctx"))
    ArraySegment(buffer, offset, buffer_size)

  /// Initialise the memory required to use this BufferManager
  member x.Init() =
    lock free_offsets (fun _ ->
      let mutable running_offset = 0
      while running_offset < total_bytes - buffer_size do
        free_offsets.Push running_offset
        running_offset <- running_offset + buffer_size)

  /// Frees the buffer back to the buffer pool
  /// WARNING: there is nothing preventing you from freeing the same offset
  /// more than once with nasty consequences
  member x.FreeBuffer(args : ArraySegment<_>, ?context : string) =
    let free_count = lock free_offsets (fun _ ->
      if free_offsets.Contains args.Offset then failwith "double free"
      free_offsets.Push args.Offset
      free_offsets.Count)
    Log.internf logger "Suave.Socket.BufferManager" (fun fmt ->
      fmt "freeing buffer: %d, free count: %d [%s]" args.Offset free_count (defaultArg context "no-ctx"))

type SocketAsyncEventArgsPool() =

  let m_pool = new Stack<SocketAsyncEventArgs>()

  member x.Push(item : SocketAsyncEventArgs) =
    lock m_pool (fun _ -> m_pool.Push item)

  member x.Pop() =
    lock m_pool (fun _ -> m_pool.Pop())

exception SocketIssue of SocketError with
  override this.ToString() =
    string this.Data0

type AsyncUserToken(?socket : Socket) =
  let mutable _socket = match socket with Some x -> x | None -> null
  let mutable _continuation : SocketAsyncEventArgs -> unit = fun _ -> ()
  member x.Socket
    with get () = _socket and set a = _socket <- a
  member x.Continuation
    with get () = _continuation and set a = _continuation <- a

type A = System.Net.Sockets.SocketAsyncEventArgs
type B = System.ArraySegment<byte>

type Error = 
  | SocketError of SocketError
  | OtherError of string

// Async is already a delayed type
type SocketOp<'a> = Async<Choice<'a,Error>>

let abort x = async { return Choice2Of2 x }

/// Wraps the Socket.xxxAsync logic into F# async logic.
let inline async_do (op : A -> bool) (prepare : A -> unit) (select: A -> 'T) (args : A) =
  Async.FromContinuations <| fun (ok, error, _) ->
    prepare args
    let k (args : A) =
        match args.SocketError with
        | System.Net.Sockets.SocketError.Success ->
          let result = select args
          ok <| Choice1Of2 result
        | e -> ok <|  Choice2Of2 (SocketError e)
    (args.UserToken :?> AsyncUserToken).Continuation <- k
    if not (op args) then
      k args

/// Prepares the arguments by setting the buffer.
let inline set_buffer (buf : B) (args: A) =
  args.SetBuffer(buf.Array, buf.Offset, buf.Count)

let inline accept (socket : Socket) =
  async_do socket.AcceptAsync ignore (fun a -> a.AcceptSocket)

let inline trans (a : SocketAsyncEventArgs) =
  new ArraySegment<_>(a.Buffer, a.Offset, a.BytesTransferred)

/// A connection (TCP implied) is a thing that can read and write from a socket
/// and that can be closed.
type Connection =
  { ipaddr       : IPAddress
    socket       : Socket
    read_args    : SocketAsyncEventArgs
    write_args   : SocketAsyncEventArgs
    buffer_manager : BufferManager
    line_buffer  : ArraySegment<byte>
    segments        : BufferSegment list }

let inline receive (cn : Connection) (buf : B) =
  async_do cn.socket.ReceiveAsync (set_buffer buf)  (fun a -> a.BytesTransferred) cn.read_args

let inline send (cn : Connection) (buf : B) =
  async_do cn.socket.SendAsync (set_buffer buf) ignore cn.write_args

/// Workflow builder to read/write to async sockets with fail/success semantics
type SocketMonad() =
  member this.Return(x:'a) : SocketOp<'a> = async{ return Choice1Of2 x }
  member this.Zero() : SocketOp<unit> = this.Return()
  member this.ReturnFrom(x : SocketOp<'a>) : SocketOp<'a> = x
  member this.Delay(f: unit ->  SocketOp<'a>) = async { return! f () }

  member this.Bind(x : SocketOp<'a>,f : 'a -> SocketOp<'b>) : SocketOp<'b> = async{
    let! result = x
    match result with
    | Choice1Of2 a -> return! f a
    | Choice2Of2 b -> return Choice2Of2 b
    }

  member this.Combine(v, f) = this.Bind(v, fun () -> f)

  member this.While(guard, body : SocketOp<unit>) : SocketOp<unit> = async {
    if guard() then
      let! result = body
      match result with
      | Choice1Of2 a ->
        return! this.While(guard, body)
      | Choice2Of2 _ ->
        return result
    else
      return! this.Zero()
      }

  member this.TryWith(body, handler) = async {
    try
      return! body
    with e ->
      return! handler e
    }

  member this.TryFinally(body, compensation) = async {
     try
       return! body
     finally
       compensation()
    }

  member this.Using(disposable : #System.IDisposable, body) = async {
    use _ = disposable
    return! body disposable
    }

  member this.For(sequence : seq<_>, body : 'a -> SocketOp<unit>) =
    this.Using(sequence.GetEnumerator(), fun (enum : IEnumerator<'a>) ->
    this.While(enum.MoveNext, this.Delay(fun _-> body enum.Current)))
 
/// The socket monad   
let socket = SocketMonad()

/// A TCP Worker is a thing that takes a TCP client and returns an asynchronous workflow thereof
type TcpWorker<'a> = Connection -> SocketOp<'a>

/// lift a Async<'a> type to the Socket monad
let lift_async (a : Async<'a>) : SocketOp<'a> = 
  async { 
    let! s = a
    return Choice1Of2 s 
  }

/// lift a Task type to the Socket monad
let lift_task (a : Task) : SocketOp<unit>  = 
  async {
    let! s = a
    return Choice1Of2 s 
  }

/// Write the string s to the stream asynchronously as ASCII encoded text
let inline async_write (connection : Connection) (s : string) : SocketOp<unit> = 
  async {
    if s.Length > 0 then
      let buff = connection.line_buffer
      let c = bytes_to_buffer s buff.Array buff.Offset
      return! send connection (new ArraySegment<_>(buff.Array, buff.Offset, c))
    else return Choice1Of2 ()
  }

let inline async_write_nl (connection : Connection) = 
  send connection eol_array_segment

let inline async_writeln (connection : Connection) (s : string) : SocketOp<unit> = 
  socket {
    do! async_write connection s
    do! async_write_nl connection
  }

/// Write the string s to the stream asynchronously from a byte array
let inline async_writebytes (connection : Connection) (b : byte[]) : SocketOp<unit> = async {
  if b.Length > 0 then return! send connection (new ArraySegment<_>(b, 0, b.Length))
  else return Choice1Of2 ()
}

/// Asynchronously write from the 'from' stream to the 'to' stream.
let transfer_x (to_stream : Connection) (from : Stream) : SocketOp<unit> =
  let buf = Array.zeroCreate<byte> 0x2000
  let rec do_block () = socket {
    let! read = lift_async <| from.AsyncRead buf
    if read <= 0 then
      return ()
    else
      do! send to_stream (new ArraySegment<_>(buf,0,read))
      return! do_block () }
  do_block ()

/// Asynchronously write from the 'from' stream to the 'to' stream, with an upper bound on
/// amount to transfer by len
let transfer_len_x (to_stream : Connection) (from : Stream) len =
  let buf_size = 0x2000
  let buf = Array.zeroCreate<byte> 0x2000
  let rec do_block left = socket {
    let! read = lift_async <| from.AsyncRead(buf, 0, Math.Min(buf_size, left))
    if read <= 0 || left - read = 0 then
      return ()
    else
      do! send to_stream (new ArraySegment<_>(buf,0,read))
      return! do_block (left - read) }
  do_block len
