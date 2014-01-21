module Suave.Socket

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Net.Sockets
 
// This class creates a single large buffer which can be divided up  
// and assigned to SocketAsyncEventArgs objects for use with each  
// socket I/O operation.   
// This enables bufffers to be easily reused and guards against  
// fragmenting heap memory. 
//  
// The operations exposed on the BufferManager class are not thread safe. 
type  BufferManager(totalBytes, bufferSize) =

  let mutable m_numBytes = totalBytes // the total number of bytes controlled by the buffer pool 
  let m_buffer = Array.zeroCreate(totalBytes); // the underlying byte array maintained by the Buffer Manager
  let m_freeIndexPool = new ConcurrentStack<int>();

  // Pops a buffer from the buffer pool
  member x.PopBuffer() : ArraySegment<byte> =
    let offset = ref -1
    if m_freeIndexPool.TryPop(offset) then
      Log.tracef (fun fmt -> fmt "reserving buffer: %d" !offset )
      ArraySegment(m_buffer, !offset, bufferSize)
    else 
      failwith "failed to obtain a buffer"

  member x.Init() = 
    let mutable counter = 0
    while counter < totalBytes - bufferSize do
      m_freeIndexPool.Push(counter)
      counter <- counter + bufferSize

  // Frees the buffer back to the buffer pool
  // WARNING: there is nothing preventing you from freeing the same offset more than once with nasty consequences
  member x.FreeBuffer(args : ArraySegment<_>) =
    Log.tracef (fun fmt -> fmt "freeing buffer: %d" args.Offset )
    m_freeIndexPool.Push(args.Offset)

type SocketAsyncEventArgsPool() =

  let m_pool = new ConcurrentStack<SocketAsyncEventArgs>()

  member x.Push(item : SocketAsyncEventArgs) =
    m_pool.Push(item)

  member x.Pop() =
   let arg = ref null
   if m_pool.TryPop(arg) then !arg else failwith "failed to obtain socket args."

  // The number of SocketAsyncEventArgs instances in the pool 
  member x.Count with get() = m_pool.Count
 
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

/// Wraps the Socket.xxxAsync logic into F# async logic.
let inline async_do (op : A -> bool) (prepare : A -> unit) (select: A -> 'T) (args : A) =
  Async.FromContinuations <| fun (ok, error, _) ->
    prepare args
    let k (args : A) =
      match args.SocketError with
      | System.Net.Sockets.SocketError.Success ->
        let result = select args
        ok result
      | e -> error (SocketIssue e)
    (args.UserToken :?> AsyncUserToken).Continuation <- k
    if not (op args) then
      k args

/// Prepares the arguments by setting the buffer.
let inline set_buffer (buf : B) (args: A) =
    args.SetBuffer(buf.Array, buf.Offset, buf.Count)

let inline accept (socket : Socket) =
    async_do socket.AcceptAsync ignore (fun a -> a.AcceptSocket)

let inline trans (a : SocketAsyncEventArgs) = 
    new ArraySegment<_>(a.Buffer,a.Offset,a.BytesTransferred)

open System.Net

/// A connection (TCP implied) is a thing that can read and write from a socket
/// and that can be closed.
type Connection = { 
  ipaddr : IPAddress;
  read   : ArraySegment<byte> -> Async<int>;
  write  : ArraySegment<byte> -> Async<unit>;
  get_buffer  : unit -> ArraySegment<byte>;
  free_buffer : ArraySegment<byte> -> unit;
  shutdown : unit -> unit 
  }

let eol_array_segment = new ArraySegment<_>(EOL, 0, 2)

/// Write the string s to the stream asynchronously
/// as ASCII encoded text
let inline async_writeln (connection : Connection) (s : string) = async {
  if s.Length > 0 then 
    let buff = connection.get_buffer()
    let c = bytes_to_buffer s buff.Array buff.Offset
    do! connection.write (new ArraySegment<_>(buff.Array, buff.Offset, c))
    connection.free_buffer buff
  do! connection.write eol_array_segment
}

/// Write the string s to the stream asynchronously
/// from a byte array
let inline async_writebytes (connection : Connection) (b : byte[]) = async {
  if b.Length > 0 then do! connection.write (new ArraySegment<_>(b, 0, b.Length))
}

open System.IO
open System.Threading.Tasks
open Async

/// Asynchronouslyo write from the 'from' stream to the 'to' stream.
let transfer_x (to_stream : Connection) (from : Stream) =
  let buf = Array.zeroCreate<byte> 0x2000
  let rec do_block () = async {
    let! read = from.AsyncRead buf
    if read <= 0 then
      return ()
    else
      do! to_stream.write (new ArraySegment<_>(buf,0,read))
      return! do_block () }
  do_block ()

/// Asynchronously write from the 'from' stream to the 'to' stream, with an upper bound on
/// amount to transfer by len
let transfer_len_x (to_stream : Connection) (from : Stream) len =
  let buf_size = 0x2000
  let buf = Array.zeroCreate<byte> 0x2000
  let rec do_block left = async {
    let! read = from.AsyncRead(buf, 0, Math.Min(buf_size, left))
    if read <= 0 || left - read = 0 then
      return ()
    else
      do! to_stream.write (new ArraySegment<_>(buf,0,read))
      return! do_block (left - read) }
  do_block len
