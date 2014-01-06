module Suave.Socket

open System
open System.Collections.Generic
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
  let m_freeIndexPool = new Stack<int>();
  let mutable m_currentIndex = 0


  // Assigns a buffer from the buffer pool to the  
  // specified SocketAsyncEventArgs object 
  // 
  // <returns>true if the buffer was successfully set, else false</returns> 
  member x.PopBuffer() =
    lock m_freeIndexPool (fun () ->
    if (m_freeIndexPool.Count > 0) then 
      ArraySegment(m_buffer, m_freeIndexPool.Pop(), bufferSize)
    else
      if ((m_numBytes - bufferSize) < m_currentIndex) then 
        failwith "we ran out of buffers"
      else
        m_currentIndex <- m_currentIndex + bufferSize
        ArraySegment(m_buffer, m_currentIndex - bufferSize, bufferSize))

  // Assigns a buffer from the buffer pool to the  
  // specified SocketAsyncEventArgs object 
  // 
  // <returns>true if the buffer was successfully set, else false</returns> 
  member x.SetBuffer(args : SocketAsyncEventArgs) =
    if (m_freeIndexPool.Count > 0) then 
      args.SetBuffer(m_buffer, m_freeIndexPool.Pop(), bufferSize)
      true
    else
      if ((m_numBytes - bufferSize) < m_currentIndex) then 
        failwith "we ran out of buffers"
      else
        args.SetBuffer(m_buffer, m_currentIndex, bufferSize)
        m_currentIndex <- m_currentIndex + bufferSize
        true

  // Removes the buffer from a SocketAsyncEventArg object.   
  // This frees the buffer back to the buffer pool 
  member x.FreeBuffer(args : SocketAsyncEventArgs) =
    m_freeIndexPool.Push(args.Offset);
    args.SetBuffer(null, 0, 0);

  // Removes the buffer from a SocketAsyncEventArg object.   
  // This frees the buffer back to the buffer pool 
  member x.FreeBuffer(args : ArraySegment<_>) =
    lock m_freeIndexPool (fun () -> m_freeIndexPool.Push(args.Offset))

type SocketAsyncEventArgsPool(capacity : int) =

  let m_pool = new Stack<SocketAsyncEventArgs>(capacity)

  member x.Push(item : SocketAsyncEventArgs) =
    lock m_pool (fun () -> m_pool.Push(item))

  member x.Pop() =
    lock m_pool (fun () -> m_pool.Pop())

  // The number of SocketAsyncEventArgs instances in the pool 
  member x.Count with get() = m_pool.Count
 
exception SocketIssue of SocketError with
    override this.ToString() =
        string this.Data0

type AsyncUserToken(?socket : Socket) =
  let mutable _socket = match socket with Some x -> x | None -> null
  let mutable _continuation : System.EventHandler<_> = null
  member x.Socket 
    with get () = _socket and set a = _socket <- a
  member x.Continuation
    with get () = _continuation and set a = _continuation <- a

type A = System.Net.Sockets.SocketAsyncEventArgs
type B = System.ArraySegment<byte>

/// Wraps the Socket.xxxAsync logic into F# async logic.
let inline asyncDo (op: A -> bool) (prepare: A -> unit) (select: A -> 'T) args =
  Async.FromContinuations <| fun (ok, error, _) ->
    prepare args
    let k (args: A) =
        match args.SocketError with
        | System.Net.Sockets.SocketError.Success ->
            let result = select args
            ok result
        | e -> error (SocketIssue e)
    (args.UserToken :?> AsyncUserToken).Continuation <- System.EventHandler<_>(fun _ -> k)
    if not (op args) then
        k args
/// Prepares the arguments by setting the buffer.
let inline setBuffer (buf: B) (args: A) =
    args.SetBuffer(buf.Array, buf.Offset, buf.Count)

let inline accept (socket: Socket) =
    asyncDo socket.AcceptAsync ignore (fun a -> a.AcceptSocket)

let inline trans (a:SocketAsyncEventArgs) = 
    new ArraySegment<_>(a.Buffer,a.Offset,a.BytesTransferred)

type Connection = { 
  ipaddr : string;
  reader : (ArraySegment<byte> -> int) -> Async<int>;
  writer : ArraySegment<byte> -> Async<unit>;
  shutdown : unit -> unit 
  }

/// Write the string s to the stream asynchronously
/// as ASCII encoded text
let inline async_writeln (connection : Connection) s = async {
  let b = bytes s
  if b.Length > 0 then do! connection.writer (new ArraySegment<_>(b, 0, b.Length))
  do! connection.writer (new ArraySegment<_>(EOL, 0, 2))
}

/// Write the string s to the stream asynchronously
/// from a byte array
let inline async_writebytes (connection : Connection) (b : byte[]) = async {
  if b.Length > 0 then do! connection.writer (new ArraySegment<_>(b, 0, b.Length))
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
      do! to_stream.writer (new ArraySegment<_>(buf,0,read))
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
      do! to_stream.writer (new ArraySegment<_>(buf,0,read))
      return! do_block (left - read) }
  do_block len
