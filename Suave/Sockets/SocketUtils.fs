namespace Suave.Sockets

open System
open System.Net.Sockets

[<AutoOpen>]
module SocketUtils =
  
  type Error = 
    | SocketError of SocketError
    | OtherError of string
  
  
  type private A = System.Net.Sockets.SocketAsyncEventArgs
  type ByteSegment = System.ArraySegment<byte>
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
  let inline set_buffer (buf : ByteSegment) (args: A) =
    args.SetBuffer(buf.Array, buf.Offset, buf.Count)
  
  let inline accept (socket : Socket) =
    async_do socket.AcceptAsync ignore (fun a -> a.AcceptSocket)
  
  let inline trans (a : SocketAsyncEventArgs) =
    new ArraySegment<_>(a.Buffer, a.Offset, a.BytesTransferred)
  