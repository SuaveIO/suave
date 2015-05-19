namespace Suave.Sockets

open System
open System.Net.Sockets

[<AutoOpen>]
module SocketUtils =

  type private SystemSocketError = SocketError

  type Error =
    /// IO/Network/Checksum errors
    | SocketError of SystemSocketError
    /// Denotes either that Suave could not interpret the data sent on the socket
    /// or that the data sent on the socket did not conform to the relevant
    /// specification (TCP/HTTP/1.1/SSE/WebSocket etc).
    ///
    /// For a HTTP socket user this means a response of '400 Bad Request', or for
    /// example WebSockets would abort the connection.
    | InputDataError of string

  type ByteSegment = System.ArraySegment<byte>

  // Async is already a delayed type
  type SocketOp<'a> = Async<Choice<'a,Error>>

  let abort x = async { return Choice2Of2 x }

  /// Wraps the Socket.xxxAsync logic into F# async logic.
  let asyncDo (op : SocketAsyncEventArgs -> bool)
              (prepare : SocketAsyncEventArgs -> unit)
              (select: SocketAsyncEventArgs -> 'T)
              (args : SocketAsyncEventArgs) =
    Async.FromContinuations <| fun (ok, error, _) ->
      prepare args

      let k (args : SocketAsyncEventArgs) =
        match args.SocketError with
        | SystemSocketError.Success ->
          let result = select args
          ok (Choice1Of2 result)
        | e -> ok (Choice2Of2 (SocketError e))

      (args.UserToken :?> AsyncUserToken).Continuation <- k

      if not (op args) then
        k args
  
  /// Prepares the arguments by setting the buffer.
  let setBuffer (buf : ByteSegment) (args: SocketAsyncEventArgs) =
    args.SetBuffer(buf.Array, buf.Offset, buf.Count)
  
  let accept (socket : Socket) evArgs =
    asyncDo socket.AcceptAsync ignore (fun a -> a.AcceptSocket) evArgs
  
  let trans (a : SocketAsyncEventArgs) =
    new ArraySegment<_>(a.Buffer, a.Offset, a.BytesTransferred)

  let internal (@|!) c errorMsg =
    match c with
    | Choice1Of2 x -> async.Return (Choice1Of2 x)
    | Choice2Of2 (y : string) -> async.Return (Choice2Of2 (InputDataError y))