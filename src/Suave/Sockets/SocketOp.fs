namespace Suave.Sockets

open System
open System.Net.Sockets

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
  /// You can specify `Some statusCode` as response status code. If `None` status code 400 will be used.
  | InputDataError of (int option*string)
  /// Represents an IO/network error; to be used when we do not have a SocketError
  /// but just an error message; like in libuv calls.
  | ConnectionError of string

type ByteSegment = System.ArraySegment<byte>

// Async is already a delayed type
type SocketOp<'a> = Async<Choice<'a,Error>>

/// The module
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SocketOp =
  open Suave.Utils
  open System.Threading.Tasks

  /// create a new successful value
  let mreturn (x : 'T) : SocketOp<'T> =
    async.Return (Choice1Of2 x)

  /// create a new unsuccessful value
  let abort (x : Error) : SocketOp<_> =
    async.Return (Choice2Of2 x)

  /// says that something is wrong with the input on a protocol level and that
  /// it's therefore a bad request (user input error) -- the error already present
  /// is overwritten with the errorMsg parameter.
  let orInputError errorMsg : _ -> SocketOp<_> = function
    | Choice1Of2 x -> async.Return (Choice1Of2 x)
    | Choice2Of2 _ -> async.Return (Choice2Of2 (InputDataError errorMsg))

  /// same as the above, but let's you do something with the existing error message
  /// through the callback function passed
  let orInputErrorf fErrorMsg : _ -> SocketOp<_> = function
    | Choice1Of2 x -> async.Return (Choice1Of2 x)
    | Choice2Of2 (y : string) -> async.Return (Choice2Of2 (InputDataError (fErrorMsg y)))

  /// Bind the result successful result of the SocketOp to fCont
  let bind (fCont : _ -> SocketOp<_>) (value : SocketOp<_>) : SocketOp<_> = async {
    let! x = value
    match x with
    | Choice1Of2 x -> return! fCont x
    | Choice2Of2 (err : Error) -> return Choice2Of2 err
    }

  /// Bind the error result of the SocketOp to fCont
  let bindError (fCont : _ -> SocketOp<_>) (value : SocketOp<_>) : SocketOp<_> = async {
    let! x = value
    match x with
    | Choice1Of2 x -> return Choice1Of2 x
    | Choice2Of2 (err : Error) -> return! fCont err
    }

  /// Map f over the contained successful value in SocketOp
  let map f (value : SocketOp<_>) : SocketOp<_> = async {
    let! x = value
    match x with
    | Choice1Of2 x -> return Choice1Of2 (f x)
    | Choice2Of2 (err : Error) -> return Choice2Of2 err 
    }

  /// Map f over the error value in SocketOp
  let mapError f (value : SocketOp<_>) : SocketOp<_> = async {
    let! x = value
    match x with
    | Choice1Of2 x -> return Choice1Of2 x
    | Choice2Of2 (err : Error) -> return Choice2Of2 (f err)
    }

  /// lift a Async<'a> type to the SocketOp
  let ofAsync (a : Async<'a>) : SocketOp<'a> = async {
    let! s = a
    return Choice1Of2 s
    }

  /// lift a Task type to the SocketOp
  let ofTask (a : Task) : SocketOp<unit> = async {
    let! s = a
    return Choice1Of2 s
    }

  module Operators =

    /// See SocketOp.orInputError
    let (@|!) c errorMsg =
      orInputError errorMsg c

    /// See SocketOp.orInputErrorf
    let (@|!!) c fErrorMsg =
      orInputErrorf fErrorMsg c

    /// See SocketOp.bindError
    let (@|>) c fError =
      bindError fError c

[<AutoOpen>]
module Utils =
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
   
  /// Makes sure the finalizer is called regardless of the result of executing body
  let finalize (body: SocketOp<unit>) (finalizer: unit -> unit) =
    async {
      let! result = body
      do finalizer()
      return result
      }