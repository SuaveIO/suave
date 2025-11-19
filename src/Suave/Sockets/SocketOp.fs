namespace Suave.Sockets

open System
open System.Net.Sockets
open System.Threading.Tasks

type private SystemSocketError = SocketError

[<Struct>]
type Error =
  /// IO/Network/Checksum errors
  | SocketError of error:SystemSocketError
  /// Denotes either that Suave could not interpret the data sent on the socket
  /// or that the data sent on the socket did not conform to the relevant
  /// specification (TCP/HTTP/1.1/SSE/WebSocket etc).
  ///
  /// For a HTTP socket user this means a response of '400 Bad Request', or for
  /// example WebSockets would abort the connection.
  /// You can specify `Some statusCode` as response status code. If `None` status code 400 will be used.
  | InputDataError of dataError:(int option*string)
  /// Represents an IO/network error; to be used when we do not have a SocketError
  /// but just an error message; like in libuv calls.
  | ConnectionError of string

type ByteSegment = Memory<byte>

/// ValueTask-based socket operation for allocation-free synchronous completions
type SocketOp<'a> = ValueTask<Result<'a,Error>>

/// The module
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SocketOp =

  /// create a new successful value
  let inline mreturn (x : 'T) : SocketOp<'T> =
    ValueTask<Result<'T,Error>>(Ok(x))

  /// create a new unsuccessful value
  let inline abort (x : Error) : SocketOp<_> =
    ValueTask<Result<_,Error>>(Result.Error(x))

  /// says that something is wrong with the input on a protocol level and that
  /// it's therefore a bad request (user input error) -- the error already present
  /// is overwritten with the errorMsg parameter.
  let inline orInputError errorMsg : _ -> SocketOp<_> = function
    | Choice1Of2 x -> mreturn(x)
    | Choice2Of2 _ -> abort (InputDataError errorMsg)

  /// same as the above, but let's you do something with the existing error message
  /// through the callback function passed
  let inline orInputErrorf fErrorMsg : _ -> SocketOp<_> = function
    | Ok x -> mreturn(x)
    | Error (y : string) -> abort (InputDataError (fErrorMsg y))

  /// Bind the result successful result of the SocketOp to fCont
  let inline bind (fCont : _ -> SocketOp<_>) (value : SocketOp<_>) : SocketOp<_> =
    ValueTask<Result<_,Error>>(
      task {
        match! value with
        | Ok x -> return! fCont x
        | Error (err : Error) -> return Result.Error err
      })

  /// Bind the error result of the SocketOp to fCont
  let inline bindError (fCont : _ -> SocketOp<_>) (value : SocketOp<_>) : SocketOp<_> =
    ValueTask<Result<_,Error>>(
      task {
        match! value with
        | Ok x -> return Ok x
        | Error (err : Error) -> return! fCont err
      })

  /// Map f over the contained successful value in SocketOp
  let inline map f (value : SocketOp<_>) : SocketOp<_> =
    ValueTask<Result<_,Error>>(
      task {
        match! value with
        | Ok x -> return Ok (f x)
        | Error (err : Error) -> return Result.Error err
      })

  /// Map f over the error value in SocketOp
  let inline mapError f (value : SocketOp<_>) : SocketOp<_> =
    ValueTask<Result<_,Error>>(
      task {
        match! value with
        | Ok x -> return Ok x
        | Error (err : Error) -> return Result.Error (f err)
      })

  /// lift a Async<'a> type to the SocketOp
  let inline ofAsync (a : Async<'a>) : SocketOp<'a> =
    ValueTask<Result<'a,Error>>(
      task {
        let! s = a
        return Ok s
      })

  /// Convert ValueTask-based SocketOp to Task-based for compatibility
  let inline toTask (value : SocketOp<'a>) : Task<Result<'a,Error>> =
    value.AsTask()

  /// Convert Task-based to ValueTask-based SocketOp
  let inline ofTask (value : Task<Result<'a,Error>>) : SocketOp<'a> =
    ValueTask<Result<'a,Error>>(value)

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
