namespace Suave

open System
open System.Collections.Generic
open System.Threading.Tasks

open Suave.Sockets
open Suave.Utils.Bytes
open System.IO.Pipelines
open System.Buffers

[<Struct>]
type ScanResult =
  NeedMore | Found of found:int | Error of Error

[<Struct>]
type SelectResult =
  FailWith of error:Error | Continue of int

type SelectFunction = ReadOnlyMemory<byte> -> int -> SelectResult

module Aux =
  /// Splits the segment list in two lits of ArraySegment; the first one containing a total of index bytes
  let inline split (bufferSequence:ReadOnlySequence<byte>) (index:int) (select:SelectFunction) : SelectResult =
    let rec loop  (bufferSequence:ReadOnlySequence<byte>) acc : SelectResult =
        if bufferSequence.Length = 0 then
          Continue acc
        else
          let pair = bufferSequence.First
          if acc + pair.Length < index then
            let selectResult = select pair pair.Length
            match selectResult with
            | Continue _ ->
              loop (bufferSequence.Slice(pair.Length)) (acc + pair.Length)
            | FailWith s ->
              FailWith s
          elif acc + pair.Length >= index then
            let bytesRead = index - acc
            let selectResult = select pair bytesRead
            match selectResult with
            | Continue _ ->
              Continue(acc + bytesRead)
              | FailWith s ->
                FailWith s
            else failwith "Suave.Web.split: invalid case"

    loop bufferSequence 0

[<AllowNullLiteral>]
type HttpReader(transport : ITransport, pipe: Pipe, cancellationToken) =

  let mutable running : bool = true
  let mutable dirty : bool = false
  let dirtyLock = new obj()
  let readLineBuffer = System.Buffers.ArrayPool<byte>.Shared.Rent(8192)

  let mutable readerCancellationTokenSource : Threading.CancellationTokenSource = null

  member this.stop () =
    try
      lock dirtyLock (fun () ->
        if dirty then
          running <- false
          // Cancel any pending reads with the token BEFORE completing writer
          try
            if readerCancellationTokenSource <> null && not readerCancellationTokenSource.IsCancellationRequested then
              readerCancellationTokenSource.Cancel()
          with _ -> ()
          try pipe.Writer.Complete() with _ -> ()
          dirty <- false)
    with _ -> ()

  member x.readMoreData () =
    let buff = pipe.Writer.GetMemory()
    let readResult = transport.read buff
    if readResult.IsCompletedSuccessfully then
      // Synchronous read completion
      match readResult.Result with
      | Ok bytesRead ->
        if bytesRead > 0 then
          pipe.Writer.Advance(bytesRead)
          ValueTask<Result<unit,Error>>(
            task {
              let! _ = pipe.Writer.FlushAsync(cancellationToken)
              return Ok()
            })
        else
          ValueTask<Result<unit,Error>>(Result.Error (Error.ConnectionError "no more data"))
      | Result.Error e ->
        ValueTask<Result<unit,Error>>(Result.Error e)
    else
      // Async path
      ValueTask<Result<unit,Error>>(
        task {
          match! readResult with
          | Ok bytesRead ->
            if bytesRead > 0 then
              pipe.Writer.Advance(bytesRead)
              let! _ = pipe.Writer.FlushAsync(cancellationToken)
              return Ok()
            else
              return Result.Error (Error.ConnectionError "no more data")
          | Result.Error e ->
            return Result.Error e
        })

  member x.getData () =
    task{
      let! result = pipe.Reader.ReadAsync(readerCancellationTokenSource.Token)
      return result
      }

  /// Iterates over a BufferSegment list looking for a marker, data before the marker
  /// is sent to the function select
  /// Returns the number of bytes read.
  member x.scanMarker (marker: byte[]) (select : SelectFunction) =
    task{
      try
        if not running then
          return Result.Error(Error.ConnectionError "No more data")
        else
          let! result = x.getData()
          if result.IsCanceled then
            return Result.Error(Error.ConnectionError "ReadAsync was canceled")
          else
            let bufferSequence = result.Buffer
            if result.IsCompleted && bufferSequence.Length = 0L then
              return Result.Error(Error.ConnectionError "no more data")
            else
              match kmpW marker bufferSequence with
              | ValueSome x ->
                let res = Aux.split bufferSequence (int x) select
                match res with
                | Continue n ->
                  pipe.Reader.AdvanceTo(bufferSequence.GetPosition(int64(n + marker.Length)))
                  return Result.Ok(Found n)
                | FailWith s ->
                  return Result.Ok(Error s)
              | ValueNone ->
                let r = Aux.split bufferSequence (int(bufferSequence.Length - int64 marker.Length)) select
                pipe.Reader.AdvanceTo(bufferSequence.GetPosition(bufferSequence.Length - marker.LongLength))
                return Result.Ok(NeedMore)
      with ex ->
        return Result.Error(Error.ConnectionError ex.Message)
  }

  /// Read the passed stream into buff until the EOL (CRLF) has been reached
  /// and returns the number of bytes read and the connection
  member x.readUntilPattern marker select : SocketOp<int> =
    ValueTask<Result<int,Error>>(
      task {
        let mutable reading = true
        let mutable error = false
        let mutable result = 0
        let mutable errorResult = Ok(0)
        while reading && not error && not(cancellationToken.IsCancellationRequested) do
          let! res = x.scanMarker marker select
          match res with
          | Ok(Found a) ->
            reading <- false
            result <- a
          | Ok(NeedMore) ->
            ()
          | Result.Error s
          | Ok(Error s) ->
            error <- true
            errorResult <- Result.Error s
        if error then
          return errorResult
        else
          return Ok (result)
      })

  member x.skip n =
    task{
      let! result= x.getData()
      let bufferSequence = result.Buffer
      pipe.Reader.AdvanceTo(bufferSequence.GetPosition(int64 n))
      return Found n
    }

  /// Read a line from the stream, calling UTF8.toString on the bytes before the EOL marker
  member x.readLine () : SocketOp<string> = 
    ValueTask<Result<string,Error>>(
      task {
        let mutable offset = 0
        match! x.readUntilPattern EOL (fun a count ->
            if offset + count > readLineBuffer.Length then
              FailWith (InputDataError (Some 414, "Line Too Long"))
            else
              let source = a.Span.Slice(0,count)
              let target = new Span<byte>(readLineBuffer,offset,count)
              source.CopyTo(target)
              offset <- offset + count
              Continue offset) with
        | Result.Error e ->
          return Result.Error e
        | Ok _ ->
          let result = Globals.UTF8.GetString(readLineBuffer, 0, offset)
          return Ok result
      })

  /// Read all headers from the stream, returning a dictionary of the headers found
  member x.readHeaders() : SocketOp<List<string*string>> =
    ValueTask<Result<List<string*string>,Error>>(
      task {
        let headers = new List<string*string>()
        let mutable flag = true
        let mutable result = Ok (headers)
        while flag && (not cancellationToken.IsCancellationRequested) do
          let! _line = x.readLine ()
          match _line with
          | Ok line ->
            if line <> String.Empty then
              let indexOfColon = line.IndexOf(':')
              if indexOfColon = -1 then
                flag <- false
                result <- Result.Error (InputDataError (Some 400, "Bad Request: Malformed Header"))
              else
              let header = (line.Substring(0, indexOfColon).ToLower(), line.Substring(indexOfColon+1).TrimStart())
              headers.Add header
            else
              flag <- false
          | Result.Error e ->
            flag <- true
            result <- Result.Error e
        return result
      })

  /// Read the post data from the stream, given the number of bytes that makes up the post data.
  member x.readPostData (bytes : int) (select:ReadOnlyMemory<byte> -> int -> unit) : Task<unit> =
    let rec loop (n:int) : Task<unit> =
      task {
        if n = 0 then
          return ()
        else
          let! result = x.getData()
          let bufferSequence = result.Buffer
          if bufferSequence.Length > 0  then
            let segment = bufferSequence.First
            if segment.Length > n then
              select segment n
              pipe.Reader.AdvanceTo(bufferSequence.GetPosition(int64(n)))
              return ()
            else
              select segment segment.Length
              pipe.Reader.AdvanceTo(bufferSequence.GetPosition(int64(segment.Length)))
              return! loop (n - segment.Length)
          else
            return ()
      }
    loop bytes

  member this.isDirty = 
    lock dirtyLock (fun () -> dirty)

  member this.init() =
    lock dirtyLock (fun () ->
      dirty <- true
      running <- true
    )
    readerCancellationTokenSource <- new Threading.CancellationTokenSource()

  member this.readLoop() = task{
    try
      let mutable reading = true
      let mutable result = Ok()
      while running && reading && not(cancellationToken.IsCancellationRequested) do
        let! a = this.readMoreData()
        match a with
        | Ok () -> ()
        | Result.Error b as a->
          reading <- false
          result <- a
          this.stop()
      return result
    with ex ->
      return Result.Error(Error.ConnectionError ex.Message)
  }

  // Return readLineBuffer to the ArrayPool when the reader is disposed
  interface IDisposable with
    member this.Dispose() =
      System.Buffers.ArrayPool<byte>.Shared.Return(readLineBuffer, true)
