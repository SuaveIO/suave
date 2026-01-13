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
  NeedMore | Found of found:int64 | Error of Error

[<Struct>]
type SelectResult =
  FailWith of error:Error | Continue of int64

type SelectFunction = ReadOnlyMemory<byte> -> int -> SelectResult

module Aux =
  /// Splits the segment list in two lits of ArraySegment; the first one containing a total of index bytes
  let inline split (bufferSequence:ReadOnlySequence<byte>) (index:int64) (select:SelectFunction) : SelectResult =
    // Use iteration instead of recursion to avoid FS3511 warning when called from task { }
    let mutable currentSequence = bufferSequence
    let mutable acc = 0L
    let mutable result = Unchecked.defaultof<SelectResult>
    let mutable finished = false
    
    while not finished do
      if currentSequence.Length = 0L then
        result <- Continue acc
        finished <- true
      else
        let pair = currentSequence.First
        if acc + int64 pair.Length < index then
          let selectResult = select pair pair.Length
          match selectResult with
          | Continue _ -> 
            currentSequence <- currentSequence.Slice(pair.Length)
            acc <- acc + int64 pair.Length
          | FailWith s -> 
            result <- FailWith s
            finished <- true
        elif acc + int64 pair.Length >= index then
          // We have reached the index within this segment
          let bytesRead = index - acc
          assert(bytesRead <= int64 Int32.MaxValue)
          let selectResult = select pair (int bytesRead)
          match selectResult with
          | Continue _ -> 
            result <- Continue(acc + bytesRead)
            finished <- true
          | FailWith s -> 
            result <- FailWith s
            finished <- true
        else 
          failwith "Suave.Web.split: invalid case"
    
    result

[<AllowNullLiteral>]
type HttpReader(transport : ITransport, pipe: Pipe, cancellationToken: Threading.CancellationToken) =

  let mutable running : bool = true
  let mutable dirty : bool = false
  let dirtyLock = new obj()
  let readLineBuffer = System.Buffers.ArrayPool<byte>.Shared.Rent(8192)

  // Use SemaphoreSlim instead of lock to allow async waiting
  let pipeReaderSemaphore = new Threading.SemaphoreSlim(1, 1)

  let mutable readerCancellationTokenSource : Threading.CancellationTokenSource = null

  member this.cancelPendingReads() =
    // Cancel the token first to stop any pending operations
    if readerCancellationTokenSource <> null && not readerCancellationTokenSource.IsCancellationRequested then
      try readerCancellationTokenSource.Cancel() with _ -> ()
    pipe.Reader.CancelPendingRead()

  member this.stop () =
    try
      lock dirtyLock (fun () ->
        if dirty then
          running <- false
          dirty <- false)
    with ex ->  ()

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
              let! _ = pipe.Writer.FlushAsync(readerCancellationTokenSource.Token)
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
              let! _ = pipe.Writer.FlushAsync(readerCancellationTokenSource.Token)
              return Ok()
            else
              return Result.Error (Error.ConnectionError "no more data")
          | Result.Error e ->
            return Result.Error e
        })

  /// Iterates over a BufferSegment list looking for a marker, data before the marker
  /// is sent to the function select
  /// Returns the number of bytes read.
  member x.scanMarker (marker: byte[]) (select : SelectFunction) =
    task {
      let mutable semaphoreAcquired = false
      try
        try
          do! pipeReaderSemaphore.WaitAsync(readerCancellationTokenSource.Token)
          semaphoreAcquired <- true
          
          let! result = pipe.Reader.ReadAsync(readerCancellationTokenSource.Token)
          
          if result.IsCanceled then
            return Result.Error(Error.ConnectionError "ReadAsync was canceled")
          else
            let bufferSequence = result.Buffer
            if result.IsCompleted && bufferSequence.Length = 0L then
              pipe.Reader.AdvanceTo(bufferSequence.End)
              return Result.Error(Error.ConnectionError "no more data")
            else
              match kmpW marker bufferSequence with
              | ValueSome x ->
                let res = Aux.split bufferSequence x select
                match res with
                | Continue n ->
                  pipe.Reader.AdvanceTo(bufferSequence.GetPosition(n + int64 marker.Length))
                  return Result.Ok(Found n)
                | FailWith s ->
                  pipe.Reader.AdvanceTo(bufferSequence.Start, bufferSequence.End)
                  return Result.Ok(Error s)
              | ValueNone ->
                let examinePosition = bufferSequence.Length - marker.LongLength
                if examinePosition > 0L then
                  let _ = Aux.split bufferSequence examinePosition select
                  pipe.Reader.AdvanceTo(bufferSequence.GetPosition(examinePosition), bufferSequence.End)
                else
                  pipe.Reader.AdvanceTo(bufferSequence.Start, bufferSequence.End)
                return Result.Ok(NeedMore)
        finally
          if semaphoreAcquired then
            pipeReaderSemaphore.Release() |> ignore
      with ex ->
        return Result.Error(Error.ConnectionError ex.Message)
    }

  /// Read the passed stream into buff until the EOL (CRLF) has been reached
  /// and returns the number of bytes read and the connection
  member x.readUntilPattern marker select : SocketOp<int64> =
    ValueTask<Result<int64,Error>>(
      task {
        let mutable reading = true
        let mutable error = false
        let mutable result = 0L
        let mutable errorResult = Ok(0L)
        let mutable iterationCount = 0
        while reading && not error && not(cancellationToken.IsCancellationRequested) do
          iterationCount <- iterationCount + 1
          let! res = x.scanMarker marker select
          match res with
          | Ok(Found a) ->
            reading <- false
            result <- a
          | Ok(NeedMore) ->
            ()
          | Result.Error s ->
            error <- true
            errorResult <- Result.Error s
          | Ok(Error s) ->
            error <- true
            errorResult <- Result.Error s
        if error then
          return errorResult
        else
          return Ok (result)
      })

  member x.skip n =
    task {
      let mutable semaphoreAcquired = false
      try
        do! pipeReaderSemaphore.WaitAsync(readerCancellationTokenSource.Token)
        semaphoreAcquired <- true
        
        let! result = pipe.Reader.ReadAsync(readerCancellationTokenSource.Token)
        if result.IsCanceled then
          return ScanResult.Error(Error.ConnectionError "ReadAsync was canceled")
        else
          let bufferSequence = result.Buffer
          pipe.Reader.AdvanceTo(bufferSequence.GetPosition(int64 n))
          return Found n
      finally
        if semaphoreAcquired then
          pipeReaderSemaphore.Release() |> ignore
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
              let source = a.Span.Slice(0,int count)
              let target = new Span<byte>(readLineBuffer,offset,int count)
              source.CopyTo(target)
              offset <- offset + int count
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
            flag <- false
            result <- Result.Error e
        return result
      })

  /// Read the post data from the stream, given the number of bytes that makes up the post data.
  member x.readPostData (bytes : int) (select:ReadOnlyMemory<byte> -> int -> unit) : Task<unit> =
    // Use a while loop instead of let rec to avoid FS3511 warning
    task {
      let mutable remaining = bytes
      let mutable iterationCount = 0
      while remaining > 0 do
        iterationCount <- iterationCount + 1
        let mutable semaphoreAcquired = false
        try
          do! pipeReaderSemaphore.WaitAsync(readerCancellationTokenSource.Token)
          semaphoreAcquired <- true
          
          let! result = pipe.Reader.ReadAsync(readerCancellationTokenSource.Token)
          if result.IsCanceled then
            remaining <- 0  // Exit loop
          else
            let bufferSequence = result.Buffer
            if bufferSequence.Length > 0L then
              let segment = bufferSequence.First
              if segment.Length > remaining then
                select segment remaining
                pipe.Reader.AdvanceTo(bufferSequence.GetPosition(int64 remaining))
                remaining <- 0
              else
                select segment segment.Length
                pipe.Reader.AdvanceTo(bufferSequence.GetPosition(int64 segment.Length))
                remaining <- remaining - segment.Length
            else
              remaining <- 0  // No data, exit loop
        finally
          if semaphoreAcquired then
            pipeReaderSemaphore.Release() |> ignore
    }

  member this.isDirty = 
    lock dirtyLock (fun () -> dirty)

  member this.init() =
    lock dirtyLock (fun () ->
      dirty <- true
      running <- true
    )
    readerCancellationTokenSource <- new Threading.CancellationTokenSource()

  member this.readLoop() = task{
    let mutable reading = true
    let mutable result = Ok()
    let mutable iterationCount = 0
    try
      while running && reading && not(readerCancellationTokenSource.IsCancellationRequested) do
        iterationCount <- iterationCount + 1
        let! a = this.readMoreData()
        match a with
        | Ok () -> ()
        | Result.Error b as a ->
          reading <- false
          result <- a
          this.stop()
      try pipe.Writer.Complete() with _ -> ()
      return result
    with ex ->
      try pipe.Writer.Complete() with _ -> ()
      return Result.Error(Error.ConnectionError ex.Message)
  }

  // Return readLineBuffer to the ArrayPool when the reader is disposed
  interface IDisposable with
    member this.Dispose() =
      System.Buffers.ArrayPool<byte>.Shared.Return(readLineBuffer, true)
      pipeReaderSemaphore.Dispose()
