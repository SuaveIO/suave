namespace Suave

open System
open System.Collections.Generic
open System.Text
open System.Threading.Tasks

open Suave.Sockets
open Suave.Sockets.Control
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

// this guy can live inside the connection actually
[<AllowNullLiteral>]
type HttpReader(transport : TcpTransport, lineBuffer : byte array, pipe: Pipe, cancellationToken) =

  let mutable running : bool = true
  let mutable dirty : bool = false

  member this.stop () =
    pipe.Writer.Complete()
    pipe.Reader.Complete()
    pipe.Reset()
    running <- false
    dirty <- false

  member (*inline*) x.readMoreData () = task {
    let buff = pipe.Writer.GetMemory()
    let! x = transport.read buff
    if x > 0 then
      pipe.Writer.Advance(x)
      let! flushResult = pipe.Writer.FlushAsync(cancellationToken)
      return Ok()
    else
      return Result.Error (Error.ConnectionError "no more data")
    }

  member (*inline*) x.getData () = task{
      let (success, result) = pipe.Reader.TryRead()
      if success then
        return result
      else
        let! result= pipe.Reader.ReadAsync(cancellationToken)
        return result
  }
  /// Iterates over a BufferSegment list looking for a marker, data before the marker
  /// is sent to the function select
  /// Returns the number of bytes read.
  member (*inline*) x.scanMarker (marker: byte[]) (select : SelectFunction) = 
    task{
      try
        let! result = x.getData()
        let bufferSequence = result.Buffer
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
          pipe.Reader.AdvanceTo(bufferSequence.GetPosition(bufferSequence.Length - int64 marker.Length))
          return Result.Ok(NeedMore)
      with ex ->
        return Result.Error(Error.ConnectionError ex.Message)
  }

  /// Read the passed stream into buff until the EOL (CRLF) has been reached
  /// and returns the number of bytes read and the connection
  member (*inline*) x.readUntilPattern marker select =
    task {
      let reading = ref true
      let error = ref false
      let result = ref 0
      let errorResult = ref (Ok(0))
      while !reading && not !error && not(cancellationToken.IsCancellationRequested) do
        let! res = x.scanMarker marker select
        match res with
        | Ok(Found a) ->
          reading := false
          result := a
        | Ok(NeedMore) ->
          ()
        | Result.Error s
        | Ok(Error s) ->
          error := true
          errorResult := Result.Error s
      if !error then
        return !errorResult
      else
        return Ok (!result)
    }

  member x.skip n =
     task{
      let! result= pipe.Reader.ReadAsync()
      let bufferSequence = result.Buffer
      // we really do not calling split because we are not doing anything with it
      //let res = Aux.split bufferSequence n (fun a b -> Continue 0 )
      //match res with
        //| Continue n ->
      pipe.Reader.AdvanceTo(bufferSequence.GetPosition(int64 n))
      return Found n
       // | FailWith s ->
         // return Error s
      }

    /// Read n bytes (dont' we need to iterate ?)
    member x.read n select =
     task{
      let! result= pipe.Reader.ReadAsync()
      let bufferSequence = result.Buffer
      let res = Aux.split bufferSequence n select
      match res with
      | Continue n ->
        pipe.Reader.AdvanceTo(bufferSequence.GetPosition(int64 n))
        return Found n
      | FailWith s ->
        return Error s
      }

  /// Read a line from the stream, calling UTF8.toString on the bytes before the EOL marker
  member (*inline*) x.readLine () = socket {
    let offset = ref 0
    let! count =
      x.readUntilPattern EOL (fun a count ->
        if !offset + count > lineBuffer.Length then
          FailWith (InputDataError (Some 414, "Line Too Long"))
        else
          let source = a.Span.Slice(0,count)
          let target = new Span<byte>(lineBuffer,!offset,count)
          source.CopyTo(target)
          offset := !offset + count
          
          Continue !offset
      )
    let result = Encoding.UTF8.GetString(lineBuffer, 0, !offset)
    return result
  }

  /// Read all headers from the stream, returning a dictionary of the headers found
  member (*inline*) x.readHeaders() =
    task {
      let headers = new List<string*string>()
      let flag = ref true
      let error = ref false
      let result = ref (Ok (headers))
      while !flag && (not cancellationToken.IsCancellationRequested) do
        let! _line = x.readLine ()
        match _line with
        | Ok line ->
          if line <> String.Empty then
            let indexOfColon = line.IndexOf(':')
            let header = (line.Substring(0, indexOfColon).ToLower(), line.Substring(indexOfColon+1).TrimStart())
            headers.Add header
          else
            flag := false
        | Result.Error e ->
          error := true
          result := Result.Error e
      return !result
    }

  /// Read the post data from the stream, given the number of bytes that makes up the post data.
  member (*inline*) x.readPostData (bytes : int) (select:ReadOnlyMemory<byte> -> int -> unit) : Task<unit> =
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

  member this.isDirty = dirty 

  member this.readLoop() = task{
    dirty <- true
    let reading = ref true
    running <- true
    let result = ref (Ok())
    while running && !reading && not(cancellationToken.IsCancellationRequested) do
      let! a = this.readMoreData()
      match a with
      | Ok () -> ()
      | a ->
        reading := false
        result := a
        this.stop()
    return result
  }

