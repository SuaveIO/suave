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

/// Lookup table of well-known lowercase HTTP header names. Allows the header
/// parser to avoid allocating a fresh string for the header name when the bytes
/// match a known header (case-insensitively). Names are kept in lowercase ASCII
/// because `getFirstCaseInsensitive`/`@@` expects keys in lowercase.
module internal KnownHeaders =

  // Common request/response headers. Order does not matter; lookup is bucketed by length.
  let private all : string[] =
    [|
      // length 4
      "host"
      // length 5
      "range"
      // length 6
      "accept"; "cookie"; "expect"; "origin"; "pragma"; "server"
      // length 7
      "referer"; "upgrade"
      // length 8
      "if-match"
      // length 10
      "connection"; "user-agent"; "set-cookie"; "keep-alive"
      // length 12
      "content-md5"
      // length 13
      "accept-ranges"; "authorization"; "cache-control"; "content-type"; "if-none-match"; "last-modified"
      // length 14
      "content-length"; "accept-charset"; "x-csrf-token"
      // length 15
      "accept-encoding"; "accept-language"; "x-forwarded-for"; "x-forwarded-by"
      // length 16
      "content-language"; "content-location"; "x-requested-with"; "x-forwarded-host"
      // length 17
      "if-modified-since"; "transfer-encoding"; "x-forwarded-proto"
      // length 19
      "content-disposition"; "if-unmodified-since"; "proxy-authenticate"
      // length 20
      "proxy-authorization"
      // length 25
      "strict-transport-security"
      // length 17
      "sec-websocket-key"
      // length 21
      "sec-websocket-version"
      // length 22
      "sec-websocket-protocol"
      // length 24
      "sec-websocket-extensions"
    |]

  // Bucket entries by length (max 32 — anything larger falls through to allocation).
  let private buckets : string[][] =
    let max = 32
    let counts = Array.zeroCreate<int> (max + 1)
    for h in all do
      if h.Length <= max then counts.[h.Length] <- counts.[h.Length] + 1
    let result = Array.init (max + 1) (fun i -> Array.zeroCreate<string> counts.[i])
    let idx = Array.zeroCreate<int> (max + 1)
    for h in all do
      if h.Length <= max then
        result.[h.Length].[idx.[h.Length]] <- h
        idx.[h.Length] <- idx.[h.Length] + 1
    result

  /// Case-insensitive ASCII equality between a byte span and a lowercase ASCII string.
  let inline asciiEqualsLowercase (span: System.ReadOnlySpan<byte>) (lower: string) : bool =
    if span.Length <> lower.Length then false
    else
      let mutable i = 0
      let mutable ok = true
      while ok && i < lower.Length do
        let b = span.[i]
        let bLower = if b >= 65uy && b <= 90uy then b + 32uy else b
        if bLower <> byte lower.[i] then ok <- false
        i <- i + 1
      ok

  /// Try to match a header-name byte span against the known set.
  /// Returns the canonical lowercase interned string on hit, null on miss.
  let tryMatch (span: System.ReadOnlySpan<byte>) : string =
    if span.Length > buckets.Length - 1 then null
    else
      let bucket = buckets.[span.Length]
      let mutable i = 0
      let mutable result : string = null
      while isNull result && i < bucket.Length do
        if asciiEqualsLowercase span bucket.[i] then result <- bucket.[i]
        i <- i + 1
      result

/// Interned ASCII byte-string lookups for HTTP request-line tokens.
/// The request line ("GET /foo HTTP/1.1") only ever contains a small finite
/// set of method and version tokens; recognising them by byte comparison and
/// returning a pre-allocated `string` constant avoids a `UTF8.GetString`
/// allocation per request for the method and per request for the version.
module internal KnownMethods =

  // Methods are case-sensitive in HTTP/1.1 (RFC 7230 §3.1.1) — no folding needed.
  let private all : string[] =
    [| "GET"; "POST"; "PUT"; "DELETE"; "HEAD"; "OPTIONS"; "PATCH"; "CONNECT"; "TRACE" |]

  let private bytesEqual (span: System.ReadOnlySpan<byte>) (s: string) : bool =
    if span.Length <> s.Length then false
    else
      let mutable i = 0
      let mutable ok = true
      while ok && i < s.Length do
        if span.[i] <> byte s.[i] then ok <- false
        i <- i + 1
      ok

  /// Return the interned method string, or `null` for an unknown method.
  let tryMatch (span: System.ReadOnlySpan<byte>) : string =
    let mutable i = 0
    let mutable result : string = null
    while isNull result && i < all.Length do
      if bytesEqual span all.[i] then result <- all.[i]
      i <- i + 1
    result

module internal KnownVersions =

  let v11 = "HTTP/1.1"
  let v10 = "HTTP/1.0"
  let v20 = "HTTP/2.0"

  let private bytesEqual (span: System.ReadOnlySpan<byte>) (s: string) : bool =
    if span.Length <> s.Length then false
    else
      let mutable i = 0
      let mutable ok = true
      while ok && i < s.Length do
        if span.[i] <> byte s.[i] then ok <- false
        i <- i + 1
      ok

  /// Return the interned version string, or `null` if unrecognised.
  let tryMatch (span: System.ReadOnlySpan<byte>) : string =
    if bytesEqual span v11 then v11
    elif bytesEqual span v10 then v10
    elif bytesEqual span v20 then v20
    else null

[<AllowNullLiteral>]
type HttpReader(transport : ITransport, pipe: Pipe, cancellationToken: Threading.CancellationToken) =

  let mutable running : bool = true
  let mutable dirty : bool = false
  let dirtyLock = new obj()
  // Rent from Suave's private BufferPool — see Suave.Globals.BufferPool for rationale.
  let readLineBuffer = Suave.Globals.BufferPool.rent 8192
  // Scratch buffer for ASCII-lowercasing unknown header names.
  let nameLowerBuffer = Suave.Globals.BufferPool.rent 256

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
          // Try to also complete the flush synchronously. PipeWriter.FlushAsync returns
          // ValueTask<FlushResult>; in steady state on a localhost benchmark this is the
          // overwhelmingly common case, so bypass the F# task builder entirely when it does.
          let flush = pipe.Writer.FlushAsync(readerCancellationTokenSource.Token)
          if flush.IsCompletedSuccessfully then
            // Touch .Result so any synchronously-thrown exception still surfaces; we don't
            // care about the FlushResult fields on the success path.
            let _ = flush.Result
            ValueTask<Result<unit,Error>>(Ok())
          else
            // Fall back to the task builder only when the flush actually went async.
            ValueTask<Result<unit,Error>>(
              task {
                let! _ = flush
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
              let flush = pipe.Writer.FlushAsync(readerCancellationTokenSource.Token)
              if flush.IsCompletedSuccessfully then
                let _ = flush.Result
                return Ok()
              else
                let! _ = flush
                return Ok()
            else
              return Result.Error (Error.ConnectionError "no more data")
          | Result.Error e ->
            return Result.Error e
        })

  /// Inspect an already-available `ReadResult` for the marker and advance the
  /// pipe reader. Returns `ValueSome r` when the scan is terminal (marker found,
  /// failure, or end-of-stream) and `ValueNone` when more data is required.
  /// Pure with respect to async \u2014 no awaits, no allocations.
  member private x.tryScanRead (marker: byte[]) (select: SelectFunction) (rr: ReadResult)
      : ValueOption<Result<ScanResult,Error>> =
    if rr.IsCanceled then
      ValueSome(Result.Error(Error.ConnectionError "ReadAsync was canceled"))
    else
      let bufferSequence = rr.Buffer
      if rr.IsCompleted && bufferSequence.Length = 0L then
        pipe.Reader.AdvanceTo(bufferSequence.End)
        ValueSome(Result.Error(Error.ConnectionError "no more data"))
      else
        match findMarker marker bufferSequence with
        | ValueSome p ->
          let res = Aux.split bufferSequence p select
          match res with
          | Continue n ->
            pipe.Reader.AdvanceTo(bufferSequence.GetPosition(n + int64 marker.Length))
            ValueSome(Result.Ok(Found n))
          | FailWith s ->
            pipe.Reader.AdvanceTo(bufferSequence.Start, bufferSequence.End)
            ValueSome(Result.Ok(Error s))
        | ValueNone ->
          let examinePosition = bufferSequence.Length - marker.LongLength
          if examinePosition > 0L then
            let _ = Aux.split bufferSequence examinePosition select
            pipe.Reader.AdvanceTo(bufferSequence.GetPosition(examinePosition), bufferSequence.End)
          else
            pipe.Reader.AdvanceTo(bufferSequence.Start, bufferSequence.End)
          ValueNone

  /// Async fallback: pump the transport until `tryScanRead` produces a terminal
  /// result, an error occurs, or cancellation is requested.
  member private x.scanMarkerAsync (marker: byte[]) (select: SelectFunction)
      : Task<Result<ScanResult,Error>> =
    task {
      try
        let mutable scanResult = Result.Error(Error.ConnectionError "scanMarker: unreachable")
        let mutable keepGoing = true
        while keepGoing do
          match! x.readMoreData() with
          | Result.Error e ->
            scanResult <- Result.Error e
            keepGoing <- false
          | Ok () ->
            let mutable rr = Unchecked.defaultof<ReadResult>
            if pipe.Reader.TryRead(&rr) then
              match x.tryScanRead marker select rr with
              | ValueSome r ->
                scanResult <- r
                keepGoing <- false
              | ValueNone -> ()  // marker not yet present; pump again
        return scanResult
      with ex ->
        return Result.Error(Error.ConnectionError ex.Message)
    }

  /// Iterates over a BufferSegment list looking for a marker, data before the marker
  /// is sent to the function select. Returns synchronously when the marker is
  /// already present in the buffered data \u2014 the common case for HTTP/1.1 keep-alive
  /// where the next request's headers typically arrive in a single recv().
  member x.scanMarker (marker: byte[]) (select : SelectFunction)
      : ValueTask<Result<ScanResult,Error>> =
    let mutable rr = Unchecked.defaultof<ReadResult>
    if pipe.Reader.TryRead(&rr) then
      match x.tryScanRead marker select rr with
      | ValueSome r -> ValueTask<Result<ScanResult,Error>>(r)
      | ValueNone -> ValueTask<Result<ScanResult,Error>>(x.scanMarkerAsync marker select)
    else
      ValueTask<Result<ScanResult,Error>>(x.scanMarkerAsync marker select)

  /// Read the passed stream into buff until the marker (e.g. CRLF) is reached
  /// and returns the number of bytes consumed before the marker.
  ///
  /// This is now a thin adapter over `scanMarker`: since `scanMarker` no longer
  /// returns `NeedMore` to its caller (the internal async fallback pumps until
  /// the scan is terminal), we just translate `ScanResult` into an `int64`.
  /// When `scanMarker` completes synchronously \u2014 the common case for HTTP/1.1
  /// keep-alive where the next request's headers arrive in a single `recv()` \u2014
  /// the entire `readUntilPattern` call also completes synchronously without
  /// allocating a state-machine box.
  member x.readUntilPattern marker select : SocketOp<int64> =
    let scan = x.scanMarker marker select
    if scan.IsCompletedSuccessfully then
      match scan.Result with
      | Ok(Found a) -> ValueTask<Result<int64,Error>>(Ok a)
      | Ok(Error s) -> ValueTask<Result<int64,Error>>(Result.Error s)
      | Ok NeedMore -> ValueTask<Result<int64,Error>>(Ok 0L)  // unreachable: scanMarker pumps internally
      | Result.Error e -> ValueTask<Result<int64,Error>>(Result.Error e)
    else
      ValueTask<Result<int64,Error>>(
        task {
          match! scan with
          | Ok(Found a) -> return Ok a
          | Ok(Error s) -> return Result.Error s
          | Ok NeedMore -> return Ok 0L  // unreachable
          | Result.Error e -> return Result.Error e
        })

  member x.skip n =
    task {
      let mutable rr = Unchecked.defaultof<ReadResult>
      let mutable scanResult = Found n
      let mutable proceed = true
      if not (pipe.Reader.TryRead(&rr)) then
        match! x.readMoreData() with
        | Result.Error e ->
          scanResult <- ScanResult.Error e
          proceed <- false
        | Ok () ->
          let! r = pipe.Reader.ReadAsync(readerCancellationTokenSource.Token)
          rr <- r
      if proceed then
        if rr.IsCanceled then
          scanResult <- ScanResult.Error(Error.ConnectionError "ReadAsync was canceled")
        else
          let bufferSequence = rr.Buffer
          pipe.Reader.AdvanceTo(bufferSequence.GetPosition(int64 n))
      return scanResult
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

  /// Read the HTTP request line directly from the byte buffer and decompose it
  /// into (method, path, rawQuery, version).
  ///
  /// Allocation profile vs. the previous `readLine` + `parseUrl` pipeline:
  ///   - method: 0 allocations on the common path (interned via `KnownMethods.tryMatch`)
  ///   - version: 0 allocations on the common path (interned via `KnownVersions.tryMatch`)
  ///   - path: 1 string allocation (sliced directly from `readLineBuffer`)
  ///   - rawQuery: 1 string allocation only when '?' is present, otherwise `String.Empty`
  /// Previously the same data cost: 1 full-line UTF-8 string + 4 `Substring` allocations.
  member x.readRequestLine () : SocketOp<string * string * string * string> =
    ValueTask<Result<string * string * string * string, Error>>(
      task {
        let mutable offset = 0
        match! x.readUntilPattern EOL (fun a count ->
            if offset + count > readLineBuffer.Length then
              FailWith (InputDataError (Some 414, "Line Too Long"))
            else
              let source = a.Span.Slice(0, int count)
              let target = new Span<byte>(readLineBuffer, offset, int count)
              source.CopyTo(target)
              offset <- offset + int count
              Continue offset) with
        | Result.Error e ->
          return Result.Error e
        | Ok _ ->
          // Walk the captured bytes once, locating the two ASCII spaces.
          let lineSpan = System.ReadOnlySpan<byte>(readLineBuffer, 0, offset)
          let firstSpace = lineSpan.IndexOf(0x20uy)
          if firstSpace <= 0 then
            return Result.Error (InputDataError (Some 400, "Invalid first line"))
          else
            let afterMethod = lineSpan.Slice(firstSpace + 1)
            let secondSpaceRel = afterMethod.IndexOf(0x20uy)
            if secondSpaceRel <= 0 then
              return Result.Error (InputDataError (Some 400, "Invalid first line"))
            else
              let methodSpan = lineSpan.Slice(0, firstSpace)
              let urlSpan = afterMethod.Slice(0, secondSpaceRel)
              let versionSpan = afterMethod.Slice(secondSpaceRel + 1)

              // Method: prefer interned; fall back to allocation only for unknowns.
              let knownMethod = KnownMethods.tryMatch methodSpan
              let methodStr =
                if not (isNull knownMethod) then knownMethod
                else System.Text.Encoding.ASCII.GetString(methodSpan)

              // Version: prefer interned; fall back to allocation only for unknowns.
              let knownVersion = KnownVersions.tryMatch versionSpan
              let versionStr =
                if not (isNull knownVersion) then knownVersion
                else System.Text.Encoding.ASCII.GetString(versionSpan)

              // Path / rawQuery: split on '?'. Path is materialised directly from
              // the byte buffer (one allocation). rawQuery only allocates when
              // the URL actually contains '?'.
              let queryRel = urlSpan.IndexOf(0x3Fuy)  // '?'
              if queryRel >= 0 then
                let pathStr = Globals.UTF8.GetString(urlSpan.Slice(0, queryRel))
                let queryStr = Globals.UTF8.GetString(urlSpan.Slice(queryRel + 1))
                return Ok (methodStr, pathStr, queryStr, versionStr)
              else
                let pathStr = Globals.UTF8.GetString(urlSpan)
                return Ok (methodStr, pathStr, System.String.Empty, versionStr)
      })

  /// Parse a single header line out of the first `offset` bytes of `readLineBuffer`.
  /// Returns the canonical lowercase name + the trimmed UTF-8 value with at most one
  /// string allocation per header (zero for the name when it is well-known).
  member private x.parseHeaderLine (offset: int) : Result<string * string, Error> =
    let lineSpan = System.ReadOnlySpan<byte>(readLineBuffer, 0, offset)
    let colonIdx = lineSpan.IndexOf(0x3Auy)
    if colonIdx <= 0 then
      Result.Error (InputDataError (Some 400, "Bad Request: Malformed Header"))
    else
      let nameSpan = lineSpan.Slice(0, colonIdx)
      // Trim leading SP/HTAB on value, trailing SP/HTAB on value.
      let mutable vStart = colonIdx + 1
      while vStart < offset && (readLineBuffer.[vStart] = 0x20uy || readLineBuffer.[vStart] = 0x09uy) do
        vStart <- vStart + 1
      let mutable vEnd = offset
      while vEnd > vStart && (readLineBuffer.[vEnd - 1] = 0x20uy || readLineBuffer.[vEnd - 1] = 0x09uy) do
        vEnd <- vEnd - 1
      // Resolve the canonical lowercase name without allocating when possible.
      let knownName = KnownHeaders.tryMatch nameSpan
      let name =
        if not (isNull knownName) then
          knownName
        elif nameSpan.Length <= nameLowerBuffer.Length then
          let mutable i = 0
          while i < nameSpan.Length do
            let b = nameSpan.[i]
            nameLowerBuffer.[i] <- if b >= 65uy && b <= 90uy then b + 32uy else b
            i <- i + 1
          System.Text.Encoding.ASCII.GetString(nameLowerBuffer, 0, nameSpan.Length)
        else
          // Pathological long header name — fall back to allocation-heavy path.
          System.Text.Encoding.ASCII.GetString(nameSpan).ToLowerInvariant()
      let value =
        if vEnd > vStart then
          Globals.UTF8.GetString(readLineBuffer, vStart, vEnd - vStart)
        else
          System.String.Empty
      Ok (name, value)

  /// Read all headers from the stream, returning a list of (name, value) pairs.
  /// Names are returned lowercase. Optimized to avoid the per-header string
  /// allocations performed by the previous string-based parser:
  ///  - Reads each header line directly into the byte buffer (no UTF8.GetString of the whole line).
  ///  - Uses byte-span scanning for ':' and whitespace trimming.
  ///  - Returns interned lowercase strings for well-known header names (zero allocation for name).
  ///  - For unknown names, lowercases ASCII in a scratch buffer and allocates exactly one string.
  ///  - Allocates exactly one string for the value.
  member x.readHeaders() : SocketOp<List<string*string>> =
    let dest = new List<string*string>()
    x.readHeadersInto(dest)

  /// Read all headers into the supplied list. The caller is responsible for clearing
  /// the list before the call. This avoids allocating a new List per request when the
  /// caller is reusing a per-connection list.
  member x.readHeadersInto(headers: List<string*string>) : SocketOp<List<string*string>> =
    ValueTask<Result<List<string*string>,Error>>(
      task {
        let mutable flag = true
        let mutable result : Result<List<string*string>,Error> = Ok headers
        while flag && (not cancellationToken.IsCancellationRequested) do
          let mutable offset = 0
          let! readResult =
            x.readUntilPattern EOL (fun a count ->
              if offset + count > readLineBuffer.Length then
                FailWith (InputDataError (Some 414, "Line Too Long"))
              else
                let source = a.Span.Slice(0, int count)
                let target = new Span<byte>(readLineBuffer, offset, int count)
                source.CopyTo(target)
                offset <- offset + int count
                Continue offset)
          match readResult with
          | Result.Error e ->
            flag <- false
            result <- Result.Error e
          | Ok _ ->
            if offset = 0 then
              // Empty line — end of headers
              flag <- false
            else
              match x.parseHeaderLine offset with
              | Ok kv -> headers.Add kv
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
      let mutable abort = false
      while remaining > 0 && not abort do
        iterationCount <- iterationCount + 1
        let mutable rr = Unchecked.defaultof<ReadResult>
        if not (pipe.Reader.TryRead(&rr)) then
          match! x.readMoreData() with
          | Result.Error _ ->
            abort <- true
          | Ok () ->
            let! r = pipe.Reader.ReadAsync(readerCancellationTokenSource.Token)
            rr <- r
        if not abort then
          if rr.IsCanceled then
            abort <- true
          else
            let bufferSequence = rr.Buffer
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
              abort <- true  // No data, exit loop
    }

  member this.isDirty = 
    lock dirtyLock (fun () -> dirty)

  member this.init() =
    lock dirtyLock (fun () ->
      dirty <- true
      running <- true
    )
    readerCancellationTokenSource <- new Threading.CancellationTokenSource()

  // Return per-connection buffers to Suave's private pool when the reader is disposed.
  interface IDisposable with
    member this.Dispose() =
      Suave.Globals.BufferPool.returnArray readLineBuffer true
      Suave.Globals.BufferPool.returnArray nameLowerBuffer true
