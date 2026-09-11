namespace Suave

module Compression =

  open Suave.Utils
  open Suave.Sockets

  open System
  open System.Collections.Concurrent
  open System.Collections.Generic
  open System.IO
  open System.IO.Compression

  type Algorithm =
    /// No compression
    | Plain
    /// GZIP compression
    | GZIP
    /// Deflate compression
    | Deflate
    /// Prints the algorithm as a string that can be put in a HTTP header
    override x.ToString() =
      match x with
      | Plain   -> "plain"
      | GZIP    -> "gzip"
      | Deflate -> "deflate"

  // You should only gzip files above a certain size threshold; we recommend a minimum range
  // between 150 and 1000 bytes. Gzipping files below 150 bytes can actually make them larger

  let MIN_BYTES_TO_COMPRESS =       500 // 500 bytes
  let MAX_BYTES_TO_COMPRESS = 524288000 // 500 megabytes

  let loadEncoder s =
    match s with
    | "gzip"    -> Some (GZIP, Compression.gzipEncode)
    | "deflate" -> Some (Deflate, Compression.deflateEncode)
    | _         -> None

  let getEncoder (request : HttpRequest) =
    match request.header "accept-encoding" with
    | Choice1Of2 value ->
      String.splita ',' value
      |> Array.map String.trim
      |> Array.tryPick loadEncoder
    | _ -> None

  let parseEncoder (request : HttpRequest) =
    match request.header "accept-encoding" with
    | Choice1Of2 value ->
      String.splita ',' value
      |> Array.map String.trim
      |> Array.tryPick
        (function
         | "gzip"    -> Some GZIP
         | "deflate" -> Some Deflate
         | _         -> None)
    | _ -> None

  /// Synchronous compression decision + transform.
  ///
  /// Hot-path-aware:
  ///   1. If `content.Length` is outside the compressible range, returns immediately
  ///      with no allocation and without inspecting any request headers.
  ///   2. Otherwise checks for `accept-encoding`; if absent or unmatched, returns
  ///      `(None, content)` without allocating a Task.
  ///   3. Only when we are actually going to compress do we allocate the encoded
  ///      byte array.
  ///
  /// This replaces the previous `task { }`-wrapped version that paid for a state
  /// machine + Task box + header lookup + comma-split + Array.map/tryPick on
  /// every `Bytes` response, even when no compression was possible.
  let transformSync (content : byte []) (ctx : HttpContext) : Algorithm option * byte [] =
    let len = content.Length
    if len <= MIN_BYTES_TO_COMPRESS || len >= MAX_BYTES_TO_COMPRESS then
      None, content
    else
      match getEncoder ctx.request with
      | Some (n, encoder) -> Some n, encoder content
      | None -> None, content

  /// Backwards-compatible task wrapper; new call sites should prefer `transformSync`.
  let transform (content : byte []) (ctx : HttpContext) : Threading.Tasks.Task<Algorithm option * byte []> =
    Threading.Tasks.Task.FromResult(transformSync content ctx)

  let compress encoding path (fs : Stream) = task {
    use newFileStream = new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.Write)
    match encoding with
    | GZIP ->
      use gzip = new GZipStream(newFileStream, CompressionMode.Compress)
      do! (fs.CopyToAsync gzip)
      return Ok ()
    | Deflate ->
      use deflate = new DeflateStream(newFileStream, CompressionMode.Compress)
      do! (fs.CopyToAsync deflate)
      return Ok ()
    | _ ->
      return failwith "invalid case."
  }

  let compressFile n (stream : Stream) compressionFolder : SocketOp<string> = 
    System.Threading.Tasks.ValueTask<Result<string,Error>>(
      task {
        let tempFileName = Path.GetRandomFileName()
        if not (Directory.Exists compressionFolder) then Directory.CreateDirectory compressionFolder |> ignore
        let newPath = Path.Combine(compressionFolder,tempFileName)
        let! a = compress n newPath stream
        match a with
        | Ok () ->
          return Ok (newPath)
        | Result.Error e ->
          return Result.Error e
      })

  /// The key under which a compressed copy of a resource is cached. The
  /// algorithm is part of the key: cached entries are not interchangeable
  /// between algorithms, or a file compressed for a `gzip` request would be
  /// handed to – and labelled `deflate` for – the next client.
  let internal cacheKey (key : string) (n : Algorithm) : struct (string * string) =
    struct (key, n.ToString())

  /// How long a file is allowed to sit in the compression folder before a sweep
  /// (see `cleanupFolder`) treats it as obsolete. A compressed copy is only a
  /// cache: evicting one costs nothing but a re-compression on the next request
  /// for that resource.
  let mutable MAX_COMPRESSED_FILE_AGE = TimeSpan.FromDays 1.

  /// The number of files a sweep leaves in the compression folder at most; when
  /// there are more, the oldest are evicted first.
  let mutable MAX_COMPRESSED_FILES = 1000

  /// Files whose deletion failed – on Windows a copy that another request is
  /// still streaming can refuse to go away. They are retried by the next sweep
  /// instead of being leaked. Bounded, so a folder that cannot be written to
  /// cannot grow this queue without limit.
  let private pendingDeletions = ConcurrentQueue<string>()
  let private MAX_PENDING_DELETIONS = 10000

  /// Best-effort delete that never throws. Returns true when the file is gone
  /// after the call (a file that was already missing counts as gone); otherwise
  /// the path is queued to be retried by the next sweep.
  let internal tryDelete (path : string) =
    try
      File.Delete path // a no-op when the file does not exist
      true
    with
    | :? FileNotFoundException | :? DirectoryNotFoundException ->
      true
    | _ ->
      if pendingDeletions.Count < MAX_PENDING_DELETIONS then
        pendingDeletions.Enqueue path
      false

  /// Retries the deletions that failed earlier. Only the queue as it stands on
  /// entry is walked: `tryDelete` re-queues what still cannot be deleted, and a
  /// sweep must not spin on it.
  let internal retryPendingDeletions () =
    let mutable remaining = pendingDeletions.Count
    let mutable deleted = 0
    let mutable path = Unchecked.defaultof<string>
    while remaining > 0 && pendingDeletions.TryDequeue(&path) do
      remaining <- remaining - 1
      if tryDelete path then deleted <- deleted + 1
    deleted

  /// Publishes `entry` under `k`, returning the path of the entry it superseded,
  /// if any.
  ///
  /// The swap is a compare-and-swap loop, so when several requests compress the
  /// same resource concurrently exactly one of them observes – and is therefore
  /// responsible for deleting – each superseded file: no file is deleted twice,
  /// and none is left behind. A superseded path is no longer reachable from the
  /// cache once this returns, so deleting it cannot race a later reader.
  let rec internal swapCacheEntry (k : struct (string * string)) (entry : string * DateTime) =
    let map = Globals.compressedFilesMap
    match map.TryGetValue k with
    | true, existing ->
      if map.TryUpdate(k, entry, existing) then Some (fst existing)
      else swapCacheEntry k entry
    | _ ->
      if map.TryAdd(k, entry) then None
      else swapCacheEntry k entry

  /// Opens a compressed copy for reading.
  ///
  /// `FileShare.Delete` matters: a superseded copy has to be removable while an
  /// earlier request is still streaming it, or the delete would fail with a
  /// sharing violation on Windows and the file would linger.
  let private openForRead (path : string) : Stream =
    new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read ||| FileShare.Delete) :> Stream

  /// Evicts obsolete files from the compression folder: everything older than
  /// `maxAge`, plus – oldest first – whatever is over `maxFiles`. The cache
  /// entry pointing at a file is dropped before the file is deleted, so no
  /// request can be handed a path that is about to disappear; a resource whose
  /// compressed copy is evicted is simply compressed again when next requested.
  ///
  /// This is what clears out copies of resources that were recompressed,
  /// renamed or deleted while the server was not running. It never throws, and
  /// returns the number of files it deleted.
  let cleanupFolder (compressionFolder : string) (maxAge : TimeSpan) (maxFiles : int) : int =
    let mutable deleted = retryPendingDeletions ()
    try
      if Directory.Exists compressionFolder then
        // Oldest first, so the files over the bound are the ones at the front.
        let files =
          DirectoryInfo(compressionFolder).GetFiles()
          |> Array.sortBy (fun fi -> fi.LastWriteTimeUtc)
        // The reverse of the cache: which entry, if any, hands out a given file.
        let keysByPath = Dictionary<string, struct (string * string)>(StringComparer.Ordinal)
        for KeyValue(k, (path, _)) in Globals.compressedFilesMap do
          keysByPath.[path] <- k
        let now = DateTime.UtcNow
        let overBound = files.Length - (max 0 maxFiles)
        for i in 0 .. files.Length - 1 do
          let fi = files.[i]
          if i < overBound || now - fi.LastWriteTimeUtc > maxAge then
            // Drop the cache entry before the file, never the other way round.
            match keysByPath.TryGetValue fi.FullName with
            | true, k -> Globals.compressedFilesMap.TryRemove k |> ignore
            | _ -> ()
            if tryDelete fi.FullName then deleted <- deleted + 1
    with _ -> ()
    deleted

  /// `cleanupFolder` with Suave's defaults, `MAX_COMPRESSED_FILE_AGE` and
  /// `MAX_COMPRESSED_FILES`. Called for you on server startup and shutdown.
  let cleanup (compressionFolder : string) : int =
    cleanupFolder compressionFolder MAX_COMPRESSED_FILE_AGE MAX_COMPRESSED_FILES

  let transformStream (key : string) (stream : Stream) (getLast : string -> DateTime)
                      compression compressionFolder ctx =
    // The path of an up-to-date compressed copy of `key`, if the cache holds
    // one and it is still on disk. An entry whose file has gone – swept away,
    // or removed by hand – is dropped, so the resource is compressed again
    // instead of the request failing on a missing file.
    let tryFindExisting (key:string) (n:Algorithm) (lastModified:DateTime) =
      let map = Globals.compressedFilesMap
      let k = cacheKey key n
      match map.TryGetValue k with
      | true, ((existingPath, prevLastModified) as existing) when lastModified <= prevLastModified ->
          if File.Exists existingPath then
            Some existingPath
          else
            // Remove the exact pair we read, so an entry written meanwhile –
            // pointing at a file that does exist – is left alone.
            let item = KeyValuePair<struct (string * string), string * DateTime>(k, existing)
            map.TryRemove item |> ignore
            None
      | _ ->
          None

    // Opening a cached copy can still lose a race against it being superseded
    // and deleted; that is a cache miss like any other, not a failed request.
    let tryOpenExisting (path : string) =
      try Some (openForRead path)
      with
      | :? IOException | :? UnauthorizedAccessException -> None

    let compressAndStoreAsync (key:string) (stream:Stream) (n:Algorithm) (lastModified:DateTime) (compressionFolder:string) =
      task {
        try
          let! newPathResult = compressFile n stream compressionFolder
          match newPathResult with
          | Ok newPath ->
              // Publish the new copy and delete the one it supersedes, in that
              // order and in one atomic step: the old file is unreachable from
              // the cache before it is deleted, and exactly one compressor ever
              // sees – and so deletes – it.
              match swapCacheEntry (cacheKey key n) (newPath, lastModified) with
              | Some superseded when superseded <> newPath ->
                  tryDelete superseded |> ignore
              | _ -> ()
              return Ok newPath
          | Result.Error e ->
              return Result.Error e
        with ex ->
          return Result.Error (Error.ConnectionError ex.Message)
      }

    task {
      if compression && stream.Length > int64(MIN_BYTES_TO_COMPRESS) && stream.Length < int64(MAX_BYTES_TO_COMPRESS) then
        match parseEncoder ctx.request with
        | Some n ->
          // First check synchronously if we already have a compressed file that is up-to-date
          let lastModified = getLast key
          match tryFindExisting key n lastModified |> Option.bind tryOpenExisting with
          | Some fs ->
            // existing compressed file is current; dispose original stream and return file stream
            stream.Dispose()
            return Ok(Some n, fs)
          | None ->
            // Need to compress — do the minimal awaited work here
            let! pathResult = compressAndStoreAsync key stream n lastModified compressionFolder
            match pathResult with
            | Result.Error e ->
              stream.Dispose()
              return Result.Error e
            | Ok path ->
              match tryOpenExisting path with
              | Some fs ->
                stream.Dispose()
                return Ok(Some n, fs)
              | None when stream.CanSeek ->
                // Our copy was superseded and deleted before we could open it.
                // The source stream was read to the end while compressing, so
                // rewind it and serve the resource uncompressed rather than fail.
                stream.Position <- 0L
                return Ok(None, stream)
              | None ->
                stream.Dispose()
                return Result.Error (Error.ConnectionError "the compressed copy was evicted before it could be served")

        | None ->
          return Ok(None, stream)
      else
        return Ok(None, stream)
    }
