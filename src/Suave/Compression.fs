namespace Suave

module Compression =

  open Suave.Utils
  open Suave.Sockets

  open System
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

  let transformStream (key : string) (stream : Stream) (getLast : string -> DateTime)
                      compression compressionFolder ctx =
    let syncCheckForExisting (key:string) (getLast: string -> DateTime) =
      let map = Globals.compressedFilesMap
      let lastModified = getLast key
      match map.TryGetValue key with
      | true, (existingPath, prevLastModified) when lastModified <= prevLastModified ->
          Choice1Of2 existingPath
      | _ ->
          Choice2Of2 lastModified

    let compressAndStoreAsync (key:string) (stream:Stream) (n:Algorithm) (lastModified:DateTime) (compressionFolder:string) =
      task {
        try
          let! newPathResult = compressFile n stream compressionFolder
          match newPathResult with
          | Ok newPath ->
              let map = Globals.compressedFilesMap
              map.[key] <- (newPath, lastModified)
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
          match syncCheckForExisting key getLast with
          | Choice1Of2 existingPath ->
            // existing compressed file is current; dispose original stream and return file stream
            stream.Dispose()
            let fs = new FileStream(existingPath, FileMode.Open, FileAccess.Read, FileShare.Read) :> Stream
            return Ok(Some n, fs)
          | Choice2Of2 lastModified ->
            // Need to compress — do the minimal awaited work here
            let! pathResult = compressAndStoreAsync key stream n lastModified compressionFolder
            match pathResult with
            | Result.Error e ->
              stream.Dispose()
              return Result.Error e
            | Ok path ->
              stream.Dispose()
              let fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read) :> Stream
              return Ok(Some n, fs)

        | None ->
          return Ok(None, stream)
      else
        return Ok(None, stream)
    }
