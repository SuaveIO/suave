namespace Suave

module Compression =

  open Suave.Utils
  open Suave.Sockets
  open Suave.Sockets.Control


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

  let transform (content : byte []) (ctx : HttpContext) connection : SocketOp<Algorithm option * byte []> =
    socket {
      if content.Length > MIN_BYTES_TO_COMPRESS && content.Length < MAX_BYTES_TO_COMPRESS then
        let request = ctx.request
        let enconding = getEncoder request
        match enconding with
        | Some (n,encoder) ->
          return Some n, encoder content
        | None ->
          return None, content
      else
        return None, content
    }

  let compress encoding path (fs : Stream) = socket {
    use newFileStream = new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.Write)
    match encoding with
    | GZIP ->
      use gzip = new GZipStream(newFileStream, CompressionMode.Compress)
      do! SocketOp.ofTask (fs.CopyToAsync gzip)
    | Deflate ->
      use deflate = new DeflateStream(newFileStream, CompressionMode.Compress)
      do! SocketOp.ofTask (fs.CopyToAsync deflate)
    | _ ->
      return failwith "invalid case."
  }

  let compressFile n (stream : Stream) compressionFolder = socket {
    let tempFileName = Path.GetRandomFileName()
    if not (Directory.Exists compressionFolder) then Directory.CreateDirectory compressionFolder |> ignore
    let newPath = Path.Combine(compressionFolder,tempFileName)
    do! compress n newPath stream
    return newPath
  }

  let transformStream (key : string) (stream : Stream) (getLast : string -> DateTime)
                      compression compressionFolder ctx =
    socket {
      if compression && stream.Length > int64(MIN_BYTES_TO_COMPRESS) && stream.Length < int64(MAX_BYTES_TO_COMPRESS) then
        let encoding = parseEncoder ctx.request
        match encoding with
        | Some n ->
          try
            // look this up, we may have already compressed it
            let map = Globals.compressedFilesMap
            let lastModified = getLast key
            match map.TryGetValue key with
            | true, (_, previousLastModified) -> 
              if lastModified > previousLastModified then
                // this file has a later modification date than it did when we compressed it
                let! newPath = compressFile n stream compressionFolder
                map.[key] <- (newPath, lastModified)
                // Here it is tempting to delete the old compressed file, but is that 
                // safe? Perhaps it could be being served on another request?
            | _ -> 
                let! newPath = compressFile n stream compressionFolder
                let value = (newPath, lastModified)
                (key, value) |> map.TryAdd |> ignore
            return Some n, new FileStream(map.[key] |> fst, FileMode.Open, FileAccess.Read, FileShare.Read) :> Stream
          finally
            stream.Dispose()
        | None ->
          return None, stream
      else
        return None, stream
    }
