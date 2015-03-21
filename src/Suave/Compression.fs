namespace Suave

module Compression =

  open Suave.Types
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
    | "gzip"    -> Some (GZIP, Encoding.gzipEncode)
    | "deflate" -> Some (Deflate, Encoding.deflateEncode)
    | _         -> None

  let getEncoder (request : HttpRequest) =
    let encondings = request.header "accept-encoding"
    match encondings with
    | Some (value : string) ->
      value.Split ','
      |> Array.map (fun s -> s.Trim())
      |> Array.tryPick (fun s -> loadEncoder s)
    | _ -> None

  let parseEncoder (request : HttpRequest) =
    let encondings = request.header "accept-encoding"
    match encondings with
    | Some (value : string) ->
      value.Split ','
      |> Array.map (fun s -> s.Trim())
      |> Array.tryPick
        (fun s ->
          match s with
          | "gzip"    -> Some GZIP
          | "deflate" -> Some Deflate
          | _         -> None)
    | _ -> None

  let transform (content : byte []) (ctx : HttpContext) connection : SocketOp<byte []> =
    socket {
      if content.Length > MIN_BYTES_TO_COMPRESS && content.Length < MAX_BYTES_TO_COMPRESS then
        let request = ctx.request
        let enconding = getEncoder request
        match enconding with
        | Some (n,encoder) ->
          do! asyncWriteLn connection (String.Concat [| "Content-Encoding: "; n.ToString() |])
          return encoder content
        | None ->
          return content
      else
        return content
    }

  let compress encoding path (fs : Stream) = socket {
    use newFileStream = new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.Write)
    match encoding with
    | GZIP ->
      use gzip = new GZipStream(newFileStream, CompressionMode.Compress)
      do! liftTask (fs.CopyToAsync gzip)
      gzip.Close()
    | Deflate ->
      use deflate = new DeflateStream(newFileStream, CompressionMode.Compress)
      do! liftTask (fs.CopyToAsync deflate)
      deflate.Close()
    | _ ->
      return failwith "invalid case."
    newFileStream.Close()
  }

  let compressFile n (stream : Stream) compressionFolder = socket {
    let tempFileName = Path.GetRandomFileName()
    if not (Directory.Exists compressionFolder) then Directory.CreateDirectory compressionFolder |> ignore
    let new_path = Path.Combine(compressionFolder,tempFileName)
    do! compress n new_path stream
    stream.Dispose()
    return new_path
  }

  let transformStream (key : string) (getData : string -> Stream) (getLast : string -> DateTime) compression compressionFolder ({ request = q } as ctx) connection =
    socket {
      let stream = getData key
      if compression && stream.Length > int64(MIN_BYTES_TO_COMPRESS) && stream.Length < int64(MAX_BYTES_TO_COMPRESS) then
        let enconding = parseEncoder q
        match enconding with
        | Some (n) ->
          do! asyncWriteLn connection (String.Concat [| "Content-Encoding: "; n.ToString() |])
          if Globals.compressedFilesMap.ContainsKey key then
            let lastModified = getLast key
            let cmpr_info = new FileInfo(Globals.compressedFilesMap.[key])
            if lastModified > cmpr_info.CreationTime then
              let! newPath =  compressFile n stream compressionFolder
              Globals.compressedFilesMap.[key] <- newPath
          else
            let! newPath =  compressFile n stream compressionFolder
            Globals.compressedFilesMap.TryAdd(key,newPath) |> ignore
          return new FileStream(Globals.compressedFilesMap.[key] , FileMode.Open, FileAccess.Read, FileShare.Read) :> Stream
        | None ->
          return stream
      else
        return stream
    }


  [<Obsolete("Renamed to loadEncoder'")>]
  /// Obsolete
  let load_encoder s = loadEncoder s
  [<Obsolete("Renamed to getEncoder'")>]
  /// Obsolete
  let get_encoder request = getEncoder  request
  [<Obsolete("Renamed to parseEncoder'")>]
  /// Obsolete
  let parse_encoder request = parseEncoder request
  [<Obsolete("Renamed to compressFile'")>]
  /// Obsolete
  let compress_file n stream compressionFolder = compressFile stream compressionFolder
  [<Obsolete("Renamed to transformStream'")>]
  /// Obsolete
  let transform_x key getData getLast compression compressionFolder ctx connection = transformStream key getData getLast compression compressionFolder ctx connection

