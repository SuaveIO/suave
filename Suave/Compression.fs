namespace Suave

module Compression =

  open Socket
  open Types

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

  let load_encoder s =
    match s with
    | "gzip"    -> Some (GZIP, Compression.gzip_encode)
    | "deflate" -> Some (Deflate, Compression.deflate_encode)
    | _         -> None

  let get_encoder (request : HttpRequest) =
    let encondings = request.headers %% "accept-encoding"
    match encondings with
    | Some (value : string) ->
      value.Split ','
      |> Array.map (fun s -> s.Trim())
      |> Array.tryPick (fun s -> load_encoder s)
    | _ -> None

  let parse_encoder (request : HttpRequest) =
    let encondings = request.headers %% "accept-encoding"
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
        let enconding = get_encoder request
        match enconding with
        | Some (n,encoder) ->
          do! async_writeln connection (String.Concat [| "Content-Encoding: "; n.ToString() |])
          return encoder content
        | None ->
          return content
      else
        return content
    }

  let compress encoding path (fs : Stream) = socket {
    use new_fs = new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.Write)
    match encoding with
    | GZIP ->
      use gzip = new GZipStream(new_fs, CompressionMode.Compress)
      do! lift_task (fs.CopyToAsync gzip)
      gzip.Close()
    | Deflate ->
      use deflate = new DeflateStream(new_fs, CompressionMode.Compress)
      do! lift_task (fs.CopyToAsync deflate)
      deflate.Close()
    | _ ->
      return failwith "invalid case."
    new_fs.Close()
  }

  let compress_file n (stream : Stream) compression_folder = socket {
    let temp_file_name = Path.GetRandomFileName()
    if not (Directory.Exists compression_folder) then Directory.CreateDirectory compression_folder |> ignore
    let new_path = Path.Combine(compression_folder,temp_file_name)
    do! compress n new_path stream
    stream.Dispose()
    return new_path
  }

  let transform_x (key : string) (get_data : string -> Stream) (get_last : string -> DateTime) compression compression_folder ({ request = q } as ctx) connection =
    socket {
      let stream = get_data key
      if compression && stream.Length > int64(MIN_BYTES_TO_COMPRESS) && stream.Length < int64(MAX_BYTES_TO_COMPRESS) then
        let enconding = parse_encoder q
        match enconding with
        | Some (n) ->
          do! async_writeln connection (String.Concat [| "Content-Encoding: "; n.ToString() |])
          if Globals.compressed_files_map.ContainsKey key then
            let last_modified = get_last key
            let cmpr_info = new FileInfo(Globals.compressed_files_map.[key])
            if last_modified > cmpr_info.CreationTime then
              let! new_path =  compress_file n stream compression_folder
              Globals.compressed_files_map.[key] <- new_path
          else
            let! new_path =  compress_file n stream compression_folder
            Globals.compressed_files_map.TryAdd(key,new_path) |> ignore
          return new FileStream(Globals.compressed_files_map.[key] , FileMode.Open, FileAccess.Read, FileShare.Read) :> Stream
        | None ->
          return stream
      else
        return stream
    }

