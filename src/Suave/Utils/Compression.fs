module internal Suave.Utils.Compression

open System.IO
open System.IO.Compression

let private encode (mkStream : Stream * CompressionMode -> Stream) (bytes: byte[]) =
  if bytes.Length > 0 then
    use memory =  new MemoryStream()
    let compressStream = mkStream(memory, CompressionMode.Compress)
    do compressStream.Write(bytes, 0, bytes.Length)
    compressStream.Dispose()
    memory.ToArray()
  else
    [||]

let private decode (mkStream : Stream * CompressionMode -> Stream) (bytes: byte[]) =
  if bytes.Length > 0 then
    use compressed =  new MemoryStream(bytes)
    use decompressStream = mkStream(compressed, CompressionMode.Decompress)
    use result = new MemoryStream()
    decompressStream.CopyTo(result)
    result.ToArray()
  else
    [||]

let private gzip (s:Stream, m:CompressionMode) = new GZipStream(s, m) :> Stream
let private deflate (s:Stream, m:CompressionMode) = new DeflateStream(s, m) :> Stream

let gzipEncode = encode gzip
let gzipDecode = decode gzip

let deflateEncode = encode deflate
let deflateDecode = decode deflate
