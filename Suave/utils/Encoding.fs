namespace Suave.Utils

module Encoding =
  open System.IO
  open System.IO.Compression

  let private encode (mk_stream : Stream * CompressionMode -> Stream) (bytes: byte[]) =
    if bytes.Length > 0 then
      use memory =  new MemoryStream()
      use compress_stream = mk_stream(memory, CompressionMode.Compress)
      do compress_stream.Write(bytes, 0, bytes.Length)
      compress_stream.Close()
      memory.ToArray()
    else
      [||]

  let private decode (mk_stream : Stream * CompressionMode -> Stream) (bytes: byte[]) =
    if bytes.Length > 0 then
      use compressed =  new MemoryStream(bytes)
      use decompress_stream = mk_stream(compressed, CompressionMode.Decompress)
      use result = new MemoryStream()
      decompress_stream.CopyTo(result)
      result.ToArray()
    else
      [||]

  let private gzip (s:Stream, m:CompressionMode) = new GZipStream(s, m) :> Stream
  let private deflate (s:Stream, m:CompressionMode) = new DeflateStream(s, m) :> Stream

  let gzip_encode = encode gzip
  let gzip_decode = decode gzip

  let deflate_encode = encode deflate
  let deflate_decode = decode deflate
