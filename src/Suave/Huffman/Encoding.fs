namespace Suave.Huffman

module Encoding =

  open System
  open System.IO

  let huffmanLength = 
    List.map List.length Table.huffmanTable |> List.toArray

  let huffmanCode = 
    Table.huffmanTable' |> List.toArray

  let initialOffset = 40

  let shiftForWrite = 32

  let idxEos = 256

  let enc (wbuf: MemoryStream) (rbuf: MemoryStream) =
    let write p (w:uint64) =
      if int64 p >= wbuf.Length then
        failwith "Buffer overrun"
      let w8 = byte(w >>> shiftForWrite)
      wbuf.WriteByte w8
      p + 1
    let rec copy p (w,o) =
      if o > shiftForWrite then
        (p,w,o)
      else
        let p' = write p w
        let w' = w <<< 8
        let o' = o + 8
        copy p' (w',o')
    let bond i encoded off =
      let len  = huffmanLength.[i] // 7
      let code = huffmanCode.[i]  // 0x6e = 110
      let scode = code <<< (off - len)
      let encoded' = encoded ||| scode
      let off' = off - len
      (encoded', off')
    let rec go (dst,encoded : uint64, off) : int =
      let i = rbuf.ReadByte()
      if i >= 0 then
        let dst',encoded', off' = copy dst (bond i encoded off)
        go (dst', encoded', off')
      elif off = initialOffset then dst
      else
        let (encoded1,_) = bond idxEos encoded off
        write dst encoded1 
    go (0,0UL,initialOffset)

  let encode (buf : byte array) rbuf =
    let wbuf = new MemoryStream(buf)
    let r = enc wbuf rbuf
    Array.sub buf 0 r
