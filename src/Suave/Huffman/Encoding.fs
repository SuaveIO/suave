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
    let rec copy p (w,o) =
      if o > shiftForWrite then
        (p,w,o)
      else
        wbuf.WriteByte w
        let w' = w <<< 8
        let o' = o + 8
        copy (p + 1) (w',o')
    let bond i encoded off =
      let len  = huffmanLength.[i]
      let code = huffmanCode.[i]
      let scode = code <<< (off - len)
      let encoded' = encoded ||| scode
      let off' = off - len
      (byte encoded', off')
    let write p w =
      if int64 p >= wbuf.Length then
        failwith "Buffer overrun"
      let w8 = w >>> shiftForWrite
      wbuf.WriteByte w8
      p + 1
    let rec go (dst,encoded, off) : int =
      let i = rbuf.ReadByte()
      if i >= 0 then
        let a,encoded', off' = copy dst (bond i encoded off)
        go ((dst + 1), int encoded', off')
      elif off = initialOffset then dst
      else
        let (encoded1,_) = bond idxEos encoded off
        write dst encoded1 |> int
    go (0,0,initialOffset)

  let encode (buf : byte array) rbuf =
    let wbuf = new MemoryStream(buf)
    let r = enc wbuf rbuf
    let reader = new StreamReader(wbuf)
    reader.ReadToEnd()