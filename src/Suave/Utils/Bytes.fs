namespace Suave.Utils

open System
open System.IO
open System.Text
open Suave.Utils.Async

type BufferSegment =
    { buffer : ArraySegment<byte>
      offset : int
      length : int }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BufferSegment =
    
    let inline mk buffer offset length =
      if length < 0 then failwith (sprintf "BufferSegment.mk: length = %d < 0" length)
      { buffer = buffer; offset = offset; length = length }


module (* internal *) Bytes =
 
  /// Ordinally compare two strings in constant time, bounded by the length of the
  /// longest string.
  let constantTimeCompare (bits : byte []) (bobs : byte []) =
    let mutable xx = uint32 bits.Length ^^^ uint32 bobs.Length
    let mutable i = 0
    while i < bits.Length && i < bobs.Length do
      xx <- xx ||| uint32 (bits.[i] ^^^ bobs.[i])
      i <- i + 1
    xx = 0u


  // for ci in (int '!')..(int '~') do printfn "%c" (char ci);;
  // https://en.wikipedia.org/wiki/HTTP_cookie#Setting_a_cookie
  let cookieEncoding =
    let repls =
      [ '+', '_'
        '/', '!'
        '=', '$' ]

    let enc bytes =
      let base64 =
        Convert.ToBase64String bytes
      repls |> List.fold (fun (str : string) (from, too) -> str.Replace (from, too)) base64

    let dec (str : string) =
      let base64 =
        repls |> List.fold (fun (str : string) (too, from) -> str.Replace(from, too)) str
      Convert.FromBase64String base64

    enc, dec

  /// The end-of-line literal, \r\n (CRLF)
  let [<Literal>] eol = "\r\n"

  /// The end-of-line 'literal' as bytes, the \r\n (CRLF) byte pair
  let EOL = ASCII.bytes eol

  /// The corresponding EOL array segment
  let eolArraySegment = new ArraySegment<_>(EOL, 0, 2)

  let inline bytes_to_buffer (s : string) (buff : byte []) (offset : int) =
    Encoding.ASCII.GetBytes (s, 0, s.Length, buff, offset)

  /// Fully transform the input stream to a byte array.
  let readFully (input : Stream) =
    use ms = new MemoryStream()
    input.CopyTo ms
    ms.ToArray()

  /// Asynchronously write from the 'from' stream to the 'to' stream, with an upper bound on
  /// amount to transfer by len
  let transferBounded (toStream : Stream) (from : Stream) len =
    let buf_size = 0x2000
    let buf = Array.zeroCreate<byte> 0x2000
    let rec do_block left = async {
      let! read = from.AsyncRead(buf, 0, Math.Min(buf_size, left))
      if read <= 0 || left - read = 0 then
        do! toStream.FlushAsync()
        return ()
      else
        do! toStream.AsyncWrite(buf, 0, read)
        return! do_block (left - read) }
    do_block len

  /// Asynchronously write from the 'from' stream to the 'to' stream.
  let transfer (toStream : Stream) (from : Stream) =
    let buf = Array.zeroCreate<byte> 0x2000
    let rec doBlock () = async {
      let! read = from.AsyncRead buf
      if read <= 0 then
        do! toStream.FlushAsync()
        return ()
      else
        do! toStream.AsyncWrite(buf, 0, read)
        return! doBlock () }
    doBlock ()

  /// Knuth-Morris-Pratt algorithm
  /// http://caml.inria.fr/pub/old_caml_site/Examples/oc/basics/kmp.ml
  let initNext p =
    let m = Array.length p
    let next = Array.create m 0
    let i = ref 1
    let j = ref 0
    while !i < m - 1 do
      if p.[!i] = p.[!j] then begin incr i; incr j; next.[!i] <- !j end else
      if !j = 0 then begin incr i; next.[!i] <- 0 end else j := next.[!j]
    next

  let kmp p =
    let next = initNext p
    let m = Array.length p
    fun s ->
      let n = Array.length s
      let  i = ref 0
      let j = ref 0 in
      while !j < m && !i < n do
        if s.[!i] = p.[!j] then begin incr i; incr j end else
        if !j = 0 then incr i else j := next.[!j]
      done;
      if !j >= m then Some(!i - m) else None

  let kmp_x p =
    let next = initNext p
    let m = Array.length p
    fun (s:ArraySegment<_>) ->
      let n = s.Count
      let  i = ref 0
      let j = ref 0 in
      while !j < m && !i < n do
        if s.Array.[s.Offset + !i] = p.[!j] then begin incr i; incr j end else
        if !j = 0 then incr i else j := next.[!j]
      done;
      if !j >= m then Some(!i - m) else None

  let inline uniteArraySegment (aas : ArraySegment<byte> list) =
    fun (i : int) ->
      if   i < 0 then failwith "invalid args"
      let rec loop k acc =
        let a = aas.[k]
        if i < acc + a.Count then 
          a.Array.[a.Offset + (i - acc)]
        else loop (k + 1) (acc + a.Count)
      loop 0 0

  let inline uniteArrayBufferSegment (aas : BufferSegment list) =
    fun (i : int) ->
      if   i < 0 then failwith "invalid args"
      let rec loop k acc =
        let a = aas.[k]
        if i < acc + a.length then 
          a.buffer.Array.[a.offset + (i - acc)]
        else loop (k + 1) (acc + a.length)
      loop 0 0

  let kmp_y p =
    let next = initNext p
    let m = Array.length p
    fun (xs : ArraySegment<byte> list) ->
      let a = uniteArraySegment xs
      let n = List.fold (fun acc (x :  ArraySegment<byte>) -> acc + x.Count) 0 xs
      let  i = ref 0
      let j = ref 0 in
      while !j < m && !i < n do
        if a(!i) = p.[!j] then begin incr i; incr j end else
        if !j = 0 then incr i else j := next.[!j]
      done;
      if !j >= m then Some(!i - m) else None

  let kmp_z p =
    let next = initNext p
    let m = Array.length p
    fun (xs : BufferSegment list) ->
      let a = uniteArrayBufferSegment xs
      let n = List.fold (fun acc (x :  BufferSegment) -> acc + x.length) 0 xs
      let  i = ref 0
      let j = ref 0 in
      while !j < m && !i < n do
        if a(!i) = p.[!j] then begin incr i; incr j end else
        if !j = 0 then incr i else j := next.[!j]
      done;
      if !j >= m then Some(!i - m) else None

  let inline unite (a : ArraySegment<_>) (b : ArraySegment<_>) =
    fun (i : int) ->
      if   i < 0       then failwith "invalid args"
      elif i < a.Count then a.Array.[a.Offset + i]
      elif i < a.Count + b.Count then b.Array.[b.Offset + (i - a.Count)]
      else failwith "invalid args"

  let kmp_x_x p =
    let next = initNext p
    let m = Array.length p
    fun (v:ArraySegment<_>) (w:ArraySegment<_>) ->
      let n = v.Count + w.Count
      let s = unite v w
      let  i = ref 0
      let j = ref 0 in
      while !j < m && !i < n do
        if s !i = p.[!j] then begin incr i; incr j end else
        if !j = 0 then incr i else j := next.[!j]
      done;
      if !j >= m then Some(!i - m) else None

  /// Returns the index of the first CRLF in the buffer
  let inline scan_crlf (b : ArraySegment<byte>) =
    let a = b.Array
    let rec loop i =
      if i > b.Offset + b.Count - 1 then None
      elif i > 0 && a.[i - 1] = EOL.[0] && a.[i] = EOL.[1] then Some (i - 1)
      else loop (i + 1)
    loop b.Offset

  /// Returns the index of the first CRLF in the union of two ArraySegment
  let inline scan_crlf_x (c : ArraySegment<byte>) (d : ArraySegment<byte>) =
    let a = unite c d
    let rec loop i =
      if i > c.Count + d.Count - 1 then None
      elif i > 0 && a (i - 1) = EOL.[0] && a i = EOL.[1] then Some (i - 1)
      else loop (i + 1)
    loop 0
