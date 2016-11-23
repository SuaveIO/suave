namespace Suave.Utils

open System
open System.IO
open System.Text

type BufferSegment = struct
  val public buffer : ArraySegment<byte>
  val public offset : int
  val public length : int

  new (buffer, offset, length) = {
    buffer = buffer
    offset = offset
    length = length
    }
end

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BufferSegment =

  let inline create buffer offset length =
    #if DEBUG
    if length < 0 then failwithf "BufferSegment.create: length = %d < 0" length
    #endif
    new BufferSegment(buffer, offset, length)

  let inline toArraySegment (b: BufferSegment) =
    ArraySegment(b.buffer.Array, b.offset, b.length)

module internal Bytes =

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
  [<Literal>]
  let eol = "\r\n"

  /// The end-of-line 'literal' as bytes, the \r\n (CRLF) byte pair
  let EOL = ASCII.bytes eol

  /// The corresponding EOL array segment
  let eolArraySegment = new ArraySegment<_>(EOL, 0, 2)

  /// Knuth-Morris-Pratt algorithm
  /// http://caml.inria.fr/pub/old_caml_site/Examples/oc/basics/kmp.ml
  let inline initNext p =
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

  let kmpX p =
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

  let kmpY p =
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

  let inline _kmpZ (p: byte []) (next: int []) m (xs : BufferSegment list) =
      let a = uniteArrayBufferSegment xs
      let n = List.fold (fun acc (x :  BufferSegment) -> acc + x.length) 0 xs
      let  i = ref 0
      let j = ref 0 in
      while !j < m && !i < n do
        if a(!i) = p.[!j] then begin incr i; incr j end else
        if !j = 0 then incr i else j := next.[!j]
      done;
      if !j >= m then Some(!i - m) else None

  let kmpZ p (xs : BufferSegment list) =
    let next = initNext p
    let m = Array.length p
    _kmpZ p next m xs

  let inline _kmpW (p: byte []) (next: int []) m (xs : BufferSegment seq) =
      let a = uniteArrayBufferSegment (Seq.toList xs)
      let n = Seq.fold (fun acc (x :  BufferSegment) -> acc + x.length) 0 xs
      let  i = ref 0
      let j = ref 0 in
      while !j < m && !i < n do
        if a(!i) = p.[!j] then begin incr i; incr j end else
        if !j = 0 then incr i else j := next.[!j]
      done;
      if !j >= m then Some(!i - m) else None

  let kmpW p (xs : BufferSegment seq) =
    let next = initNext p
    let m = Array.length p
    _kmpW p next m xs

  let inline unite (a : ArraySegment<_>) (b : ArraySegment<_>) =
    fun (i : int) ->
      if   i < 0       then failwith "invalid args"
      elif i < a.Count then a.Array.[a.Offset + i]
      elif i < a.Count + b.Count then b.Array.[b.Offset + (i - a.Count)]
      else failwith "invalid args"

  let kmpXX p =
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
  let inline scanCrlf (b : ArraySegment<byte>) =
    let a = b.Array
    let rec loop i =
      if i > b.Offset + b.Count - 1 then None
      elif i > 0 && a.[i - 1] = EOL.[0] && a.[i] = EOL.[1] then Some (i - 1)
      else loop (i + 1)
    loop b.Offset

  /// Returns the index of the first CRLF in the union of two ArraySegment
  let inline scanCrlfX (c : ArraySegment<byte>) (d : ArraySegment<byte>) =
    let a = unite c d
    let rec loop i =
      if i > c.Count + d.Count - 1 then None
      elif i > 0 && a (i - 1) = EOL.[0] && a i = EOL.[1] then Some (i - 1)
      else loop (i + 1)
    loop 0
