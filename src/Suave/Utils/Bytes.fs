namespace Suave.Utils

open System
open System.Buffers

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

  let eolMemory = new Memory<byte>(EOL)

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

  let inline _kmpW (p: byte []) (next: int []) m (xs : ReadOnlySequence<byte>) =
      let n = xs.Length
      let mutable i = 0L
      let mutable j = 0L
      let a (x) =
        xs.Slice(xs.GetPosition(x)).First.Span[0]
      while j < m && i < n do
        if a(i) = p.[int j] then
          i <- i + 1L
          j <- j + 1L
        else
          if j = 0 then
            i <- i + 1L
          else
            j <- next.[int j]
      done;
      if j >= m then ValueSome(i - m) else ValueNone

  let inline kmpW p (xs : ReadOnlySequence<byte>) =
    let next = initNext p
    let m = Array.length p
    _kmpW p next m xs

