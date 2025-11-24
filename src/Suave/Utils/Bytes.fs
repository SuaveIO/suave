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
    let mutable i =  1
    let mutable j = 0
    while i < m - 1 do
      if p.[i] = p.[j] then
        i <- i + 1
        j <- j + 1 
        next.[i] <- j
      else
        if j = 0 then
          i <- i + 1
          next.[i] <- 0
        else 
          j <- next.[j]
    next

  let inline _kmpW (p: byte []) (next: int []) m (xs : ReadOnlySequence<byte>) =
      let mutable i = 0L
      let mutable j = 0
      let mutable found = false
      let mutable result = ValueNone
      // Iterate through segments
      let mutable enumerator = xs.GetEnumerator()
      while not found && enumerator.MoveNext() do
        let span = enumerator.Current.Span
        let mutable byteIdx = 0
        while not found && byteIdx < span.Length do
          // Current byte in the sequence
          let b = span.[byteIdx]
          
          // KMP failure function: back up j until we have a match or j is 0
          while j > 0 && b <> p.[j] do
            j <- next.[j]
          
          // Check if current byte matches pattern[j]
          if b = p.[j] then j <- j + 1
          
          // Found complete match - exit early
          if j = m then
            result <- ValueSome(i - int64(m - 1))
            found <- true
          
          i <- i + 1L
          byteIdx <- byteIdx + 1
      
      result

  let inline kmpW p (xs : ReadOnlySequence<byte>) =
    let next = initNext p
    let m = Array.length p
    _kmpW p next m xs

