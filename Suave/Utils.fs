/// A module for composing the applicatives.
[<AutoOpen>]
module Suave.Utils

open System.Collections.Generic

/// Return success with some value
let inline succeed x = Some x

/// Return failure without any value
let fail = None

/// Return failure with a value that is ignored
let inline never _ = None

/// bind f inp evaluates to match inp with None -> None | Some x -> f x
/// The same as Option.bind.
let bind = Option.bind

/// Delay the computation of f
let delay f = f()

/// Compose (bind) two arguments, 'a' and 'b', so that the result of
/// the composition can be applied to an argument of 'a' and then passed
/// to 'b', if 'a' yields a value.
let inline (>>=) a b = fun x -> Option.bind b (a x)

/// Left-to-right Kleisli composition of monads.
let (>=>) a b = fun x -> 
  match a x with
  | None   -> b x
  | r      -> r

/// Entry-point for composing the applicative routes of the http application,
/// by iterating the options, applying the context, arg, to the predicate
/// from the list of options, until there's a match/a Some(x) which can be
/// run.
let rec choose options arg =
  match options with
  | []        -> None
  | p :: tail ->
    match p arg with
    | Some x -> Some x
    | None   -> choose tail arg

/// Pipe the request through a bird that can peck at it.
let inline warbler f a = f a a //which bird? A Warbler!

/// Pipe the request through a bird that can peck at it.
let inline (>>==) a b = a >>= warbler (fun r -> b r)

/// Try find a value by key in a dictionary
let look_up (target : IDictionary<'b,'a>) key =
  match target.TryGetValue(key) with
  | true, v  -> Some v
  | false, _ -> None

/// Try find a value by key in a dictionary
let (?) (target : IDictionary<'b,'a>) key =
  look_up target key

/// Assign a value to the key in the dictionary
let (?<-) (target : IDictionary<string, 'a>) key value =
  target.[key] <- value

/// Force the evaluation of the option, so that if there is no value,
/// an InvalidOperationException is raised.
let opt = function
  | Some x -> x
  | None   -> invalidArg "arg1" "The argument was required but was not present"

/// The constant function, which returns its constant, no matter
/// its input.
let cnst x = fun _ -> x

/// The conditional function that applies f x a if there's a value in d,
/// or otherwise, applies g a, if there is no value in d.
let cond d f g a =
  match d with
  | Some x -> f x a
  | None   -> g a

//- theorem: identity = (cnst |> warbler)
//(warbler cnst) x = cnst x x = fun _ -> x

[<RequireQualifiedAccess>]
module UTF8 =
  open System
  open System.Text

  let inline to_string (b : byte []) (index : int) (count : int) =
    Encoding.UTF8.GetString(b, index, count)

  let inline to_string' (b : byte []) =
    Encoding.UTF8.GetString b

  /// Get the UTF-8 bytes for the string
  let inline bytes (s : string) =
    Encoding.UTF8.GetBytes s

  /// Encode the string as UTF8 encoded in Base64.
  let inline base64_encode (s : string) =
    let bytes = Encoding.UTF8.GetBytes s
    Convert.ToBase64String bytes

  let inline base64_decode s =
    let bytes = Convert.FromBase64String s
    Encoding.UTF8.GetString bytes

[<RequireQualifiedAccess>]
module ASCII =
  open System
  open System.Text

  /// Get the ASCII bytes for the string
  let inline bytes (s : string) =
    Encoding.ASCII.GetBytes s

  /// Convert the byte array of ASCII-encoded chars to a string, starting at 'index' for 'count' characters
  /// (each character is necessarily one byte)
  let inline to_string (buff : byte[]) (index : int) (count : int) =
    Encoding.ASCII.GetString(buff, index, count)

  let inline to_string' (b : byte []) =
    Encoding.ASCII.GetString b

  /// Encode the string as ASCII encoded in Base64.
  let inline base64_encode (s : string) =
    let bytes = Encoding.ASCII.GetBytes s
    Convert.ToBase64String bytes

  /// Decode the string containing Base64-encoded ASCII string data to a string
  let inline base64_decode (s : string) =
    let bytes = Convert.FromBase64String s
    Encoding.ASCII.GetString bytes

module Option =
  let or_default value opt =
    opt |> Option.fold (fun s t -> t) value

module RandomExtensions =
  open System

  type internal Random with
    /// generate a new random ulong64 value
    member x.NextUInt64() =
      let buffer = Array.zeroCreate<byte> sizeof<UInt64>
      x.NextBytes buffer
      BitConverter.ToUInt64(buffer, 0)

module Bytes =
  open System
  open System.IO
  open System.Text

  type BufferSegment =
    { buffer : ArraySegment<byte>
    ; offset : int
    ; length : int }

  let inline mk_buffer_segment buffer offset length =
    Some { buffer = buffer; offset = offset; length = length }

  /// The end-of-line literal, \r\n (CRLF)
  let [<Literal>] eol = "\r\n"

  /// The end-of-line 'literal' as bytes, the \r\n (CRLF) byte pair
  let EOL = ASCII.bytes eol

  /// The corresponding EOL array segment
  let eol_array_segment = new ArraySegment<_>(EOL, 0, 2)

  let inline bytes_to_buffer (s : string) (buff : byte array) (offset : int) =
    Encoding.ASCII.GetBytes (s, 0, s.Length, buff, offset)

  /// Fully transform the input stream to a byte array.
  let read_fully (input : Stream) =
    use ms = new MemoryStream()
    input.CopyTo ms
    ms.ToArray()

  /// Asynchronously write from the 'from' stream to the 'to' stream, with an upper bound on
  /// amount to transfer by len
  let transfer_len (to_stream : Stream) (from : Stream) len =
    let buf_size = 0x2000
    let buf = Array.zeroCreate<byte> 0x2000
    let rec do_block left = async {
      let! read = from.AsyncRead(buf, 0, Math.Min(buf_size, left))
      if read <= 0 || left - read = 0 then
        do! to_stream.FlushAsync()
        return ()
      else
        do! to_stream.AsyncWrite(buf, 0, read)
        return! do_block (left - read) }
    do_block len

  /// Asynchronously write from the 'from' stream to the 'to' stream.
  let transfer (to_stream : Stream) (from : Stream) =
    let buf = Array.zeroCreate<byte> 0x2000
    let rec do_block () = async {
      let! read = from.AsyncRead buf
      if read <= 0 then
        do! to_stream.FlushAsync()
        return ()
      else
        do! to_stream.AsyncWrite(buf, 0, read)
        return! do_block () }
    do_block ()

  /// Knuth-Morris-Pratt algorithm
  /// http://caml.inria.fr/pub/old_caml_site/Examples/oc/basics/kmp.ml
  let init_next p =
    let m = Array.length p
    let next = Array.create m 0
    let i = ref 1
    let j = ref 0
    while !i < m - 1 do
      if p.[!i] = p.[!j] then begin incr i; incr j; next.[!i] <- !j end else
      if !j = 0 then begin incr i; next.[!i] <- 0 end else j := next.[!j]
    next

  let kmp p =
    let next = init_next p
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
    let next = init_next p
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

  let inline unite (a : ArraySegment<_>) (b : ArraySegment<_>) =
    fun (i : int) -> 
      if   i < 0       then failwith "invalid args"
      elif i < a.Count then a.Array.[a.Offset + i]
      elif i < a.Count + b.Count then b.Array.[b.Offset + (i - a.Count)]
      else failwith "invalid args"

  let kmp_x_x p =
    let next = init_next p
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

module Compression =
  open System.IO
  open System.IO.Compression

  let gzip_encode (bytes : byte []) =
    use memory =  new MemoryStream()
    use gzip = new GZipStream(memory, CompressionMode.Compress)
    do gzip.Write(bytes, 0, bytes.Length)
    gzip.Close()
    memory.ToArray()

  let gzip_decode (bytes : byte []) =
    use compressed =  new MemoryStream(bytes)
    use gzip = new GZipStream(compressed, CompressionMode.Decompress)
    use result = new MemoryStream()
    gzip.CopyTo(result)
    result.ToArray()

  let deflate_encode (bytes : byte []) =
    use memory =  new MemoryStream()
    use gzip = new DeflateStream(memory, CompressionMode.Compress)
    do gzip.Write(bytes, 0, bytes.Length)
    gzip.Close()
    memory.ToArray()

  let deflate_decode (bytes : byte []) =
    use compressed =  new MemoryStream(bytes)
    use gzip = new DeflateStream(compressed, CompressionMode.Decompress)
    use result = new MemoryStream()
    gzip.CopyTo(result)
    result.ToArray()

module Parsing =
  open Bytes

  open System
  open System.IO
  open System.Net

  /// Gets whether the passed ip is a local IPv4 or IPv6 address.
  /// Example: 127.0.0.1, ::1 return true. If the IP cannot be parsed,
  /// returns false.
  let is_local_address (ip : string) =
    match IPAddress.TryParse ip with
    | false, _   -> false
    | true,  ip' -> IPAddress.IsLoopback ip'

  /// Parse the data in the string to a dictionary, assuming k/v pairs are separated
  /// by the ampersand character.
  let parse_data (s : string) (param : Dictionary<string,string>) =
    s.Split('&')
    |> Array.iter (fun (k : string) ->
         k.Split('=')
         |> (fun d -> if d.Length = 2 then param.Add(d.[0], System.Web.HttpUtility.UrlDecode(d.[1]))))
    param

  /// parse the url into its constituents and fill out the passed dictionary with
  /// query string key-value pairs
  let inline parse_url (line : string) (dict : Dictionary<string,string>) =
    let parts = line.Split(' ')
    if parts.Length < 2 || parts.Length > 3 then failwith (sprintf "invalid url: '%s'" line)
    let indexOfMark = parts.[1].IndexOf('?')

    if indexOfMark > 0 then
      let raw_query = parts.[1].Substring(indexOfMark + 1)
      parse_data raw_query dict |> ignore
      (parts.[0], parts.[1].Substring(0,indexOfMark), "?" + raw_query, parts.[2])
    else
      (parts.[0], parts.[1], String.Empty, parts.[2])

  /// Parse the cookie data in the string into a dictionary
  let parse_cookie (s : string) =
    s.Split(';')
    |> Array.map (fun (x : string) ->
                  let parts = x.Split('=')
                  (parts.[0], parts.[1]))

  /// Parse a string array of key-value-pairs, combined using the equality character '='
  /// into a dictionary
  let parse_key_value_pairs arr =
    let dict = new Dictionary<string,string>()
    arr
    |> Array.iter (fun (x : String) ->
                   let parts = x.Split('=')
                   dict.Add(parts.[0], parts.[1]))
    dict

  /// Parse the header parameters into key-value pairs, as a dictionary.
  /// Fails if the header is a None.
  let header_params (header : string option) =
    match header with
    | Some x ->
      let parts = x.Split(';') |> Array.map (fun x -> x.TrimStart())
      parse_key_value_pairs (Array.sub parts 1 (parts.Length - 1))
    | None ->
      failwith "did not find header, because header_params received None"
