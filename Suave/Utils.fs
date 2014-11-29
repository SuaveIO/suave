/// A module for composing the applicatives.
[<AutoOpen>]
module Suave.Utils

open System.Collections.Generic

/// A (string * string) list, use (%%) to access
type NameValueList = (string * string) list

/// A (string * string option) list, use (^^) to access
type NameOptionValueList = (string * string option) list

/// Try find a value by key in a dictionary
let look_up (target : IDictionary<'b,'a>) key =
  match target.TryGetValue key with
  | true, v  -> Some v
  | false, _ -> None

let get_first (target : NameValueList) key =
  match List.tryFind (fun (a,b) -> a.Equals key) target with
  | Some value -> snd value |> Some
  | None -> None

/// Try find a value by key in a dictionary
let (?) (target : IDictionary<'b,'a>) key =
  look_up target key

let (%%) (target : NameValueList) key =
  get_first target key

let (^^) (target : NameOptionValueList) key =
  match List.tryFind (fun (a,b) -> a.Equals key) target with
  | Some value ->
    snd value
  | None -> None

/// Assign a value to the key in the dictionary
let (?<-) (target : IDictionary<string, 'a>) key value =
  target.[key] <- value

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

module String =

  /// Ordinally compare two strings in constant time, bounded by the length of the
  /// longest string.
  let cnst_time_cmp_ord (str1 : string) (str2 : string) =
    let mutable xx = uint32 str1.Length ^^^ uint32 str2.Length
    let mutable i = 0
    while i < str1.Length && i < str2.Length do
      xx <- xx ||| uint32 (int str1.[i] ^^^ int str2.[i])
      i <- i + 1
    xx = 0u

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
      offset : int
      length : int }

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module BufferSegment =

    let inline mk buffer offset length =
      if length < 0 then failwith (sprintf "BufferSegment.mk: length = %d < 0" length)
      { buffer = buffer; offset = offset; length = length }

  /// The end-of-line literal, \r\n (CRLF)
  let [<Literal>] eol = "\r\n"

  /// The end-of-line 'literal' as bytes, the \r\n (CRLF) byte pair
  let EOL = ASCII.bytes eol

  /// The corresponding EOL array segment
  let eol_array_segment = new ArraySegment<_>(EOL, 0, 2)

  let inline bytes_to_buffer (s : string) (buff : byte []) (offset : int) =
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

  let inline unite_array_segment (aas : ArraySegment<byte> list) =
    fun (i : int) ->
      if   i < 0 then failwith "invalid args"
      let rec loop k acc =
        let a = aas.[k]
        if i < acc + a.Count then 
          a.Array.[a.Offset + (i - acc)]
        else loop (k + 1) (acc + a.Count)
      loop 0 0

  let inline unite_array_buffer_segment (aas : BufferSegment list) =
    fun (i : int) ->
      if   i < 0 then failwith "invalid args"
      let rec loop k acc =
        let a = aas.[k]
        if i < acc + a.length then 
          a.buffer.Array.[a.offset + (i - acc)]
        else loop (k + 1) (acc + a.length)
      loop 0 0

  let kmp_y p =
    let next = init_next p
    let m = Array.length p
    fun (xs : ArraySegment<byte> list) ->
      let a = unite_array_segment xs
      let n = List.fold (fun acc (x :  ArraySegment<byte>) -> acc + x.Count) 0 xs
      let  i = ref 0
      let j = ref 0 in
      while !j < m && !i < n do
        if a(!i) = p.[!j] then begin incr i; incr j end else
        if !j = 0 then incr i else j := next.[!j]
      done;
      if !j >= m then Some(!i - m) else None

  let kmp_z p =
    let next = init_next p
    let m = Array.length p
    fun (xs : BufferSegment list) ->
      let a = unite_array_buffer_segment xs
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

/// Small crypto module that can do HMACs and generate random strings to use
/// as keys
module Crypto =
  open System
  open System.Text
  open System.Security.Cryptography

  /// the global crypto-random pool for uniform and therefore cryptographically
  /// secure random values
  let crypt_random = RandomNumberGenerator.Create()

  /// Fills the passed array with random bytes
  let randomize (bytes : byte []) =
    crypt_random.GetBytes bytes
    bytes

  /// The default hmac algorithm
  [<Literal>]
  let HMACAlgorithm = "HMACSHA256"

  [<Literal>]
  let HMACBytes = 32 // = 256 / 8

  /// Calculate the HMAC of the passed data given a private key
  let hmac (key : byte []) (data : byte[]) =
    use hmac = HMAC.Create(HMACAlgorithm)
    hmac.Key <- key
    hmac.ComputeHash data

  /// Calculate the HMAC value given the string-key (will be converted to UTF8-bytes)
  /// and a seq of string-data which will be concatenated in its order and hmac-ed.
  let hmac' (key : string) (data : string seq) =
    let data' = String.Concat data |> UTF8.bytes
    hmac (UTF8.bytes key) data'

  /// The default alphabet (a string of characters) that is used to generate keys.
  [<Literal>]
  let GenerateKeyDefaultAlphabet = "abcdefghijklmnopqrstuvwuxyz0123456789"

  /// Generates a key from the available characters with the given key size.
  let generate_key (available_chars : string) (key_size : int) =
    let arr = Array.create<byte> key_size 0uy
    crypt_random.GetBytes arr
    let result = new StringBuilder(key_size)
    arr |> Array.iter (fun (b : byte) ->
        result.Append available_chars.[int b % available_chars.Length] |> ignore)
    result.ToString()

  /// Generates a cryptographically strong id of size key_size with the
  /// default alphabet
  let generate_key' key_size = generate_key GenerateKeyDefaultAlphabet key_size

module Compression =
  open System.IO
  open System.IO.Compression

  let gzip_encode (bytes : byte []) =
    if bytes.Length > 0 then
      use memory =  new MemoryStream()
      use gzip = new GZipStream(memory, CompressionMode.Compress)
      do gzip.Write(bytes, 0, bytes.Length)
      gzip.Close()
      memory.ToArray()
    else
      [||]

  let gzip_decode (bytes : byte []) =
    if bytes.Length > 0 then
      use compressed =  new MemoryStream(bytes)
      use gzip = new GZipStream(compressed, CompressionMode.Decompress)
      use result = new MemoryStream()
      gzip.CopyTo(result)
      result.ToArray()
    else
      [||]

  let deflate_encode (bytes : byte []) =
    if bytes.Length > 0 then
      use memory =  new MemoryStream()
      use gzip = new DeflateStream(memory, CompressionMode.Compress)
      do gzip.Write(bytes, 0, bytes.Length)
      gzip.Close()
      memory.ToArray()
    else
      [||]

  let deflate_decode (bytes : byte []) =
    if bytes.Length > 0 then
      use compressed =  new MemoryStream(bytes)
      use gzip = new DeflateStream(compressed, CompressionMode.Decompress)
      use result = new MemoryStream()
      gzip.CopyTo(result)
      result.ToArray()
    else
      [||]

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
  let parse_data (s : string) =
    let parse_arr (d : string array) =
      if d.Length = 2 then (d.[0], Some <| System.Web.HttpUtility.UrlDecode(d.[1]))
      else d.[0],None
    s.Split('&')
    |> Array.toList
    |> List.map (fun (k : string) -> k.Split('=') |> parse_arr)

  /// parse the url into its constituents and fill out the passed dictionary with
  /// query string key-value pairs
  let inline parse_url (line : string) =
    let parts = line.Split(' ')
    if parts.Length < 2 || parts.Length > 3 then failwith (sprintf "invalid url: '%s'" line)
    let indexOfMark = parts.[1].IndexOf('?')

    if indexOfMark > 0 then
      let raw_query = parts.[1].Substring(indexOfMark + 1)
      (parts.[0], parts.[1].Substring(0,indexOfMark), raw_query, parts.[2])
    else
      (parts.[0], parts.[1], String.Empty, parts.[2])

  /// Parse the cookie's name and data in the string into a dictionary.
  let parse_cookie (s : string) =
    s.Split(';')
    |> Array.map (fun (x : string) ->
                  let parts = x.Split('=')
                  parts.[0].Trim(), parts.[1].Trim())
    |> Array.toList

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
