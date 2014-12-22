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
  open System

  /// Ordinally compare two strings in constant time, bounded by the length of the
  /// longest string.
  let eq_ord_cnst_time (str1 : string) (str2 : string) =
    let mutable xx = uint32 str1.Length ^^^ uint32 str2.Length
    let mutable i = 0
    while i < str1.Length && i < str2.Length do
      xx <- xx ||| uint32 (int str1.[i] ^^^ int str2.[i])
      i <- i + 1
    xx = 0u

  /// Compare ordinally with ignore case.
  let eq_ord_ci (str1 : string) (str2 : string) =
    String.Equals(str1, str2, StringComparison.OrdinalIgnoreCase)

  let trim (s : string) =
    s.Trim()

  let split (c : char) (s : string) =
    s.Split c |> Array.toList

module Option =
  let or_default value opt =
    opt |> Option.fold (fun s t -> t) value

module Map =

  let put key value m =
    match m |> Map.tryFind key with
    | None -> m |> Map.add key value
    | Some _ -> m |> Map.remove key |> Map.add key value

module Choice =

  let mk x = Choice1Of2 x

  let map f = function
    | Choice1Of2 v   -> Choice1Of2 (f v)
    | Choice2Of2 err -> Choice2Of2 err

  let map_2 f = function
    | Choice1Of2 v   -> Choice1Of2 v
    | Choice2Of2 err -> Choice2Of2 (f err)

  let bind (f : 'a -> Choice<'b, 'c>) (v : Choice<'a, 'c>) =
    match v with
    | Choice1Of2 v -> f v
    | Choice2Of2 c -> Choice2Of2 c

  let from_option on_missing = function
    | Some x -> Choice1Of2 x
    | None   -> Choice2Of2 on_missing

module List =

  let flat_map f xs =
    xs
    |> List.map f
    |> List.concat

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
 
  /// Ordinally compare two strings in constant time, bounded by the length of the
  /// longest string.
  let cnst_time_cmp (bits : byte []) (bobs : byte []) =
    let mutable xx = uint32 bits.Length ^^^ uint32 bobs.Length
    let mutable i = 0
    while i < bits.Length && i < bobs.Length do
      xx <- xx ||| uint32 (bits.[i] ^^^ bobs.[i])
      i <- i + 1
    xx = 0u

  type BufferSegment =
    { buffer : ArraySegment<byte>
      offset : int
      length : int }


  // for ci in (int '!')..(int '~') do printfn "%c" (char ci);;
  // https://en.wikipedia.org/wiki/HTTP_cookie#Setting_a_cookie
  let cookie_encoding =
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

module Compression =
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

/// Small crypto module that can do HMACs and generate random strings to use
/// as keys, as well as create a 'cryptobox'; i.e. a AES256+HMACSHA256 box with
/// compressed plaintext contents so that they can be easily stored in cookies.
module Crypto =
  open System
  open System.IO
  open System.IO.Compression
  open System.Text
  open System.Security.Cryptography

  /// The default hmac algorithm
  [<Literal>]
  let HMACAlgorithm = "HMACSHA256"

  /// The length of the HMAC value in number of bytes
  [<Literal>]
  let HMACLength = 32 // = 256 / 8

  /// Calculate the HMAC of the passed data given a private key
  let hmac (key : byte []) offset count (data : byte[]) =
    use hmac = HMAC.Create(HMACAlgorithm)
    hmac.Key <- key
    hmac.ComputeHash (data, offset, count)

  let hmac' key (data : byte []) =
    hmac key 0 (data.Length) data

  /// Calculate the HMAC value given the key
  /// and a seq of string-data which will be concatenated in its order and hmac-ed.
  let hmac'' (key : byte []) (data : string seq) =
    hmac' key (String.Concat data |> UTF8.bytes)

  /// # bits in key
  let KeySize   = 256

  /// # bytes in key
  let KeyLength = KeySize / 8

  /// # bits in block
  let BlockSize = 128

  /// # bytes in IV
  /// 16 bytes for 128 bit blocks
  let IVLength = BlockSize / 8

  /// the global crypto-random pool for uniform and therefore cryptographically
  /// secure random values
  let crypt_random = RandomNumberGenerator.Create()

  /// Fills the passed array with random bytes
  let randomize (bytes : byte []) =
    crypt_random.GetBytes bytes
    bytes

  /// Generates a string key from the available characters with the given key size.
  let generate_key key_length =
    Array.zeroCreate<byte> key_length |> randomize

  let generate_key' () =
    generate_key KeyLength

  let generate_iv iv_length =
    Array.zeroCreate<byte> iv_length |> randomize

  let generate_iv' () =
    generate_iv IVLength

  /// key: 32 bytes for 256 bit key
  /// Returns a new key and a new iv as two byte arrays as a tuple.
  let generate_keys () =
    generate_key' (), generate_iv' ()

  type SecretboxEncryptionError =
    | InvalidKeyLength of string
    | EmptyMessageGiven

  type SecretboxDecryptionError =
    | TruncatedMessage of string
    | AlteredOrCorruptMessage of string

  let private secretbox_init key iv =
    let aes = new AesManaged()
    aes.KeySize   <- KeySize
    aes.BlockSize <- BlockSize
    aes.Mode      <- CipherMode.CBC
    aes.Padding   <- PaddingMode.PKCS7
    aes.IV        <- iv
    aes.Key       <- key
    aes

  let secretbox (key : byte []) (msg : byte []) =
    if key.Length <> KeyLength then
      Choice2Of2 (InvalidKeyLength (sprintf "key should be %d bytes but was %d bytes" KeyLength (key.Length)))
    elif msg.Length = 0 then
      Choice2Of2 EmptyMessageGiven
    else
      let iv  = generate_iv' ()
      use aes = secretbox_init key iv

      let mk_cipher_text (msg : byte []) (key : byte []) (iv : byte []) =
        use enc      = aes.CreateEncryptor(key, iv)
        use cipher   = new MemoryStream()
        use crypto   = new CryptoStream(cipher, enc, CryptoStreamMode.Write)
        let bytes = msg |> Compression.gzip_encode
        crypto.Write (bytes, 0, bytes.Length)
        crypto.FlushFinalBlock()
        cipher.ToArray()

      use cipher_text = new MemoryStream()

      let bw  = new BinaryWriter(cipher_text)
      bw.Write iv
      bw.Write (mk_cipher_text msg key iv)
      bw.Flush ()

      let hmac = hmac' key (cipher_text.ToArray())
      bw.Write hmac
      bw.Dispose()

      Choice1Of2 (cipher_text.ToArray())

  let secretbox' (key : byte []) (msg : string) =
    secretbox key (msg |> UTF8.bytes)

  let secretbox_open (key : byte []) (cipher_text : byte []) =
    let hmac_calc = hmac key 0 (cipher_text.Length - HMACLength) cipher_text
    let hmac_given = Array.zeroCreate<byte> HMACLength
    Array.blit cipher_text (cipher_text.Length - HMACLength) // from
               hmac_given  0                                 // to
               HMACLength                                    // # bytes for hmac

    if cipher_text.Length < HMACLength + IVLength then
      Choice2Of2 (
        TruncatedMessage (
          sprintf "cipher text length was %d but expected >= %d"
                  cipher_text.Length (HMACLength + IVLength)))
    elif not (Bytes.cnst_time_cmp hmac_calc hmac_given) then
      Choice2Of2 (AlteredOrCorruptMessage "calculated HMAC does not match expected/given")
    else
      let iv = Array.zeroCreate<byte> IVLength
      Array.blit cipher_text 0
                 iv 0
                 IVLength
      use aes     = secretbox_init key iv
      use denc    = aes.CreateDecryptor(key, iv)
      use plain   = new MemoryStream()
      use crypto  = new CryptoStream(plain, denc, CryptoStreamMode.Write)
      crypto.Write(cipher_text, IVLength, cipher_text.Length - IVLength - HMACLength)
      crypto.FlushFinalBlock()
      Choice1Of2 (plain.ToArray() |> Compression.gzip_decode)

  let secretbox_open' k c =
    secretbox_open k c |> Choice.map UTF8.to_string'

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
