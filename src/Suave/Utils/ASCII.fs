[<RequireQualifiedAccess>]
module Suave.Utils.ASCII

open System
open System.Text

/// Get the ASCII bytes for the string
let inline bytes (s : string) =
  Encoding.ASCII.GetBytes s

/// Convert the byte array of ASCII-encoded chars to a string, starting at 'index' for 'count' characters
/// (each character is necessarily one byte)
let inline toStringAtOffset (buff : byte[]) (index : int) (count : int) =
  Encoding.ASCII.GetString(buff, index, count)

let inline toString (b : byte []) =
  Encoding.ASCII.GetString b

/// Encode the string as ASCII encoded in Base64.
let inline encodeBase64 (s : string) =
  let bytes = Encoding.ASCII.GetBytes s
  Convert.ToBase64String bytes

/// Decode the string containing Base64-encoded ASCII string data to a string
let inline decodeBase64 (s : string) =
  let bytes = Convert.FromBase64String s
  Encoding.ASCII.GetString bytes

[<Obsolete("Renamed to toStringAtOffset")>]
let to_string buff index count = toStringAtOffset buff index count
[<Obsolete("Renamed to toString")>]
let to_string' b = toString b
[<Obsolete("Renamed to encodeBase64")>]
let base64_encode s = encodeBase64 s
[<Obsolete("Renamed to decodeBase64")>]
let base64_decode s = decodeBase64 s
