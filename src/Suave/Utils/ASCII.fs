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