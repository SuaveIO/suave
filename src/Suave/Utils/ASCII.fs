/// This module provide convenience functions for working with ASCII strings.
/// It's one of the few public Suave Util modules. It is not recommended to use
/// this module outside of pure machine-to-machine communication, as people
/// speak more languages then US english, like Mandarin, Swedish or Vietnamese,
/// which cannot be represented in ASCII.
[<RequireQualifiedAccess>]
module Suave.Utils.ASCII

open System
open System.Text

/// Get the ASCII-encoding of the string.
let bytes (s : string) =
  Encoding.ASCII.GetBytes s

/// Get the ASCII-encoding of the string and writes it to the passed `buff` at
/// `offset`.
let bytesToBuffer (s : string) (buff : byte []) (offset : int) =
  Encoding.ASCII.GetBytes (s, 0, s.Length, buff, offset)

/// Convert the byte array to a string, by indexing into the passed buffer `b`
/// and taking `count` bytes from it.
let toStringAtOffset (buff : byte[]) (index : int) (count : int) =
  Encoding.ASCII.GetString(buff, index, count)

/// Convert the full buffer `b` filled with ASCII-encoded strings into a CLR
/// string.
let toString (b : byte []) =
  Encoding.ASCII.GetString b

/// Convert the passed string `s` to ASCII and then encode the buffer with
/// base64. This function is lossy if your CLR string contains characters which
/// cannot be represented in ASCII.
let encodeBase64 (s : string) =
  let bytes = Encoding.ASCII.GetBytes s
  Convert.ToBase64String bytes

/// Convert the passed string `s`, assumed to be a valid Base64 encoding, to a
/// CLR string, going through ASCII.
let decodeBase64 (s : string) =
  let bytes = Convert.FromBase64String s
  Encoding.ASCII.GetString bytes
