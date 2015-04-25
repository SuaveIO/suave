namespace Suave.Utils

[<RequireQualifiedAccess>]
module UTF8 =
  open System
  open System.Text

  let inline toStringAtOffset (b : byte []) (index : int) (count : int) =
    Encoding.UTF8.GetString(b, index, count)

  let inline toString (b : byte []) =
    Encoding.UTF8.GetString b

  /// Get the UTF-8 bytes for the string
  let inline bytes (s : string) =
    Encoding.UTF8.GetBytes s

  /// Encode the string as UTF8 encoded in Base64.
  let inline encodeBase64 (s : string) =
    let bytes = Encoding.UTF8.GetBytes s
    Convert.ToBase64String bytes

  let inline decodeBase64 s =
    let bytes = Convert.FromBase64String s
    Encoding.UTF8.GetString bytes

  [<Obsolete("Renamed to toStringAtOffset")>]
  /// Obsolete
  let to_string  buff index count = toStringAtOffset buff index count
  [<Obsolete("Renamed to toString")>]
  /// Obsolete
  let to_string'  b = toString b
  [<Obsolete("Renamed to encodeBase64")>]
  /// Obsolete
  let base64_encode  s = encodeBase64 s
  [<Obsolete("Renamed to decodeBase64")>]
  /// Obsolete
  let base64_decode  s =  decodeBase64 s
