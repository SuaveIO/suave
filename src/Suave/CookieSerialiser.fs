namespace Suave

type CookieSerialiser =
  abstract serialise : Map<string, obj> -> byte []
  abstract deserialise : byte [] -> Map<string, obj>

open System.Text.Json

type BinaryFormatterSerialiser() =
  interface CookieSerialiser with
    member x.serialise m =
      JsonSerializer.SerializeToUtf8Bytes<_>(m)

    member x.deserialise data =
      JsonSerializer.Deserialize<_>(data)
