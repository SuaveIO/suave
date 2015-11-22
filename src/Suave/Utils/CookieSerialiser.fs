namespace Suave.Utils

open System.IO
open System.Runtime.Serialization.Formatters.Binary

type CookieSerialiser =
  abstract Serialise : Map<string, obj> -> byte []
  abstract Deserialise : byte [] -> Map<string, obj>

type BinaryFormatterSerialiser() =
  interface CookieSerialiser with
    member x.Serialise m =
      use ms = new MemoryStream()
      let f = new BinaryFormatter()
      f.Serialize(ms, m)
      ms.ToArray()

    member x.Deserialise data =
      use ms = new MemoryStream(data)
      let f = new BinaryFormatter()
      f.Deserialize ms :?> _