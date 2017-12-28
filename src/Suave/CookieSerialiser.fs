namespace Suave

open System.IO

type CookieSerialiser =
  abstract serialise : Map<string, obj> -> byte []
  abstract deserialise : byte [] -> Map<string, obj>

open System.Runtime.Serialization.Json

type JsonFormatterSerialiser() =
  interface CookieSerialiser with
    member x.serialise m =
      use ms = new MemoryStream()
      let f = new DataContractJsonSerializer(typeof<_>)
      f.WriteObject(ms, m)
      ms.ToArray()

    member x.deserialise data =
      use ms = new MemoryStream(data)
      let f = new DataContractJsonSerializer(typeof<_>)
      f.ReadObject(ms) :?> _

open System.Runtime.Serialization.Formatters.Binary

type BinaryFormatterSerialiser() =
  interface CookieSerialiser with
    member x.serialise m =
      use ms = new MemoryStream()
      let f = new BinaryFormatter()
      f.Serialize(ms, m)
      ms.ToArray()

    member x.deserialise data =
      use ms = new MemoryStream(data)
      let f = new BinaryFormatter()
      f.Deserialize ms :?> _