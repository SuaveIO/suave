namespace Suave

open System.IO

type CookieSerialiser =
  abstract Serialise : Map<string, obj> -> byte []
  abstract Deserialise : byte [] -> Map<string, obj>

#if NETSTANDARD1_5
open System.Runtime.Serialization.Json

type JsonFormatterSerialiser() =
  interface CookieSerialiser with
    member x.Serialise m =
      use ms = new MemoryStream()
      let f = new DataContractJsonSerializer(typeof<_>)
      f.WriteObject(ms, m)
      ms.ToArray()
      
    member x.Deserialise data =
      use ms = new MemoryStream(data)
      let f = new DataContractJsonSerializer(typeof<_>)
      f.ReadObject(ms) :?> _
      
#else
open System.Runtime.Serialization.Formatters.Binary

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

#endif