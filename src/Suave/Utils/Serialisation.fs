namespace Suave.Utils

open System.IO
open System.Runtime.Serialization.Json

type CookieSerialiser =
  abstract Serialise : Map<string, obj> -> byte []
  abstract Deserialise : byte [] -> Map<string, obj>

type JsonFormatterSerialiser() =
  interface CookieSerialiser with
    member x.Serialise m =
      use ms = new MemoryStream()
      let f = new System.Runtime.Serialization.Json.DataContractJsonSerializer(typedefof<System.Object>)
      f.WriteObject(ms, m)
      ms.ToArray()

    member x.Deserialise data =
      use ms = new MemoryStream(data)
      let f = new System.Runtime.Serialization.Json.DataContractJsonSerializer(typedefof<System.Object>)
      f.ReadObject(ms) :?> _