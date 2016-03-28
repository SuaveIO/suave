module Suave.Json

open System.IO
open System.Runtime.Serialization.Json
open System.Text

open Suave.Operators

/// Convert the object to a JSON representation inside a byte array (can be made string of)
let toJson<'T> (o: 'T) =
  let dcs = DataContractJsonSerializer(o.GetType())
  use ms = new MemoryStream()
  dcs.WriteObject(ms, o)
  ms.ToArray()

/// Transform the byte array representing a JSON object to a .Net object
let fromJson<'T> (bytes : byte []) =
  let dcs = DataContractJsonSerializer(typeof<'T>)
  use ms = new MemoryStream()
  ms.Write(bytes, 0, bytes.Length)
  ms.Seek(0L, SeekOrigin.Begin) |> ignore
  dcs.ReadObject(ms) :?> 'T

/// Expose function f through a json call where you decide
/// which serializer to use.

let mapJsonWith<'TIn, 'TOut> (deserializer:byte[] -> 'TIn) (serializer:'TOut->byte[]) f =
  request(fun r ->
    f (deserializer r.rawForm)
    |> serializer
    |> Successful.ok
    >=> Writers.setMimeType "application/json")

/// Expose function f through a json call; lets you write like
///
/// let app =
///   url "/path"  >>= map_json some_function;
///

let mapJson<'T1,'T2> = mapJsonWith<'T1,'T2> fromJson toJson
