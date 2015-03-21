module Suave.Json

open System.IO
open System.Runtime.Serialization.Json
open System.Text

open Suave.Http
open Suave.Web

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

/// Expose function f through a json call; lets you write like
///
/// let app =
///   url "/path"  >>= map_json some_function;
///

let mapJson f =
  Types.request(fun r ->
      f (fromJson r.rawForm) 
      |> toJson
      |> Successful.ok 
      >>= Writers.setMimeType "application/json") 

/// Obsolete
[<System.Obsolete("Use toJson")>]
let to_json o = toJson o
/// Obsolete
[<System.Obsolete("Use fromJson")>]
let from_json  bytes  = fromJson bytes
/// Obsolete
[<System.Obsolete("Use mapJson")>]
let map_json f = mapJson f
