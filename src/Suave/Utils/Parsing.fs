/// This module could do with a refactor.
module internal Suave.Utils.Parsing

open System
open System.Collections.Generic
open System.IO
open System.Net

/// Gets whether the passed ip is a local IPv4 or IPv6 address.
/// Example: 127.0.0.1, ::1 return true. If the IP cannot be parsed,
/// returns false.
let isLocalAddress (ip : string) =
  match IPAddress.TryParse ip with
  | false, _   -> false
  | true,  ip' -> IPAddress.IsLoopback ip'

/// Parse the data in the string to a dictionary, assuming k/v pairs are separated
/// by the ampersand character.
let parseData (s : string) =
  let parseArr (d : string array) =
    if d.Length = 2 then (d.[0], Some <| System.Net.WebUtility.UrlDecode(d.[1]))
    else d.[0],None
  s.Split('&')
  |> Array.toList
  |> List.filter (not << String.IsNullOrWhiteSpace)
  |> List.map (fun (k : string) -> k.Split('=') |> parseArr)

/// parse the url into its constituents and fill out the passed dictionary with
/// query string key-value pairs
let inline parseUrl (line : string) =
  let parts = line.Split(' ')
  if parts.Length <> 3 then 
    Choice2Of2("Invalid url")
  else
    let indexOfMark = parts.[1].IndexOf('?')

    if indexOfMark > 0 then
      let rawQuery = parts.[1].Substring(indexOfMark + 1)
      Choice1Of2(parts.[0], parts.[1].Substring(0,indexOfMark), rawQuery, parts.[2])
    else
      Choice1Of2(parts.[0], parts.[1], String.Empty, parts.[2])

/// Parse a string array of key-value-pairs, combined using the equality character '='
/// into a dictionary
let parseKVPairs arr =
  let dict = new Dictionary<string,string>()
  arr
  |> Array.iter (fun (x : String) ->
                 let parts = x.Split('=')
                 if parts.Length = 1 then
                   dict.Add(parts.[0], "")
                 else
                   dict.Add(parts.[0], parts.[1]))
  dict

/// Parse the header parameters into key-value pairs, as a dictionary.
/// Fails if the header is a None.
let headerParams (header : string) =
  let parts = header |> String.splita ';' |> Array.map String.trimStart
  parseKVPairs parts