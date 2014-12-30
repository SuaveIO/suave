namespace Sauve.Utils

module Parsing =

  open System
  open System.Collections.Generic
  open System.IO
  open System.Net

  /// Gets whether the passed ip is a local IPv4 or IPv6 address.
  /// Example: 127.0.0.1, ::1 return true. If the IP cannot be parsed,
  /// returns false.
  let is_local_address (ip : string) =
    match IPAddress.TryParse ip with
    | false, _   -> false
    | true,  ip' -> IPAddress.IsLoopback ip'

  /// Parse the data in the string to a dictionary, assuming k/v pairs are separated
  /// by the ampersand character.
  let parse_data (s : string) =
    let parse_arr (d : string array) =
      if d.Length = 2 then (d.[0], Some <| System.Web.HttpUtility.UrlDecode(d.[1]))
      else d.[0],None
    s.Split('&')
    |> Array.toList
    |> List.map (fun (k : string) -> k.Split('=') |> parse_arr)

  /// parse the url into its constituents and fill out the passed dictionary with
  /// query string key-value pairs
  let inline parse_url (line : string) =
    let parts = line.Split(' ')
    if parts.Length < 2 || parts.Length > 3 then failwith (sprintf "invalid url: '%s'" line)
    let indexOfMark = parts.[1].IndexOf('?')

    if indexOfMark > 0 then
      let raw_query = parts.[1].Substring(indexOfMark + 1)
      (parts.[0], parts.[1].Substring(0,indexOfMark), raw_query, parts.[2])
    else
      (parts.[0], parts.[1], String.Empty, parts.[2])

  /// Parse a string array of key-value-pairs, combined using the equality character '='
  /// into a dictionary
  let parse_key_value_pairs arr =
    let dict = new Dictionary<string,string>()
    arr
    |> Array.iter (fun (x : String) ->
                   let parts = x.Split('=')
                   dict.Add(parts.[0], parts.[1]))
    dict

  /// Parse the header parameters into key-value pairs, as a dictionary.
  /// Fails if the header is a None.
  let header_params (header : string option) =
    match header with
    | Some x ->
      let parts = x.Split(';') |> Array.map (fun x -> x.TrimStart())
      parse_key_value_pairs (Array.sub parts 1 (parts.Length - 1))
    | None ->
      failwith "did not find header, because header_params received None"
