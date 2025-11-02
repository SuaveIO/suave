/// This module could do with a refactor.
module internal Suave.Utils.Parsing

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Text.RegularExpressions

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
  
  if String.IsNullOrWhiteSpace(s) then
    []
  else
    // Use span-based splitting to reduce allocations
    let span = s.AsSpan()
    let mutable result = []
    let mutable start = 0
    
    for i = 0 to span.Length - 1 do
      if span.[i] = '&' then
        if i > start then
          let segment = s.Substring(start, i - start)
          if not (String.IsNullOrWhiteSpace(segment)) then
            result <- (segment.Split('=') |> parseArr) :: result
        start <- i + 1
    
    // Handle last segment
    if start < span.Length then
      let segment = s.Substring(start)
      if not (String.IsNullOrWhiteSpace(segment)) then
        result <- (segment.Split('=') |> parseArr) :: result
    
    List.rev result

/// parse the url into its constituents and fill out the passed dictionary with
/// query string key-value pairs
let inline parseUrl (line : string) =
  // Use ReadOnlySpan to reduce allocations from splitting and substring operations
  let span = line.AsSpan()
  
  // Find space positions
  let mutable firstSpace = -1
  let mutable secondSpace = -1
  
  for i = 0 to span.Length - 1 do
    if span.[i] = ' ' then
      if firstSpace = -1 then
        firstSpace <- i
      elif secondSpace = -1 then
        secondSpace <- i
  
  if firstSpace = -1 || secondSpace = -1 || firstSpace >= secondSpace then
    Choice2Of2("Invalid url")
  else
    let method = line.Substring(0, firstSpace)
    let urlSpan = span.Slice(firstSpace + 1, secondSpace - firstSpace - 1)
    let version = line.Substring(secondSpace + 1)
    
    // Find query string marker
    let mutable queryIndex = -1
    for i = 0 to urlSpan.Length - 1 do
      if urlSpan.[i] = '?' then
        queryIndex <- i
    
    if queryIndex > 0 then
      let path = line.Substring(firstSpace + 1, queryIndex)
      let rawQuery = line.Substring(firstSpace + 1 + queryIndex + 1, urlSpan.Length - queryIndex - 1)
      Choice1Of2(method, path, rawQuery, version)
    else
      let path = line.Substring(firstSpace + 1, secondSpace - firstSpace - 1)
      Choice1Of2(method, path, String.Empty, version)

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

/// Parse the boundary from the value of the Content-Type header.
/// Based on the allowed set from
/// https://www.rfc-editor.org/rfc/rfc2046#section-5.1.1
/// It allows alphanumeric characters, punctuations and spaces (except at the
/// end). Quotation marks seem to be optional as well.
///
let parseBoundary contentType =
  let pattern = "boundary=\"?([a-zA-Z0-9'\(\)+_,-.\/:=? ]*)(?<! )\"?"
  Regex.Match(contentType, pattern).Groups.[1].Value
