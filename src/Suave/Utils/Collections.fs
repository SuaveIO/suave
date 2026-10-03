/// A module for composing the applicatives.
[<AutoOpen>]
module Suave.Utils.Collections

open System.Collections.Generic
open System

/// A (string * string) list, use (%%) to access
type NameValueList = (string * string) list

/// A (string * string option) list, use (^^) to access
type NameOptionValueList = (string * string option) list

type IDictionary<'b,'a> with
  member dict.TryLookup key =
    match dict.TryGetValue key with
    | true, v  -> Choice1Of2 v
    | false, _ -> Choice2Of2 ("Key " + (key.ToString()) + " was not present")

let private indexOfFirst (comparison : StringComparison) (target : List<string*string>) (key : string) =
  let mutable index = 0
  let mutable found = false
  while index < target.Count && not found do
    let name, _ = target.[index]
    if name.Equals(key, comparison) then found <- true
    else index <- index + 1
  if found then index else -1

let private getFirstWithComparison comparison (target : List<string*string>) (key : string) =
  let index = indexOfFirst comparison target key
  if index < 0 then
    Choice2Of2 ("Key " + (key.ToString()) + " was not present")
  else
    Choice1Of2 (let (a,b) = target[index] in b)

/// The value of the first entry named `key`, or null when there is none. Unlike the
/// Choice-returning lookups it allocates nothing when the key is absent, which matters
/// on the request path where most probed headers are missing.
let internal tryGetFirstWithComparison comparison (target : List<string*string>) (key : string) : string =
  let index = indexOfFirst comparison target key
  if index < 0 then null else snd target.[index]

let getFirst target key =
  getFirstWithComparison StringComparison.Ordinal target key

let getFirstCaseInsensitive target key =
  getFirstWithComparison StringComparison.InvariantCultureIgnoreCase target key

let internal getFirstOrdinalIgnoreCase target key =
  getFirstWithComparison StringComparison.OrdinalIgnoreCase target key

let getFirstOpt (target : NameOptionValueList) (key : string) =
  match target |> List.tryPick (fun (a,b) -> if a.Equals key then b else None) with
  | Some b -> Choice1Of2 b
  | None -> Choice2Of2 ("Couldn't find key '" + key + "' in NameOptionValueList")

let tryGetChoice1 f x =
  match f x with
  | Choice1Of2 str -> Some str
  | Choice2Of2 _ -> None

let (%%) target key = getFirst target key
let (^^) target key = getFirstOpt target key

let (@@) (target:List<string*string>) key =
  getFirst target key
  
