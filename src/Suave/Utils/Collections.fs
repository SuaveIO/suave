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

let getFirst (target : List<string*string>) (key : string) =
  let index = target.FindIndex(fun (x:string,y:string) -> x.Equals(key))
  if index < 0 then
    Choice2Of2 ("Key " + (key.ToString()) + " was not present")
  else
    Choice1Of2 (let (a,b) = target[index] in b)

let getFirstCaseInsensitive (target : List<string*string>) (key : string) =
  let index = target.FindIndex(fun (x:string,y:string) -> x.Equals(key, StringComparison.InvariantCultureIgnoreCase))
  if index < 0 then
    Choice2Of2 ("Key " + (key.ToString()) + " was not present")
  else
    Choice1Of2 (let (a,b) = target[index] in b)

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
  let index = target.FindIndex(fun (x:string,y:string) -> x.Equals(key))
  if index < 0 then
    Choice2Of2 ("Key " + (key.ToString()) + " was not present")
  else
    Choice1Of2 (let (a,b) = target[index] in b)
  
