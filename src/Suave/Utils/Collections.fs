/// A module for composing the applicatives.
[<AutoOpen>]
module Suave.Utils.Collections

open System.Collections.Generic

/// A (string * string) list, use (%%) to access
type NameValueList = (string * string) list

/// A (string * string option) list, use (^^) to access
type NameOptionValueList = (string * string option) list

type IDictionary<'b,'a> with
  member dict.TryLookup key =
    match dict.TryGetValue key with
    | true, v  -> Choice1Of2 v
    | false, _ -> Choice2Of2 (sprintf "Key %A was not present" key)

let getFirst (target : NameValueList) (key : string) =
  match target |> List.tryPick (fun (a, b) -> if a.Equals key then Some b else None) with
  | Some b -> Choice1Of2 b
  | None   -> Choice2Of2 (sprintf "Couldn't find key '%s' in NameValueList" key)

let getFirstCaseInsensitive (target : NameValueList) (key : string) =
  match target |> List.tryPick (fun (a, b) -> if String.equalsCaseInsensitive a key then Some b else None) with
  | Some b -> Choice1Of2 b
  | None   -> Choice2Of2 (sprintf "Couldn't find key '%s' in NameValueList" key)

[<System.Obsolete("Use getFirstCaseInsensitive with an i")>]
let getFirstCaseInsensitve (target : NameValueList) (key : string) =
  getFirstCaseInsensitive target key

let getFirstOpt (target : NameOptionValueList) (key : string) =
  match target |> List.tryPick (fun (a,b) -> if a.Equals key then b else None) with
  | Some b -> Choice1Of2 b
  | None -> Choice2Of2 (sprintf "Couldn't find key '%s' in NameOptionValueList" key)

let tryGetChoice1 f x =
  match f x with
  | Choice1Of2 str -> Some str
  | Choice2Of2 _ -> None

let (%%) target key = getFirst target key
let (^^) target key = getFirstOpt target key
