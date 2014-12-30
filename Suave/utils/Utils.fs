/// A module for composing the applicatives.
[<AutoOpen>]
module Suave.Utils

open System.Collections.Generic

/// A (string * string) list, use (%%) to access
type NameValueList = (string * string) list

/// A (string * string option) list, use (^^) to access
type NameOptionValueList = (string * string option) list

/// Try find a value by key in a dictionary
let look_up (target : IDictionary<'b,'a>) key =
  match target.TryGetValue key with
  | true, v  -> Some v
  | false, _ -> None

let get_first (target : NameValueList) key =
  match List.tryFind (fun (a,b) -> a.Equals key) target with
  | Some value -> snd value |> Some
  | None -> None

/// Try find a value by key in a dictionary
let (?) (target : IDictionary<'b,'a>) key =
  look_up target key

let (%%) (target : NameValueList) key =
  get_first target key

let (^^) (target : NameOptionValueList) key =
  match List.tryFind (fun (a,b) -> a.Equals key) target with
  | Some value ->
    snd value
  | None -> None

/// Assign a value to the key in the dictionary
let (?<-) (target : IDictionary<string, 'a>) key value =
  target.[key] <- value
