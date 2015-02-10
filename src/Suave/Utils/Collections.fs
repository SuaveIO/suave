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
        | true, v  -> Some v
        | false, _ -> None

let getFirst (target : NameValueList) key =
  match List.tryFind (fun (a,b) -> a.Equals key) target with
  | Some value -> snd value |> Some
  | None -> None

let (%%) (target : NameValueList) key =
  getFirst target key

let (^^) (target : NameOptionValueList) key =
  match List.tryFind (fun (a,b) -> a.Equals key) target with
  | Some value ->
    snd value
  | None -> None


type Property<'T,'P> = ('T -> 'P) * ('P -> 'T -> 'T) 

let internal Property<'T,'P> getter setter : Property<'T,'P> = (getter,setter)

