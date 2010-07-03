module Suave.Combinator

open System.Collections.Generic

let succeed x = Some(x)
let fail = None

let bind p rest =
    match p with
        | None -> fail
        | Some r -> rest r
        
let delay f = f()

let (>>=) a b = fun x -> bind (a x) b

let rec choose options arg = 
    match options with
    |[] -> None
    |p::tail  -> match p arg with
                 |None -> choose tail arg
                 |Some(x)  -> Some(x)
                 
let warbler f a = f a a //wich bird? A Warbler!

let look_up (target : IDictionary<string,string>) targetKey  = 
  let value = ref null
  if target.TryGetValue(targetKey, value) then 
    !value
  else ""

let (?) (target : IDictionary<string,string>) targetKey =
  look_up target targetKey                 

