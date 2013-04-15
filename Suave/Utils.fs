[<AutoOpen>]
module Suave.Utils

open System.Collections.Generic

let succeed x = Some(x)
let fail = None
let never _ = None

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

let look_up (target : IDictionary<'b,'a>) key  = 
  match target.TryGetValue(key) with
  | (true, v) -> Some(v)
  | (false, _) -> None
  
let opt = function
    |Some(x) -> x
    |None    -> failwith "Invalid arg."

let (?) (target : IDictionary<'b,'a>) key =
  look_up target key 
  
let (?<-) (target : IDictionary<string, 'a>) key value =
  target.[key] <- value    
  
let cnst x = fun _ -> x

let cond d f g a =
    match d with
    |Some(x) -> f x a
    |None -> g a

//- theorem: identity = (cnst |> warbler)
//(warbler cnst) x = cnst x x = fun _ -> x