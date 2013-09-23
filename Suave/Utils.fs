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

let (>>==) a b = a >>= warbler (fun r ->  b r)

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

open System.IO

let read_fully (input:Stream) = 
    use ms = new MemoryStream()
    input.CopyTo(ms);
    ms.ToArray();

open System    
open System.Text
    
let encode_base64 (s:string) = 
    let bytes = ASCIIEncoding.ASCII.GetBytes(s);
    Convert.ToBase64String(bytes);
    
let decode_base64 (s:string) =
     let bytes = Convert.FromBase64String(s);
     ASCIIEncoding.ASCII.GetString(bytes);
     
let eol = "\r\n"

let bytes (s:string) = Encoding.ASCII.GetBytes(s)

let EOL = bytes eol

let async_writeln (stream:Stream) s = async {
    let b = bytes s
    do! stream.AsyncWrite(b, 0, b.Length)
    do! stream.AsyncWrite(EOL, 0, 2)    
}

let async_writebytes (stream:Stream) b = async {
    do! stream.AsyncWrite(b, 0, b.Length)    
}

let unblock f =
    async { 
        do! Async.SwitchToNewThread ()
        let res = f()
        do! Async.SwitchToThreadPool ()
        return res 
    } 
