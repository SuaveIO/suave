[<AutoOpen>]
module Suave.WebPart
(*
SuaveTask of 'a is defined as `Async<'a option>`. It's implied that by returning None, the SuaveTask expects Suave to
continue on to the next (in the `choose` below). Another name for `SuaveTask` is `AsyncOption` as can be seen in the
builder definition below.

WebPart of 'a is defined as a function that takes 'a and returns SuaveTask of 'a or AsyncOption of 'a

WebPart without a specific type parameter is understood as WebPart of HttpContext.

*)
type WebPart<'a> = 'a -> Async<'a option>
// WebPart
let inline succeed x = async.Return (Some x)
// SuaveTask
let fail<'a> : Async<'a option> = async.Return (Option<'a>.None)
// WebPart
let never : WebPart<'a> = fun x -> fail
// Operates on SuaveTask
let bind (f: 'a -> Async<'b option>) (a: Async<'a option>) = async {
  let! p = a
  match p with
  | None ->
    return None
  | Some q ->
    let r = f q
    return! r
  }
// Operates on SuaveTask
let compose (first : 'a -> Async<'b option>) (second : 'b -> Async<'c option>)
            : 'a -> Async<'c option> =
  fun x ->
    bind second (first x)

type AsyncOptionBuilder() =
  member this.Return(x:'a) : Async<'a option> = async { return Some x }
  member this.Zero() : Async<unit option> = this.Return()
  member this.ReturnFrom(x : Async<'a option>) = x
  member this.Delay(f: unit ->  Async<'a option>) = async { return! f () }
  member this.Bind(x :Async<'a option>, f : 'a -> Async<'b option>) : Async<'b option> = bind f x

let asyncOption = AsyncOptionBuilder()

let rec choose (options : WebPart<'a> list) : WebPart<'a> =
  fun arg -> async {
  match options with
  | []        -> return None
  | p :: tail ->
    let! res = p arg
    match res with
    | Some x -> return Some x
    | None   -> return! choose tail arg
  }

let rec inject (postOp : WebPart<'a>) (pairs : (WebPart<'a> * WebPart<'a>) list) : WebPart<'a> =
  fun arg -> async {
    match pairs with
    | []        -> return None
    | (p,q) :: tail ->
      let! res = p arg
      match res with
      | Some x ->
        return! (compose postOp q) x
      | None   -> return! inject postOp tail arg
    }

let inline warbler f a = f a a

let inline cnst x = fun _ -> x

let cond d f g a =
  match d with
  | Choice1Of2 x -> f x a
  | Choice2Of2 _ -> g a

let inline tryThen (a : WebPart<'a>) (b : WebPart<'a>) : WebPart<'a> =
  fun x ->
    async {
      let! e = a x
      match e with
      | None -> return! b x
      | r -> return r
    }

let inline concatenate a b = fun x ->
    match a x with
    | None   -> b x
    | r      -> r
