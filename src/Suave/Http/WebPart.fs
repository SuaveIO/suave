namespace Suave.Http

[<AutoOpen>]
module WebPart =

  open Operators

  let inline succeed x = async.Return (Some x)

  let fail = async.Return None

  let never : WebPart = fun x -> fail

  let rec choose (options : WebPart list) : WebPart =
    fun arg -> async {
    match options with
    | []        -> return None
    | p :: tail ->
      let! res = p arg 
      match res with
      | Some x -> return Some x
      | None   -> return! choose tail arg
    }

  /// Inject a webPart
  ///
  /// +------------+                                            +--------------+
  /// | url "/a"   +----------+                       +---------+   cont1      |
  /// +------------+          |                       |         +--------------+
  ///                         |                       |                         
  /// +-------------+         |       +----------+    |         +--------------+
  /// |  url "/b"   +---------+-------+ injected +----+---------+  cont2       |
  /// +-------------+         |       +----------+    |         +--------------+
  ///                         |                       |                         
  /// +-------------+         |                       |         +--------------+
  /// | url "/b"    +---------+                       +---------+  cont3       |
  /// +-------------+                                           +--------------+

  let rec inject (postOp : WebPart) (pairs : (WebPart*WebPart) list) : WebPart =
    fun arg -> async {
      match pairs with
      | []        -> return None
      | (p,q) :: tail ->
        let! res = p arg
        match res with
        | Some x ->
          return! (postOp >>= q) x
        | None   -> return! inject postOp tail arg
      }

  let inline warbler f a = f a a //which bird? A Warbler!

  let inline cnst x = fun _ -> x

  let cond d f g a =
    match d with
    | Choice1Of2 x -> f x a
    | Choice2Of2 _ -> g a


  let inline tryThen (a : WebPart) (b : WebPart) : WebPart =
    fun x ->
      async {
        let! e = a x
        match e with
        | None ->
          let! result = b x
          match result with
          | None -> return None
          | r -> return r
        | r -> return r
      }

  let inline concatenate a b = fun x ->
      match a x with
      | None   -> b x
      | r      -> r
    
  module Operators =

    let inline (<|>) a b = tryThen a b

    let inline (@@) a b = concatenate a b
