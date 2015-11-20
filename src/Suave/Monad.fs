namespace Suave.Http

/// This module contact the correct operator definitions.
module Monad =

  /// The base monad
  type AsyncOption<'a> = Async<'a option>

  let bind (a: AsyncOption<'a>) (f: 'a -> AsyncOption<'b>) = async {
    let! p = a
    match p with
    | None ->
      return None
    | Some q ->
      let r = f q
      return! r
    }

  /// Classic bind
  let (>>=) a b = bind a b

  /// Left-to-right Kleisli composition of monads.
  let (>=>) (first : 'a -> AsyncOption<'b>)  (second : 'b -> AsyncOption<'c>) : 'a -> AsyncOption<'c> =
    fun x ->
      bind (first x) second

  type AsyncOptionBuilder() =
    member this.Return(x:'a) : AsyncOption<'a> = async { return Some x }
    member this.Zero() : AsyncOption<unit> = this.Return()
    member this.ReturnFrom(x : AsyncOption<'a>) : AsyncOption<'a> = x
    member this.Delay(f: unit ->  AsyncOption<'a>) = async { return! f () }
    member this.Bind(x : AsyncOption<'a>, f : 'a -> AsyncOption<'b>) : AsyncOption<'b> = bind x f

  ///  With this workflow you can write WebParts like this
  ///  let task ctx = asyncOption {
  ///    let! _ = GET ctx
  ///    let! ctx = Writers.setHeader "foo" "bar"
  ///    return ctx
  ///  }
  ///
  ///  we can still use the old symbol but now has a new meaning
  ///  let foo ctx = GET ctx >>= OK "hello"
  ///

  let asyncOption = AsyncOptionBuilder()

  /// Concatenate f
  let inline (@@) a b = fun x ->
    match a x with
    | None   -> b x
    | r      -> r

  let inline (<|>) (a : WebPart) (b : WebPart) : WebPart =
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