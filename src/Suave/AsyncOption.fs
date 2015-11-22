namespace Suave

/// The base monad
type AsyncOption<'a> = Async<'a option>

module AsyncOption =

  /// Classic bind
  let bind (f: 'a -> AsyncOption<'b>) (a: AsyncOption<'a>) = async {
    let! p = a
    match p with
    | None ->
      return None
    | Some q ->
      let r = f q
      return! r
    }

  /// Left-to-right Kleisli composition.
  let compose (first : 'a -> AsyncOption<'b>)  (second : 'b -> AsyncOption<'c>) : 'a -> AsyncOption<'c> =
    fun x ->
        bind second (first x)

  type AsyncOptionBuilder() =
    member this.Return(x:'a) : AsyncOption<'a> = async { return Some x }
    member this.Zero() : AsyncOption<unit> = this.Return()
    member this.ReturnFrom(x : AsyncOption<'a>) : AsyncOption<'a> = x
    member this.Delay(f: unit ->  AsyncOption<'a>) = async { return! f () }
    member this.Bind(x : AsyncOption<'a>, f : 'a -> AsyncOption<'b>) : AsyncOption<'b> = bind f x

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

  module Operators =

    let (>>=) a b = bind b a

    let (>=>) a b = compose a b

