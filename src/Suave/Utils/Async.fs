namespace Suave.Utils

module Async =

  open System
  open System.Threading.Tasks

  type Microsoft.FSharp.Control.AsyncBuilder with
    /// An extension method that overloads the standard 'Bind' of the 'async' builder. The new overload awaits on
    /// a standard .NET task
    member x.Bind(t: Task<'T>, f:'T -> Async<'R>): Async<'R> =
      async.Bind(Async.AwaitTask t, f)

    /// An extension method that overloads the standard 'Bind' of the 'async' builder. The new overload awaits on
    /// a standard .NET task which does not commpute a value
    member x.Bind(t: Task, f: unit -> Async<'R>): Async<'R> =
      async.Bind(Async.AwaitTask t, f)


  let result = async.Return

  let map f value = async {
    let! v = value
    return f v
  }

  let bind f xAsync = async {
    let! x = xAsync
    return! f x
  }

  let withTimeout timeoutMillis operation =
    async {
      let! child = Async.StartChild(operation, timeoutMillis)
      try
        let! result = child
        return Some result
      with :? TimeoutException ->
        return None
    }

  let apply fAsync xAsync = async {
    // start the two asyncs in parallel
    let! fChild = Async.StartChild fAsync
    let! xChild = Async.StartChild xAsync

    // wait for the results
    let! f = fChild
    let! x = xChild

    // apply the function to the results
    return f x
  }

  let lift2 f x y =
    apply (apply (result f) x) y

  let lift3 f x y z =
    apply (apply (apply (result f) x) y) z

  let lift4 f x y z a =
    apply (apply (apply (apply (result f) x) y) z) a

  let lift5 f x y z a b =
    apply (apply (apply (apply (apply (result f) x) y) z) a) b

  module Operators =

    let inline (>>=) m f =
      bind f m

    let inline (=<<) f m =
      bind f m

    let inline (<*>) f m =
      apply f m

    let inline (<!>) f m =
      map f m

    let inline ( *>) m1 m2 =
      lift2 (fun _ x -> x) m1 m2

    let inline ( <*) m1 m2 =
      lift2 (fun x _ -> x) m1 m2

