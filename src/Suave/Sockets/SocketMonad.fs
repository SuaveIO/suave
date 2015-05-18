namespace Suave.Sockets

open System
open System.Collections.Generic

/// Workflow builder to read/write to async sockets with fail/success semantics
type SocketMonad() =
  member this.Return(x:'a) : SocketOp<'a> = async{ return Choice1Of2 x }
  member this.Zero() : SocketOp<unit> = this.Return()
  member this.ReturnFrom(x : SocketOp<'a>) : SocketOp<'a> = x
  member this.Delay(f: unit ->  SocketOp<'a>) = async { return! f () }

  member this.Bind(x : SocketOp<'a>,f : 'a -> SocketOp<'b>) : SocketOp<'b> = async {
    let! result = x
    match result with
    | Choice1Of2 a -> return! f a
    | Choice2Of2 b -> return Choice2Of2 b
    }

  member this.Combine(v, f) = this.Bind(v, fun () -> f)

  member this.While(guard, body : SocketOp<unit>) : SocketOp<unit> = async {
    if guard() then
      let! result = body
      match result with
      | Choice1Of2 a ->
        return! this.While(guard, body)
      | Choice2Of2 _ ->
        return result
    else
      return! this.Zero()
    }

  member this.TryWith(body, handler) = async {
    try
      return! body
    with e ->
      return! handler e
    }

  member this.TryFinally(body, compensation) = async {
     try
       return! body
     finally
       compensation()
    }

  member this.Using(disposable : #System.IDisposable, body) = async {
    use _ = disposable
    return! body disposable
    }

  member this.For(sequence : seq<_>, body : 'a -> SocketOp<unit>) =
    this.Using(sequence.GetEnumerator(), fun (enum : IEnumerator<'a>) ->
    this.While(enum.MoveNext, this.Delay(fun _-> body enum.Current)))


[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SocketMonad =
  /// The socket monad   
  let socket = SocketMonad()