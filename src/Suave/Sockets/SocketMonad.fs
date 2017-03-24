namespace Suave.Sockets.Control

open System
open System.Collections.Generic
open Suave.Sockets

/// Workflow builder to read/write to async sockets with fail/success semantics
type SocketMonad() =
  member this.Return(x:'a) : SocketOp<'a> = async { return Choice1Of2 x }
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
    this.Using(sequence.GetEnumerator(),
      fun (enum : IEnumerator<'a>) -> this.While(enum.MoveNext, this.Delay(fun _-> body enum.Current)))


[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SocketMonad =
  /// The socket monad
  let socket = SocketMonad()


  type SocketState<'s,'a> = 's -> SocketOp<'a * 's>

  type SocketStateBuilder<'s>() =
    member x.Return v : SocketState<'s,_> = fun s -> socket{ return v,s }
    member x.Bind(v, f) : SocketState<'s,_> =
      fun s -> socket {
        let! (a,s) = v s
        let v' = f a
        return! v' s }
    member x.Zero() = x.Return()
    member x.Combine(v, f) = x.Bind(v, fun () -> f)
    member x.Delay(f: unit ->  SocketState<'s,'a>) : SocketState<'s,'a> = fun s -> f () s
    member x.Using(disposable : #System.IDisposable, body : 'a -> SocketState<'s,_>) = fun s -> socket {
      use _ = disposable
      return! body disposable s
    }
    member x.While(guard, body : SocketState<'s,_>) : SocketState<'s,_> = fun s -> socket {
      if guard() then
        let! (_,s') = body s
        return! x.While(guard, body) s'
      else
        return! x.Zero() s
      }
    member x.For(sequence : seq<_>, body : 'a -> SocketState<'s,unit>) =
      fun s ->
        x.Using(sequence.GetEnumerator(),
          fun (enum : IEnumerator<'a>) -> x.While(enum.MoveNext, x.Delay(fun _-> body enum.Current))) s

  let withConnection = SocketStateBuilder<Connection>()

