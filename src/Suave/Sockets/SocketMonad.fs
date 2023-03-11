namespace Suave.Sockets.Control

open System
open System.Collections.Generic
open Suave.Sockets

/// Workflow builder to read/write to async sockets with fail/success semantics
type SocketMonad() =
  member this.Return(x:'a) : SocketOp<'a> = task { return Ok x }
  member this.Zero() : SocketOp<unit> = this.Return()
  member this.ReturnFrom(x : SocketOp<'a>) : SocketOp<'a> = x
  member this.Delay(f: unit ->  SocketOp<'a>) = task { return! f () }

  member this.Bind(x : SocketOp<'a>,f : 'a -> SocketOp<'b>) : SocketOp<'b> =
    task {
    let! result = x
    match result with
    | Ok a -> return! (f a)
    | Error b -> return Result.Error b
    }
 
  member this.Combine(v, f) =
    this.Bind(v, fun () -> f)

  // This is buggy avoid using
  member this.While(guard, body : SocketOp<unit>) : SocketOp<unit> =
    task{
      let error = ref false
      let errorResult = ref (Ok())
      while not !error && guard () do
        let! a = body
        match a with
        | Ok () -> ()
        | Result.Error e as a ->
          error := true
          errorResult := a
      if !error then
        return !errorResult
      else
        return Ok()
    }
    (*task {
      if guard() then
        let! result = body
        match result with
        | Ok () ->
          return! this.While(guard, body)
        | Error x ->
          return Result.Error x
      else
        return Ok()
      }*)

  member this.TryWith(body, handler) =
    task {
      try
        return! body
      with e ->
        return! handler e
      }

  member this.TryFinally(body, compensation) =
    task {
       try
         return! body
       finally
         compensation()
      }

  member this.Using(disposable, body) =
    task {
      use _ = disposable
      return! body disposable
      }

  member this.For(sequence : seq<_>, body : 'a -> SocketOp<unit>) =
    (fun (enum : IEnumerator<'a>) -> task {
      let good = ref true
      let result = ref (Ok ())
      while !good && enum.MoveNext() do
        let! a = body enum.Current
        match a with
        | Ok _ -> ()
        | Result.Error e ->
          good := false
          result := Result.Error e
      return !result 
    }) (sequence.GetEnumerator())

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SocketMonad =
  /// The socket monad
  let socket = SocketMonad()

