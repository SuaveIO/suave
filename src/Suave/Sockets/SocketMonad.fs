namespace Suave.Sockets.Control

open System
open System.Collections.Generic
open System.Threading.Tasks
open Suave.Sockets

/// Workflow builder to read/write to async sockets with fail/success semantics
type SocketMonad() =
  member this.Return(x:'a) : SocketOp<'a> = ValueTask<Result<'a,Error>>(Ok x)
  member this.Zero() : SocketOp<unit> = this.Return()
  member this.ReturnFrom(x : SocketOp<'a>) : SocketOp<'a> = x
  member this.Delay(f: unit ->  SocketOp<'a>) = ValueTask<Result<'a,Error>>(task { return! (f ()).AsTask() })

  member this.Bind(x : SocketOp<'a>,f : 'a -> SocketOp<'b>) : SocketOp<'b> =
    ValueTask<Result<'b,Error>>(
      task {
        let! result = x.AsTask()
        match result with
        | Ok a -> return! (f a).AsTask()
        | Error b -> return Result.Error b
      })
 
  member this.Combine(v, f) =
    this.Bind(v, fun () -> f)

  // This is buggy avoid using
  member this.While(guard, body : SocketOp<unit>) : SocketOp<unit> =
    ValueTask<Result<unit,Error>>(
      task{
        let mutable error = false
        let mutable errorResult = Ok()
        while not error && guard () do
          let! a = body.AsTask()
          match a with
          | Ok () -> ()
          | Result.Error e as a ->
            error <- true
            errorResult <- a
        if error then
          return errorResult
        else
          return Ok()
      })

  member this.TryWith(body : SocketOp<'a>, handler : exn -> SocketOp<'a>) : SocketOp<'a> =
    ValueTask<Result<'a,Error>>(
      task {
        try
          return! body.AsTask()
        with e ->
          return! (handler e).AsTask()
      })

  member this.TryFinally(body : SocketOp<'a>, compensation : unit -> unit) : SocketOp<'a> =
    ValueTask<Result<'a,Error>>(
      task {
         try
           return! body.AsTask()
         finally
           compensation()
      })

  member this.Using(disposable : 'a, body : 'a -> SocketOp<'b>) : SocketOp<'b> when 'a :> IDisposable =
    ValueTask<Result<'b,Error>>(
      task {
        use _ = disposable
        return! (body disposable).AsTask()
      })

  member this.For(sequence : seq<_>, body : 'a -> SocketOp<unit>) =
    (fun (enum : IEnumerator<'a>) -> 
      ValueTask<Result<unit,Error>>(
        task {
          let mutable good = true
          let mutable result = Ok ()
          while good && enum.MoveNext() do
            let! a = (body enum.Current).AsTask()
            match a with
            | Ok _ -> ()
            | Result.Error e ->
              good <- false
              result <- Result.Error e
          return result 
        })) (sequence.GetEnumerator())

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SocketMonad =
  /// The socket monad
  let socket = SocketMonad()
