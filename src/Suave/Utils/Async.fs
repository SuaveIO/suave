[<AutoOpen>]
module Suave.Utils.Async

open System
open System.IO
open System.Threading.Tasks
open System.Threading

/// Helper to just invoke the three 'funcs' once.
let internal invokeOnce funcs =
  let counter = ref 0
  let invokeOnce' f x =
    if (Interlocked.CompareExchange (counter, 1, 0) = 0) then
      f x
  let (a, b, c) = funcs
  (invokeOnce' a, invokeOnce' b, invokeOnce' c)

type Microsoft.FSharp.Control.Async with
  /// Spawn an async with a timeout, throwing <see cref="System.TimeoutException" /> after
  /// the timeout.
  static member WithTimeout(timeout : TimeSpan, computation : Async<'T>) : Async<'T> =
    let callback (success, error, cancellation) =
      let (success, error, cancellation) = invokeOnce (success, error, cancellation)
      let fetchResult = async {
        try
          let! result = computation
          success result
        with ex ->
          error ex }
      let timeoutExpired = async {
        do! Async.Sleep (int timeout.TotalMilliseconds)
        let ex = new TimeoutException ("Timeout expired") :> Exception
        error ex }

      Async.StartImmediate fetchResult
      Async.StartImmediate timeoutExpired

    Async.FromContinuations callback

  /// Raise an exception on the async computation/workflow. 
  static member AsyncRaise (e : exn) = 
    Async.FromContinuations(fun (_,econt,_) -> econt e) 

  /// Await a task asynchronously
  static member AwaitTask (t : Task) =
    let flattenExns (e : AggregateException) = e.Flatten().InnerExceptions.[0]
    let rewrapAsyncExn (it : Async<unit>) =
      async { try do! it with :? AggregateException as ae -> do! Async.AsyncRaise (flattenExns ae) }
    let tcs = new TaskCompletionSource<unit>(TaskCreationOptions.None)
    t.ContinueWith((fun t' ->
      if t.IsFaulted then tcs.SetException(t.Exception |> flattenExns)
      elif t.IsCanceled then tcs.SetCanceled ()
      else tcs.SetResult(())), TaskContinuationOptions.ExecuteSynchronously)
    |> ignore
    tcs.Task |> Async.AwaitTask |> rewrapAsyncExn

type Microsoft.FSharp.Control.AsyncBuilder with

  /// An extension method that overloads the standard 'Bind' of the 'async' builder. The new overload awaits on
  /// a standard .NET task
  member x.Bind(t : Task<'T>, f:'T -> Async<'R>) : Async<'R> = async.Bind(Async.AwaitTask t, f)

  /// An extension method that overloads the standard 'Bind' of the 'async' builder. The new overload awaits on
  /// a standard .NET task which does not commpute a value
  member x.Bind(t : Task, f : unit -> Async<'R>) : Async<'R> = async.Bind(Async.AwaitTask t, f)

/// Haskell's TVar but without the STM.
type AsyncResultCell<'T>() =
  let source = new TaskCompletionSource<'T>()

  /// Complete the async result cell, setting the value. If this invocation was the first
  /// invocation, returns true, otherwise if there already is a value set, return false.
  member x.Complete result =
    source.TrySetResult result

  /// Await the result of the AsyncResultCell, yielding Some(:'T)
  /// after the timeout or otherwise None.
  member x.AwaitResult(?timeout : TimeSpan) = async {
    match timeout with
    | None ->
      let! res = source.Task
      return Some res
    | Some time ->
      try
        let! res = Async.WithTimeout(time, Async.AwaitTask(source.Task))
        return Some res
      with
      | :? TimeoutException as e ->
        return None
    }