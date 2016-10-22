/// For F# 3.0, you can open this module to get extensions for Async<'a>:
///
/// - Support for bind on Task<'a> in async{}
/// - Async.WithTimeout : TimeSpan * Async<'a> -> _
/// - Async.AsyncRaise : exn -> _
/// - Async.AwaitTask : Task -> _
///
/// For F# 4.0 these things are mostly implemented, so you can choose not to
/// open this module.
module Suave.Utils.AsyncExtensions

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
  /// Spawn an async with a timeout, throwing <see cref="System.TimeoutException" />
  /// after the timeout.
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

type System.Threading.Tasks.Task<'T> with
  /// From Task-based asynchronous model to Begin/End pattern
  static member ToIAsyncResult<'T>(task: Task<'T>,callback: AsyncCallback,state:obj) : IAsyncResult =

    let tcs = new TaskCompletionSource<'T>(state)

    let routine (t:Task<'T>) = 
       let x = 
         if (t.IsFaulted) then
           tcs.TrySetException(t.Exception.InnerExceptions)
         else if (t.IsCanceled) then 
           tcs.TrySetCanceled()
         else 
           tcs.TrySetResult(t.Result)

       if (callback <> null) then
          callback.Invoke(tcs.Task)

    let t = task.ContinueWith( routine, TaskScheduler.Default)
    tcs.Task :> IAsyncResult