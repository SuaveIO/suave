namespace Suave.Utils

open System
open System.Threading.Tasks
open Suave.Utils.AsyncExtensions

/// Haskell's TVar but without the STM. You can use this for your programs;
/// it performs a non-blocking wait for a single value. Use AwaitTimeout to get
/// its contained result as an 'T option
type AsyncResultCell<'T>() =
  let source = new TaskCompletionSource<'T>()

  /// Complete the async result cell, setting the value. If this invocation was
  /// the first invocation, returns true, otherwise if there already is a value
  /// set, return false.
  member x.complete result =
    source.TrySetResult result

  /// Await the result of the AsyncResultCell, yielding Some(:'T)
  /// after the timeout or otherwise None.
  member x.awaitResult(?timeout : TimeSpan) = async {
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