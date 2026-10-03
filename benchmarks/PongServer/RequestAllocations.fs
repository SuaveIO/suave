module RequestAllocations

open System
open System.Diagnostics
open System.Threading.Tasks
open Suave
open Suave.Successful

type private ErrorBoundary(errorHandler : exn -> Async<HttpContext option>) =
  member _.Original workflow = async {
    try
      let! result = workflow
      return result
    with error -> return! errorHandler error
  }

  member _.TailReturn workflow = async {
    try return! workflow
    with error -> return! errorHandler error
  }

  member _.Direct workflow = async.TryWith(workflow, errorHandler)

let private verifyBoundaries result =
  for name, wrap in [
    "original", (fun (boundary : ErrorBoundary) -> boundary.Original)
    "tail return", (fun boundary -> boundary.TailReturn)
    "direct TryWith", (fun boundary -> boundary.Direct)
  ] do
    let mutable errors = 0
    let boundary = ErrorBoundary(fun _ -> errors <- errors + 1; async.Return result)
    let invoke workflow = Async.StartImmediateAsTask(wrap boundary workflow)
    let expectResult (pending : Task<HttpContext option>) =
      if pending.GetAwaiter().GetResult().IsNone then failwith (name + ": missing result")
    invoke (async.Return result) |> expectResult
    invoke (async { return raise (InvalidOperationException "probe") }) |> expectResult
    invoke (async {
      do! Async.Sleep 1
      return result
    }) |> expectResult
    invoke (async { return raise (OperationCanceledException "ordinary exception") }) |> expectResult
    let canceled =
      invoke (Async.FromContinuations(fun (_, _, cancel) -> cancel (OperationCanceledException "cancellation continuation")))
    try
      canceled.GetAwaiter().GetResult() |> ignore
      failwith (name + ": expected cancellation")
    with :? OperationCanceledException -> ()
    if not canceled.IsCanceled || errors <> 2 then
      failwith (name + ": changed error/cancellation handling")
  printfn "Boundary behavior checks passed."

let run () =
  let context =
    { HttpContext.empty with
        request = { HttpRequest.empty with rawPath = "/"; rawMethod = "GET" } }
  let result = Some context
  let completedTask = Task.FromResult result
  let completedAsync = async.Return result
  let output = HttpOutput(Unchecked.defaultof<_>, Unchecked.defaultof<_>)
  let boundary = ErrorBoundary(fun error -> async { return raise error })
  let handler = OK ""
  let routed =
    Suave.Router.router {
      get "/" handler
      get "/user/:id" (fun ctx -> OK (Suave.Router.routeParam "id" ctx |> Option.defaultValue "") ctx)
      post "/user" handler
    }
  let cases : (string * (unit -> Task<HttpContext option>)) list = [
    "Task.FromResult control", (fun () -> Task.FromResult result)
    "task awaits completed Task", (fun () -> task { return! completedTask })
    "StartImmediateAsTask, prebuilt Async", (fun () -> Async.StartImmediateAsTask completedAsync)
    "task awaits prebuilt Async", (fun () -> task { return! completedAsync })
    "task awaits fresh Async.Return", (fun () -> task { return! async.Return result })
    "task awaits executeTask(prebuilt Async)", (fun () -> task { return! output.executeTask completedAsync })
    "task awaits OK WebPart", (fun () -> task { return! handler context })
    "task awaits executeTask(OK WebPart)", (fun () -> task { return! output.executeTask (handler context) })
    "task awaits routed WebPart", (fun () -> task { return! routed context })
    "task awaits executeTask(routed WebPart)", (fun () -> task { return! output.executeTask (routed context) })
    "prototype original wrapper, routed", (fun () -> task { return! boundary.Original (routed context) })
    "prototype tail-return wrapper, routed", (fun () -> task { return! boundary.TailReturn (routed context) })
    "prototype direct TryWith, routed", (fun () -> task { return! boundary.Direct (routed context) })
    "Task to Async to Task", (fun () -> task { return! Async.AwaitTask completedTask })
  ]
  let consume (pending : Task<HttpContext option>) =
    if not pending.IsCompletedSuccessfully then
      failwith "This probe requires synchronous success for thread-local allocation accounting."
    if pending.GetAwaiter().GetResult().IsNone then
      failwith "Expected the WebPart to handle the request."
  verifyBoundaries result
  printfn "Runtime: %O; FSharp.Core: %O; server GC: %b" Environment.Version (typeof<Async<int>>.Assembly.GetName().Version) System.Runtime.GCSettings.IsServerGC
  printfn "Round\tCase\tBytes/op\tNanoseconds/op"
  let iterations = 200000
  for round in 1 .. 3 do
    for name, invoke in cases do
      for warmup in 1 .. 20000 do
        invoke () |> consume
      let timer = Stopwatch()
      let before = GC.GetAllocatedBytesForCurrentThread()
      timer.Start()
      for iteration in 1 .. iterations do
        invoke () |> consume
      timer.Stop()
      let allocated = GC.GetAllocatedBytesForCurrentThread() - before
      printfn "%d\t%s\t%.1f\t%.1f" round name (float allocated / float iterations) (timer.Elapsed.TotalNanoseconds / float iterations)
  0