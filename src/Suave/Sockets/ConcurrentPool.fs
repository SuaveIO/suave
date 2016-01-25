namespace Suave.Sockets

open System
open System.Collections.Concurrent

type ConcurrentPool<'T>() =

  let objects = ConcurrentBag<'T> ()

  let mutable objectGenerator: Func<'T> = null

  member x.Pop() =
   match objects.TryTake()with
   | true, item ->
     item
   | _,_ -> objectGenerator.Invoke()

  member x.Push(item) =
      objects.Add(item)

  member this.ObjectGenerator
    with get () = objectGenerator
    and set (value) = objectGenerator <- value
