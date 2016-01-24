namespace Suave.Sockets

open System
open System.Collections.Concurrent

type ConcurrentPool<'T when 'T:(new: unit -> 'T)>() =

  let objects = ConcurrentBag<'T> ()

  member x.Pop() =
   match objects.TryTake()with
   | true, item ->
     item
   | _,_ -> new 'T()

  member x.Push(item) =
      objects.Add(item)