namespace Suave.Sockets

open System
open System.Collections.Generic
open System.Net.Sockets

type ConcurrentPool<'T>() =

  let pool = new Stack<'T>()

  member x.Push(item : 'T) =
    lock pool (fun _ -> pool.Push item)

  member x.Pop() =
    lock pool (fun _ -> pool.Pop())
