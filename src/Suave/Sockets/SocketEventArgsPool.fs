namespace Suave.Sockets

open System
open System.Collections.Generic
open System.Net.Sockets

type SocketAsyncEventArgsPool() =

  let pool = new Stack<SocketAsyncEventArgs>()

  member x.Push(item : SocketAsyncEventArgs) =
    lock pool (fun _ -> pool.Push item)

  member x.Pop() =
    lock pool (fun _ -> pool.Pop())
