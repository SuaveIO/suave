namespace Suave.Sockets

open System
open System.Collections.Generic
open System.Net.Sockets

type SocketAsyncEventArgsPool() =

  let m_pool = new Stack<SocketAsyncEventArgs>()

  member x.Push(item : SocketAsyncEventArgs) =
    lock m_pool (fun _ -> m_pool.Push item)

  member x.Pop() =
    lock m_pool (fun _ -> m_pool.Pop())
