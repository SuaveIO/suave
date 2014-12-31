namespace Suave.Sockets

open System.Net.Sockets

type AsyncUserToken(?socket : Socket) =
  let mutable _socket = match socket with Some x -> x | None -> null
  let mutable _continuation : SocketAsyncEventArgs -> unit = fun _ -> ()
  member x.Socket
    with get () = _socket and set a = _socket <- a
  member x.Continuation
    with get () = _continuation and set a = _continuation <- a