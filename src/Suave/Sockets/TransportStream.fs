namespace Suave.Sockets

open System
open System.IO
open System.Threading.Tasks

open Suave.Utils.AsyncExtensions

type TransportStream(transport : ITransport) =
  inherit Stream()
  override x.CanRead with get() = true
  override x.CanSeek with get() = false
  override x.CanWrite with get() = true
  override x.Flush() = ()

  override x.Seek(offset : int64, origin : SeekOrigin) =
    raise (NotImplementedException())

  override x.SetLength(value : int64) =
    raise (NotImplementedException())

  override x.Read (buffer : byte[],offset : int,count : int) : int =
    match Async.RunSynchronously <| transport.read (Memory(buffer,offset,count)) with
    | Ok n -> n
    | Error x -> raise (Exception(x.ToString()))

  override x.ReadAsync (buffer : byte[],offset : int,count : int, ct) =

    let task = async{ 
      let! a = transport.read(Memory(buffer,offset,count))
      match a with
      | Ok x -> return x
      | Error e -> return failwith (e.ToString())
      }
    Async.StartAsTask (task, TaskCreationOptions.AttachedToParent,ct)

  override x.BeginRead(buffer : byte[],offset : int,count : int,  callback : AsyncCallback, state : obj) : IAsyncResult=
    let task = x.ReadAsync(buffer,offset,count)
    Task<_>.ToIAsyncResult(task,callback,state)

  override x.EndRead(ar:IAsyncResult) =
    let task = ar :?> Task<int> 
    task.Result

  override x.Write (buffer : byte[],offset : int,count : int) =
    match Async.RunSynchronously <| transport.write(Memory(buffer,offset,count)) with
    | Ok _ -> ()
    | Error x -> raise (Exception(x.ToString()))

  override x.WriteAsync (buffer : byte[],offset : int,count : int, ct) =
    let task = async{ 
      let! a = transport.write(Memory(buffer,offset,count))
      match a with
      | Ok x -> return x
      | Error e -> return failwith (e.ToString())
      }
    Async.StartAsTask (task, TaskCreationOptions.AttachedToParent,ct) :> Task

  override x.BeginWrite(buffer : byte[],offset : int,count : int,  callback : AsyncCallback, state : obj) : IAsyncResult=
    let task = x.WriteAsync(buffer,offset,count)
    Task<_>.ToIAsyncResult(task :?> Task<unit>,callback,state)

  override x.EndWrite(ar:IAsyncResult) =
    let task = ar :?> Task<unit> 
    task.Result

  override x.Length
    with get() = raise (NotImplementedException())

  override x.Position
    with get() = raise (NotImplementedException())
    and  set(v) = raise (NotImplementedException())