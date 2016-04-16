namespace Suave.Sockets

open System
open System.IO
open System.Threading.Tasks

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
    match Async.RunSynchronously <| transport.read (ArraySegment(buffer,offset,count)) with
    | Choice1Of2 n -> n
    | Choice2Of2 x -> raise (Exception(x.ToString()))

  override x.Write (buffer : byte[],offset : int,count : int) =
    match Async.RunSynchronously <| transport.write(ArraySegment(buffer,offset,count)) with
    | Choice1Of2 _ -> ()
    | Choice2Of2 x -> raise (Exception(x.ToString()))

  override x.WriteAsync (buffer : byte[],offset : int,count : int, ct) =
    Async.StartAsTask (transport.write(ArraySegment(buffer,offset,count)), TaskCreationOptions.AttachedToParent,ct) :> Task

  override x.Length
    with get() = raise (NotImplementedException())

  override x.Position
    with get() = raise (NotImplementedException())
    and  set(v) = raise (NotImplementedException())