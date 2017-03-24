module Suave.Tests.AsyncSocket

open System
open System.Text
open System.Net
open FsCheck
open Expecto
open Expecto.Logging
open Expecto.Logging.Message
open Suave
open Suave.Sockets
open Suave.Tests.TestUtilities

[<Tests>]
let tests =
  let logger = Log.create "AsyncSocket"

  let createStubTransport (writes: ResizeArray<byte[]>) =
    { new ITransport with
        member x.write (buf: ByteSegment) =
          let written = Array.zeroCreate<byte> buf.Count
          Buffer.BlockCopy(buf.Array, buf.Offset, written, 0, buf.Count)
          writes.Add written
          async.Return (Choice.create ())

        member x.read (buf: ByteSegment): SocketOp<int> =
          failwith "Read is not supported in the stub"

        member x.shutdown () =
          async.Return ()
    }

  let usingConn bufSize fn =
    let writes = ResizeArray<_>()
    let tx = createStubTransport writes
    let conn =
      let bufSize = max 6 bufSize
      let bm = BufferManager (bufSize * 1000, bufSize, true)
      { transport = tx
        bufferManager = bm
        lineBuffer = bm.PopBuffer "First buffer"
        segments = []
        lineBufferCount = 0
        socketBinding = SocketBinding.create IPAddress.Loopback 8080us
      }
    fn conn, writes

  testList "AsyncSocket" [
    ftestPropertyWithConfig (1,1) fsCheckConfig "sum of bytes" <|
      fun (UTF8String (str, _, bytesCount) as sample, bufSize) ->
        if str.Length < 6 then true else
        let concatenated = sample.concat ()

        logger.debugWithBP (
          eventX "'sum of bytes' called with string length {len}"
          >> setField "sample" sample
          >> setField "len" concatenated.Length)
        |> Async.RunSynchronously

        let _, writes =
          usingConn bufSize <| fun conn ->
            let (res: Async<Choice<unit * Connection, Error>>) = AsyncSocket.asyncWrite concatenated conn
            let (res: Choice<unit * Connection, Error>) = Async.RunSynchronously res
            Expect.isChoice1Of2 res "Should write to stub successfully"

        Expect.isGreaterThan writes.Count 0 "Should have written at least one buffer"

        let bytesWritten = uint32 (writes |> Seq.sumBy (fun bs -> bs.Length))
        Expect.equal bytesWritten bytesCount "The generated # bytes should equal the # bytes written"
        true
  ]