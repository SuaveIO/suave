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

  let fsCheckConfig =
    { FsCheckConfig.defaultConfig with
        startSize = 1
        endSize = 20000
        maxTest = 2000
        arbitrary =
          [
            typeof<UTF8Strings>
          ] }

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

  let pretty (bytes: byte[]) =
    bytes
    |> Array.mapi (fun i b ->
        [|
          yield "0x"
          yield Convert.ToString(b, 16)
          yield " "
        |])
    |> Array.concat
    |> String.Concat

  testList "AsyncSocket" [
    testCase "<= 3 byte chars" <| fun _ ->
      let sample =
        [|("毠", [|230uy; 175uy; 160uy|])
          ("Ə", [|198uy; 143uy|])
          ("j", [|106uy|])
          ("㟴", [|227uy; 159uy; 180uy|]) |]
      let str = UTF8String(sample, 0us, 0u).concat ()
      let dotnetBytes = Encoding.UTF8.GetBytes str
      let pretty = pretty dotnetBytes
      Expect.equal dotnetBytes.Length 9 (sprintf "Should output UTF8 bytes, but got\n%A\n%s" dotnetBytes pretty)

    testCase "4 byte chars" <| fun _ ->
      let sample = [| ("���", [|244uy; 179uy; 138uy; 133uy|]) |]
      let str = UTF8String(sample, 0us, 0u).concat ()
      let dotnetBytes = Encoding.UTF8.GetBytes str
      let pretty = pretty dotnetBytes
      // https://en.wikipedia.org/wiki/Specials_(Unicode_block)#Replacement_character

      Expect.equal dotnetBytes.Length 9
                   "Should output three replacement chars (which are three bytes)"
      Expect.sequenceEqual (Encoding.UTF8.GetBytes "���") dotnetBytes
                           "Should output three replacement chars"

    testPropertyWithConfig fsCheckConfig "sum of bytes" <|
      fun (UTF8String (_, _, _) as sample, bufSize) ->
        if bufSize < 6 then
          true 
        else
          let concatenated = sample.concat ()
          let bytesCount = uint32((Encoding.UTF8.GetBytes concatenated).Length)

          logger.debugWithBP (
            eventX "'sum of bytes' called with string length {len}"
            >> setField "sample" sample
            >> setField "len" concatenated.Length)
          |> Async.RunSynchronously

          let mutable bufferedBytes = 0
          let _, writes =
            usingConn bufSize <| fun conn ->
              let (res: Async<Choice<unit * Connection, Error>>) = AsyncSocket.asyncWrite concatenated conn
              let (res: Choice<unit * Connection, Error>) = Async.RunSynchronously res
              Expect.isChoice1Of2 res "Should write to stub successfully"
              match res with
              | Choice1Of2 (_, cn) ->
                // Count the bytes that are left in the lineBuffer
                bufferedBytes <- bufferedBytes + cn.lineBufferCount
              | _ ->
                failwith "does not happen"

          if uint32 bufSize < bytesCount then
            Expect.isGreaterThan writes.Count 0 "Should have written at least one buffer"

          let bytesWritten = uint32 (writes |> Seq.sumBy (fun bs -> bs.Length)) + (uint32 bufferedBytes)
          Expect.equal bytesWritten bytesCount "The generated # bytes should equal the # bytes written"
          true
  ]