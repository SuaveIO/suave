module Suave.Tests.AsyncSocket

open System
open System.Text
open Expecto
open Expecto.Logging
open Suave.Sockets
open Suave.Tests.TestUtilities

[<Tests>]
let tests =

  let pretty (bytes: byte[]) =
    bytes
    |> Array.map (sprintf "0x%02x ")
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
      let sample = [| ("\ud835\udda5", [|240uy; 157uy; 150uy; 165uy|]) // Surrogate pair for U+1D5A5 MATHEMATICAL SANS-SERIF CAPITAL F
                      ("\ud834\udd30", [|240uy; 157uy; 132uy; 176uy|]) // Surrogate pair for U+1D130 MUSICAL SYMBOL SHARP UP
                      ("𝖥", [|240uy; 157uy; 150uy; 165uy|]) // Actual U+1D5A5 character in source file (still a surrogate pair internally)
                      ("𝄰", [|240uy; 157uy; 132uy; 176uy|]) // Actual U+1D130 character in source file (still a surrogate pair internally)
                   |]
      let str = UTF8String(sample, 0us, 0u).concat ()
      let dotnetBytes = Encoding.UTF8.GetBytes str
      let pretty = pretty dotnetBytes
      Expect.equal dotnetBytes.Length 16 (sprintf "Should output UTF8 bytes, but got\n%A\n%s" dotnetBytes pretty)
(*

  let usingConn bufSize fn =
    let writes = ResizeArray<_>()
    let tx = createStubTransport writes
    let conn =
      { transport = tx
        lineBuffer = Array.zeroCreate bufSize
        lineBufferCount = 0
        pipe = new Pipe ()
        socketBinding = SocketBinding.create IPAddress.Loopback 8080us
      }
    fn conn, writes

    testPropertyWithConfig fsCheckConfig "sum of bytes" <|
      fun (UTF8String (_, _, bytesCount) as sample, bufSize) ->
        if bufSize < 6 then
          true
        else
          let concatenated = sample.concat ()
          logger.verboseWithBP (fun level ->
            eventX "'sum of bytes' called with string length {len}" level
            |> setField "sample" sample
            |> setField "len" concatenated.Length)
          |> Async.RunSynchronously

          let mutable bufferedBytes = 0
          let _, writes =
            usingConn bufSize <| fun (conn:Connection) ->
              let res = conn.asyncWrite concatenated
              let (res) = res.Result
              Expect.isOk res "Should write to stub successfully"
              match res with
              | Ok (_) ->
                // Count the bytes that are left in the lineBuffer
                bufferedBytes <- bufferedBytes + conn.lineBufferCount
              | _ ->
                failwith "does not happen"

          if uint32 bufSize < bytesCount then
            Expect.isGreaterThan writes.Count 0 "Should have written at least one buffer"

          let bytesWritten = uint32 (writes |> Seq.sumBy (fun bs -> bs.Length)) + (uint32 bufferedBytes)
          Expect.equal bytesWritten bytesCount "The generated # bytes should equal the # bytes written"
          true

          *)
  ]