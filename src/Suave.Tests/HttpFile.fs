module Suave.Tests.HttpFile

open Fuchu

open System
open System.IO
open System.Text
open System.Net.Sockets

open Suave.Types
open Suave.Http
open Suave.Http.Successful
open Suave.Utils

open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let ``canonicalization attacks`` =
  testList "canonicalization attacks" [
    testCase "should throw" <| fun _ ->
      Assert.Raise("'../../passwd' is not a valid path",
        typeof<Exception>,
        fun _ -> Files.resolvePath currentPath "../../passwd" |> ignore)
  ]

[<Tests>]
let compression =
  let runWithConfig = runWith defaultConfig

  let testFileSize = (new FileInfo(Path.Combine(currentPath,"test-text-file.txt"))).Length

  testList "getting basic gzip/deflate responses" [
      testCase "200 OK returns 'Havana' with gzip " <| fun _ ->
        Assert.Equal("expecting 'Havana'", "Havana", runWithConfig (OK "Havana") |> reqGZip HttpMethod.GET "/" None)

      testCase "200 OK returns 'Havana' with deflate " <| fun _ ->
        Assert.Equal("expecting 'Havana'", "Havana", runWithConfig (OK "Havana") |> reqDeflate HttpMethod.GET "/" None)

      testCase "verifiying we get the same size uncompressed" <| fun _ ->
        Assert.Equal("length should match"
        , testFileSize
        , (runWithConfig (Files.browseFileHome "test-text-file.txt") |> reqBytes HttpMethod.GET "/" None).Length |> int64)

      testCase "gzip static file" <| fun _ ->
        Assert.Equal("length should match"
        , testFileSize
        , (runWithConfig (Files.browseFileHome "test-text-file.txt") |> reqGZipBytes HttpMethod.GET "/" None).Length |> int64)
    ]

[<Tests>]
let ``http HEAD method`` =
  let runWithConfig = runWith defaultConfig

  testList "HEAD on `file`" [

    testCase  "HEAD does not return content" <| fun _ ->
      
      let ctx = runWithConfig (Files.browseFileHome "test-text-file.txt") 

      withContext (fun _ ->
        let client = new TcpClient("127.0.0.1",8083)
        let message = sprintf "HEAD %s HTTP/1.1\r\nHost: %s\r\nConnection: Close\r\n\r\n" "/foo" "127.0.0.1"
        let outputData = ASCII.bytes message
        let stream = client.GetStream()
        stream.Write(outputData, 0, outputData.Length)

        use streamReader = new StreamReader(stream)
    
        // read header lines
        let rec loop _ =
          let line = streamReader.ReadLine()
          if line.Equals("") then ()
          else loop ()
        loop ()

        Assert.Equal("Stream should be at the end.", true, streamReader.EndOfStream)) ctx
      
  ]
