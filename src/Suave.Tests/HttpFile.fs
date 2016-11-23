module Suave.Tests.HttpFile

open Expecto

open System
open System.IO
open System.Text
open System.Net.Sockets

open Suave
open Suave.Successful
open Suave.Utils

open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let ``canonicalization attacks`` (_: SuaveConfig) =
  testList "canonicalization attacks" [
    testCase "should throw" <| fun _ ->
      Expect.throwsT<Exception> (fun _ -> Files.resolvePath currentPath "../../passwd" |> ignore)
        "'../../passwd' is not a valid path"
    testCase "can use dot" <| fun _ ->
      Expect.equal (Files.resolvePath currentPath ".") currentPath "expect currentPath"
  ]

[<Tests>]
let compression cfg =
  let runWithConfig = runWith cfg

  let testFileSize = (new FileInfo(Path.Combine(currentPath,"test-text-file.txt"))).Length

  testList "getting basic gzip/deflate responses" [
      testCase "200 OK returns 'Havana' with gzip " <| fun _ ->
        let actual = runWithConfig (OK "Havana") |> reqGZip HttpMethod.GET "/" None
        let expected = "Havana"
        Expect.equal actual expected "expecting 'Havana'"

      testCase "200 OK returns 'Havana' with deflate " <| fun _ ->
        let actual = runWithConfig (OK "Havana") |> reqDeflate HttpMethod.GET "/" None
        Expect.equal actual "Havana" "expecting 'Havana'"

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
let ``http HEAD method`` cfg =
  let runWithConfig = runWith cfg
  let ip, port =
    let binding = SuaveConfig.firstBinding cfg
    string binding.socketBinding.ip,
    int binding.socketBinding.port

  testList "HEAD on `file`" [
    testCase  "HEAD does not return content" <| fun _ ->

      let ctx = runWithConfig (Files.browseFileHome "test-text-file.txt")

      withContext (fun _ ->
        let client = new TcpClient(ip, port)
        let message = sprintf "HEAD %s HTTP/1.1\r\nHost: %s\r\nConnection: Close\r\n\r\n" "/foo" ip
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

        Expect.equal streamReader.EndOfStream true "Stream should be at the end.") ctx
  ]
