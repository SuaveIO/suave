module Suave.Tests.Parsing

open Fuchu

open System
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text

open Suave
open Suave.Http
open Suave.AsyncOption.Operators
open Suave.Http.Applicatives
open Suave.Http.RequestErrors
open Suave.Http.Successful
open Suave.Utils
open Suave.Web

open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let parsingMultipart cfg =
  let runWithConfig = runWith cfg

  let postData1 = readBytes "request.txt"
  let postData2 = readText "request-1.txt"
  let postData3 = readText "request-2.txt"

  let testUrlEncodedForm fieldName =
    request (fun r ->
      match r.formData fieldName  with
      | Choice1Of2 str -> OK str
      | Choice2Of2 _ -> OK "field-does-not-exists")

  let testMultipartForm =
    request (fun r ->
      match getFirst r.multiPartFields "From" with
      | Choice1Of2 str -> OK str
      | Choice2Of2 _ -> OK "field-does-not-exists")

  let byteArrayContent = new ByteArrayContent(postData1)
  byteArrayContent.Headers.TryAddWithoutValidation("Content-Type","multipart/form-data; boundary=99233d57-854a-4b17-905b-ae37970e8a39") |> ignore

  testList "http parser tests" [
      testCase "parsing a large multipart form" <| fun _ ->
        Assert.Equal("", "Bob <bob@wishfulcoding.mailgun.org>", runWithConfig testMultipartForm |> req HttpMethod.POST "/" (Some byteArrayContent))

      testCase "parsing a large urlencoded form data" <| fun _ ->
        Assert.Equal("", "hallo wereld", 
          runWithConfig (testUrlEncodedForm "stripped-text") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(postData2, Encoding.UTF8, "application/x-www-form-urlencoded")))

      testCase "parsing a large urlencoded form data" <| fun _ ->
        Assert.Equal("", "Pepijn de Vos <pepijndevos@gmail.com>", 
          runWithConfig (testUrlEncodedForm "from") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(postData3, Encoding.UTF8, "application/x-www-form-urlencoded")))

      testCase "parsing a large urlencoded form data" <| fun _ ->
        Assert.Equal("", "no attachment 2",
          runWithConfig (testUrlEncodedForm "subject") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(postData3, Encoding.UTF8, "application/x-www-form-urlencoded")))

      testCase "parsing a large urlencoded form data" <| fun _ ->
        Assert.Equal("", "identifier 123abc", 
          runWithConfig (testUrlEncodedForm "body-plain") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(postData3, Encoding.UTF8, "application/x-www-form-urlencoded")))

      testCase "parsing a large urlencoded form data" <| fun _ ->
        Assert.Equal("", "field-does-not-exists", 
          runWithConfig (testUrlEncodedForm "body-html") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(postData3, Encoding.UTF8, "application/x-www-form-urlencoded")))
  ]

open System.Net
open System.Net.Sockets
open Suave.Logging
open Suave.Sockets

[<Tests>]
let parsingMultipart2 cfg =
  let ip, port =
    let binding = SuaveConfig.firstBinding cfg
    binding.socketBinding.ip,
    int binding.socketBinding.port

  let app =
    choose
      [ POST
        >=> choose [
            path "/filecount" >=> warbler (fun ctx ->
              OK (string ctx.request.files.Length))

            path "/filenames"
              >=> Writers.setMimeType "application/json"
              >=> warbler (fun ctx ->
                  //printfn "inside suave"
                  ctx.request.files
                  |> List.map (fun f ->
                    "\"" + f.fileName + "\"")
                  |> String.concat ","
                  |> fun files -> "[" + files + "]"
                  |> OK)

            path "/msgid"
              >=> request (fun r ->
                match r.multiPartFields |> List.tryFind (fst >> (=) "messageId") with
                | Some (_, yep) -> OK yep
                | None -> NOT_FOUND "Nope... Not found"
              )

            NOT_FOUND "Nope."
        ]
      ]

  let runWithConfig = runWith cfg //{ cfg with logger = Loggers.ConsoleWindowLogger(LogLevel.Verbose) }

  let sendRecvRaw (data : byte []) =
    use sender = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
    sender.Connect (new IPEndPoint(ip, port))
    let written = sender.Send data
    Assert.Equal("same written as given", data.Length, written)

    let respBuf = Array.zeroCreate<byte> 0x100
    let resp = sender.Receive respBuf
    ASCII.toStringAtOffset respBuf 0 resp

  let sendRecv (data : byte []) =
    let client = new TcpClient(string ip, port)
    use stream = client.GetStream()
    stream.Write(data, 0, data.Length)

    use streamReader = new StreamReader(stream)
    streamReader.ReadToEnd()

  testList "sending funky multiparts" [
    testCase "sending two files under same form name" <| fun _ ->
      let ctx = runWithConfig app
      try
        let data = readBytes "request-multipartmixed-twofiles.txt"
        let subject = sendRecv data
        Assert.StringContains("Expecting 200 OK", "HTTP/1.1 200 OK", subject)
        Assert.StringContains(
          "Expecting response to contain file name",
          "file1.txt",
          subject)
      finally
        disposeContext ctx

    testCase "extracting messageId from form-data post" <| fun _ ->
      let ctx = runWithConfig app
      try
        let data = readBytes "request-binary-n-formdata.txt"
        let subject = sendRecv data
        Assert.StringContains("Expecting 200 OK", "HTTP/1.1 200 OK", subject)
        Assert.StringContains("Expecting response to contain messageid", "online sha1 hash of all files", subject)
      finally
        disposeContext ctx

    testCase "no host header" <| fun _ ->
      let ctx = runWithConfig app
      try
        let data = readBytes "request-no-host-header.txt"
        let subject = sendRecvRaw data
        Assert.StringContains("Expecting 400 Bad Request", "HTTP/1.1 400 Bad Request", subject)
      finally
        disposeContext ctx

    testCase "bug 256" <| fun _ ->
      let ctx = runWithConfig app
      try
        let data = readBytes "request-hangs.txt"
        let subject = sendRecvRaw data
        Assert.StringContains("Expecting 404 Not Found", "HTTP/1.1 404 Not Found", subject)
      finally
        disposeContext ctx
    ]