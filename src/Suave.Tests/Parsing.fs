module Suave.Tests.Parsing

open Expecto
open System
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open Suave
open Suave.Operators
open Suave.Filters
open Suave.RequestErrors
open Suave.Successful
open Suave.Utils
open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let parseQuery =
  testList "http parser tests" [
    testCase "can parse query with =" <| fun _ ->
      let subject =
        Parsing.parseData "bewit=c&d=Q%3D%3D&r=https%3A%2F%2Fqvitoo.dev%3A8080%2F"
      Expect.equal subject.Length 3 "Should have three values"

      let actual = subject.[1] |> snd |> Option.get
      Expect.equal actual "Q==" "Should contain Q=="

    testCase "can parse empty query" <| fun _ ->
      let subject =
        Parsing.parseData ""
      Expect.equal subject.Length 0 "Should be empty list"
    ]

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
      let actual = runWithConfig testMultipartForm |> req HttpMethod.POST "/" (Some byteArrayContent)
      Expect.equal actual "Bob <bob@wishfulcoding.mailgun.org>" "Should return correct value"

    testCase "parsing a large urlencoded form data - stripped-text" <| fun _ ->
      Assert.Equal("", "hallo wereld",
        runWithConfig (testUrlEncodedForm "stripped-text") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(postData2, Encoding.UTF8, "application/x-www-form-urlencoded")))

    testCase "parsing a large urlencoded form data - from" <| fun _ ->
      Assert.Equal("", "Pepijn de Vos <pepijndevos@gmail.com>",
        runWithConfig (testUrlEncodedForm "from") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(postData3, Encoding.UTF8, "application/x-www-form-urlencoded")))

    testCase "parsing a large urlencoded form data - subject" <| fun _ ->
      Assert.Equal("", "no attachment 2",
        runWithConfig (testUrlEncodedForm "subject") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(postData3, Encoding.UTF8, "application/x-www-form-urlencoded")))

    testCase "parsing a large urlencoded form data - body-plain" <| fun _ ->
      Assert.Equal("", "identifier 123abc",
        runWithConfig (testUrlEncodedForm "body-plain") |> reqGZip HttpMethod.POST "/" (Some <| new StringContent(postData3, Encoding.UTF8, "application/x-www-form-urlencoded")))

    testCase "parsing a large urlencoded form data - body-html" <| fun _ ->
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
    Expect.equal written data.Length "same written as given"

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
        Expect.stringContains subject "HTTP/1.1 200 OK" "Expecting 200 OK"
        Expect.stringContains subject "file1.txt" "Expecting response to contain file name"
      finally
        disposeContext ctx

    testCase "extracting messageId from form-data post" <| fun _ ->
      let ctx = runWithConfig app
      try
        let data = readBytes "request-binary-n-formdata.txt"
        let subject = sendRecv data
        Expect.stringContains subject "HTTP/1.1 200 OK" "Expecting 200 OK"
        Expect.stringContains subject "online sha1 hash of all files" "Expecting response to contain messageid"
      finally
        disposeContext ctx

    testCase "no host header" <| fun _ ->
      let ctx = runWithConfig app
      try
        let data = readBytes "request-no-host-header.txt"
        let subject = sendRecvRaw data
        Expect.stringContains subject "HTTP/1.1 400 Bad Request" "Expecting 400 Bad Request"
      finally
        disposeContext ctx

    testCase "bug 256" <| fun _ ->
      let ctx = runWithConfig app
      try
        let data = readBytes "request-hangs.txt"
        let subject = sendRecvRaw data
        Expect.stringContains subject "HTTP/1.1 404 Not Found" "Expecting 404 Not Found"
      finally
        disposeContext ctx
    ]

[<Tests>]
let testLineBuffer cfg =
  let longUri = String.replicate (cfg.bufferSize + 100) "A" 
  let runWithConfig = runWith cfg

  testList "test line buffer" [

    testCase "GET uri larger than line buffer length" <| fun _ ->
      let actual = runWithConfig (OK "response") |> req HttpMethod.GET longUri None
      Expect.equal actual "Line Too Long" "expecting data to be returned"

    ]
