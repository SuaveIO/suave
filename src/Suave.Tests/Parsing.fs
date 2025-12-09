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
open Suave.Utils.Parsing

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

    testCase "can parse equal signs in query" <| fun _ ->

      let subject =
        Parsing.parseData "q=a=b"
      Expect.equal subject.Length 1 "Should have one value"
      let actual = subject.[0] |> snd |> Option.get
      Expect.equal actual "a=b" "Should contain a=b"

    testCase "can parse query with missing values" <| fun _ ->
      let subject =
        Parsing.parseData "a=1&b=&c=3&d"
      Expect.equal subject.Length 4 "Should have four values"

      let actualB = subject.[1] |> snd |> Option.get
      Expect.equal actualB "" "b should be empty string"

      let actualD = subject.[3] |> snd
      Expect.equal actualD (Some("")) "d should be Some(\"\")"

    testCase "can parse query with multiple =" <| fun _ ->
      let subject =
        Parsing.parseData "a==1==&b===2==="
      Expect.equal subject.Length 2 "Should have two values"
      let actualA = subject.[0] |> snd |> Option.get
      Expect.equal actualA "=1==" "a should be '=1=='"
      let actualB = subject.[1] |> snd |> Option.get
      Expect.equal actualB "==2===" "b should be '==2==='"

    testCase "can parse query with empty values only" <| fun _ ->
      let subject =
        Parsing.parseData "a=&b=&c="
      Expect.equal subject.Length 3 "Should have three values"
      let actualA = subject.[0] |> snd |> Option.get
      Expect.equal actualA "" "a should be empty string"
      let actualB = subject.[1] |> snd |> Option.get
      Expect.equal actualB "" "b should be empty string"
      let actualC = subject.[2] |> snd |> Option.get
      Expect.equal actualC "" "c should be empty string"

    testCase "can parse query with no keys only values" <| fun _ ->
      let subject =
        Parsing.parseData "=1&=2&=3"
      Expect.equal subject.Length 3 "Should have three values"
      let actual1 = subject.[0] |> snd |> Option.get
      Expect.equal actual1 "1" "first value should be '1'"
      let actual2 = subject.[1] |> snd |> Option.get
      Expect.equal actual2 "2" "second value should be '2'"
      let actual3 = subject.[2] |> snd |> Option.get
      Expect.equal actual3 "3" "third value should be '3'"

    testCase "can parse query with only keys no values" <| fun _ ->
      let subject =
        Parsing.parseData "a&b&c"
      Expect.equal subject.Length 3 "Should have three values"
      let actualA = subject.[0] |> snd |> Option.get
      Expect.equal actualA "" "a should be empty string"
      let actualB = subject.[1] |> snd |> Option.get
      Expect.equal actualB "" "b should be empty string"
      let actualC = subject.[2] |> snd |> Option.get
      Expect.equal actualC "" "c should be empty string"  

    testCase "can parse query with mixed cases" <| fun _ ->
      let subject =
        Parsing.parseData "a=1&b&c=&=4&=&&d==5=="
      Expect.equal subject.Length 6 "Should have six values"
      let actualA = subject.[0] |> snd |> Option.get
      Expect.equal actualA "1" "a should be '1'"
      let actualB = subject.[1] |> snd |> Option.get
      Expect.equal actualB "" "b should be empty string"
      let actualC = subject.[2] |> snd |> Option.get
      Expect.equal actualC "" "c should be empty string"
      let actualFirstEmptyKey = subject.[3] |> snd |> Option.get
      Expect.equal actualFirstEmptyKey "4" "first empty key should be '4'"
      let actualSecondEmptyKey = subject.[4] |> snd |> Option.get
      Expect.equal actualSecondEmptyKey "" "second empty key should be empty string"
      let actualD = subject.[5] |> snd |> Option.get
      Expect.equal actualD "=5==" "d should be '=5=='"

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
              OK (string ctx.request.files.Count))

            path "/filenames"
              >=> Writers.setMimeType "application/json"
              >=> warbler (fun ctx ->
                  //printfn "inside suave"
                  ctx.request.files
                  |> Seq.map (fun f ->
                    "\"" + f.fileName + "\"")
                  |> String.concat ","
                  |> fun files -> "[" + files + "]"
                  |> OK)

            path "/msgid"
              >=> request (fun r ->
                match r.multiPartFields |> Seq.tryFind (fst >> (=) "messageId") with
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

[<Tests>]
let testParseBoundary =

  let compare tuple =
    match tuple with
    | (contentType, expected) ->
      Expect.equal (parseBoundary contentType) expected "Parsed boundary does not match expected value"

  testList
    "test multipart boundary parsing"
    [ testCase "Matching boundaries"
      <| fun _ ->
           [ ("multipart/form-data; boundary=\"abc123 / + _,_.():=? as\"",
              "abc123 / + _,_.():=? as")
             ("multipart/form-data; boundary=/tkQKiFqMgZt:mHzua_JFrUFWHgNid",
              "/tkQKiFqMgZt:mHzua_JFrUFWHgNid")
             ("multipart/mixed; boundary=-idczlATz:FmvuIs'aHQSrGltky:Td",
              "-idczlATz:FmvuIs'aHQSrGltky:Td")
             ("multipart/form-data; boundary=99233d57-854a-4b17-905b-ae37970e8a39",
              "99233d57-854a-4b17-905b-ae37970e8a39")
             ("multipart/form-data; boundary=---------------------------19533183328386942351998832384",
              "---------------------------19533183328386942351998832384") ]
           |> List.map compare
           |> ignore

      testCase "Matching boundaries with charset defined"
      <| fun _ ->
           [ ("multipart/form-data; charset=utf8; boundary=\"abc123 / + _,_.():=? as\"",
              "abc123 / + _,_.():=? as")
             ("multipart/form-mixed; charset=utf8; boundary=/tkQKiFqMgZt:mHzua_JFrUFWHgNid",
              "/tkQKiFqMgZt:mHzua_JFrUFWHgNid")]
           |> List.map compare
           |> ignore

      testCase "Boundaries with spaces at the end"
      <| fun _ ->
           [ ("multipart/form-data; boundary=/tkQKiFqMgZt:mHzua_JFrUFWHgNi d  ",
              "/tkQKiFqMgZt:mHzua_JFrUFWHgNi d")
             ("multipart/mixed; boundary=\"------------020601 070403020003080 006 \"",
              "------------020601 070403020003080 006") ]
           |> List.map compare
           |> ignore

      testCase "Not matching boundaries"
      <| fun _ ->
           [
             ("multipart/form-data; boundary=unicorn🦄", "unicorn")
             ("multipart/form-data; boundary=%rfeo@", "")
             ("multipart/form-data; boundary=", "")]
           |> List.map compare
           |> ignore
      ]

