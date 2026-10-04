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

[<Tests>]
let filePartSinkTests cfg =
  /// Build a minimal multipart/form-data body as a byte array.
  let buildMultipartBody (boundary: string) fileName mimeType (fileContent: byte[]) =
    let crlf = "\r\n"
    let header =
      sprintf "--%s%sContent-Disposition: form-data; name=\"file\"; filename=\"%s\"%sContent-Type: %s%s%s"
        boundary crlf fileName crlf mimeType crlf crlf
    let footer = sprintf "%s--%s--%s" crlf boundary crlf
    Array.concat
      [ Encoding.ASCII.GetBytes header
        fileContent
        Encoding.ASCII.GetBytes footer ]

  let makeMultipartContent boundary body =
    let content = new ByteArrayContent(body)
    content.Headers.TryAddWithoutValidation(
      "Content-Type", sprintf "multipart/form-data; boundary=%s" boundary) |> ignore
    content

  let webpart =
    request (fun r ->
      if r.files.Count = 1 then
        let f = r.files.[0]
        OK (sprintf "%s|%s|%s" f.fieldName f.fileName f.mimeType)
      else
        RequestErrors.BAD_REQUEST "unexpected file count")

  testList "file-part sink" [

    testCase "custom sink receives file bytes and onSuccess produces HttpUpload" <| fun _ ->
      let boundary     = "test-boundary-123"
      let fileName     = "hello.bin"
      let mimeType     = "application/octet-stream"
      let fileContent  = [| 10uy; 20uy; 30uy; 40uy; 50uy |]

      let mutable capturedBytes : byte[] = [||]
      let mutable successCalled = false
      let mutable errorCalled   = false

      let sink : FilePartSink = fun fieldName fn mt ->
        let ms = new MemoryStream()
        { stream    = ms
          onSuccess = fun () ->
            capturedBytes <- ms.ToArray()
            successCalled <- true
            { fieldName = fieldName; fileName = fn; mimeType = mt; tempFilePath = "" }
          onError   = fun () ->
            errorCalled <- true }

      let sinkConfig : SuaveConfig = { cfg with filePartSink = Some sink }
      let body = buildMultipartBody boundary fileName mimeType fileContent

      let actual = runWith sinkConfig webpart |> req HttpMethod.POST "/" (Some (makeMultipartContent boundary body))

      Expect.equal actual "file|hello.bin|application/octet-stream" "response should reflect upload metadata"
      Expect.isTrue  successCalled "onSuccess should have been called"
      Expect.isFalse errorCalled   "onError should not have been called"
      Expect.equal   capturedBytes fileContent "sink stream should contain the exact file bytes"

    testCase "custom sink onError is called for empty file part" <| fun _ ->
      let boundary    = "test-boundary-456"
      let fileName    = "empty.txt"
      let mimeType    = "text/plain"
      let fileContent = [||]   // zero bytes

      let mutable errorCalled   = false
      let mutable successCalled = false

      let sink : FilePartSink = fun _fieldName _fn _mt ->
        let ms = new MemoryStream()
        { stream    = ms
          onSuccess = fun () ->
            successCalled <- true
            { fieldName = ""; fileName = ""; mimeType = ""; tempFilePath = "" }
          onError   = fun () ->
            errorCalled <- true }

      let sinkConfig : SuaveConfig = { cfg with filePartSink = Some sink }
      let body = buildMultipartBody boundary fileName mimeType fileContent

      // Empty file → request returns no upload; server responds with the "unexpected file count" branch
      let _ = runWith sinkConfig webpart |> req HttpMethod.POST "/" (Some (makeMultipartContent boundary body))

      Expect.isTrue  errorCalled   "onError should have been called for empty part"
      Expect.isFalse successCalled "onSuccess should not have been called for empty part"
  ]


[<Tests>]
let requestHeadParsingTests cfg =
  // Requests whose head is fully buffered are parsed synchronously from the pipe;
  // fragmented, oversized and body-carrying requests hand over to the streaming
  // parser. These tests drive both paths over a raw keep-alive connection.
  let ip, port =
    let binding = SuaveConfig.firstBinding cfg
    binding.socketBinding.ip,
    int binding.socketBinding.port

  let headerOrDash name (r : HttpRequest) =
    match r.header name with
    | Choice1Of2 v -> v
    | Choice2Of2 _ -> "-"

  let app =
    choose [
      path "/body" >=> request (fun r -> OK ("body:" + Encoding.UTF8.GetString r.rawForm))
      path "/headers"
        >=> Writers.addHeader "content-type" "text/plain"
        >=> Writers.addHeader "X-Custom-Name" "custom value"
        >=> OK "headers"
      request (fun r ->
        OK (String.Join("|", [ r.rawMethod; r.path; r.rawQuery; headerOrDash "x-test" r; string r.headers.Count ])))
    ]

  let countOf (marker : string) (text : string) =
    let mutable count = 0
    let mutable index = text.IndexOf(marker, StringComparison.Ordinal)
    while index >= 0 do
      count <- count + 1
      index <- text.IndexOf(marker, index + marker.Length, StringComparison.Ordinal)
    count

  /// Send each chunk separately (with a pause so they arrive as separate reads)
  /// and collect the responses until `responses` complete ones have arrived.
  let exchange (chunks : string list) responses =
    use socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
    socket.NoDelay <- true
    socket.ReceiveTimeout <- 5000
    socket.Connect(IPEndPoint(ip, port))
    for chunk in chunks do
      socket.Send(Encoding.ASCII.GetBytes chunk) |> ignore
      Threading.Thread.Sleep 30
    let received = StringBuilder()
    let buffer = Array.zeroCreate<byte> 4096
    let complete () =
      // Every response in these tests has a short body following its head.
      let text = received.ToString()
      countOf "HTTP/1.1 " text >= responses
      && text.EndsWith("\r\n", StringComparison.Ordinal) |> not
      && countOf "\r\n\r\n" text >= responses
    let mutable closed = false
    while not (complete ()) && not closed do
      let n = socket.Receive buffer
      if n = 0 then closed <- true
      else received.Append(Encoding.ASCII.GetString(buffer, 0, n)) |> ignore
    received.ToString()

  let withServer f =
    let ctx = runWith cfg app
    try f () finally disposeContext ctx

  let get path headers =
    sprintf "GET %s HTTP/1.1\r\nHost: localhost\r\n%s\r\n" path headers

  testList "request head parsing" [
    testCase "pipelined requests in one packet are answered in order" <| fun _ ->
      withServer (fun () ->
        let response = exchange [ get "/first?a=1" "X-Test: one\r\n" + get "/second" "X-Test: two\r\n" ] 2
        Expect.equal (countOf "HTTP/1.1 200 OK" response) 2 "Both requests are answered"
        let first = response.IndexOf("GET|/first|a=1|one|2", StringComparison.Ordinal)
        let second = response.IndexOf("GET|/second||two|2", StringComparison.Ordinal)
        Expect.isGreaterThanOrEqual first 0 "First request parsed"
        Expect.isGreaterThan second first "Second request parsed after the first")

    testCase "a head split across reads is parsed once complete" <| fun _ ->
      withServer (fun () ->
        let response =
          exchange [ "GET /fr"; "agmented?q=2 HTTP/1.1\r\nHo"; "st: localhost\r\nX-Te"; "st: split\r\n"; "\r\n" ] 1
        Expect.stringContains response "GET|/fragmented|q=2|split|2" "Fragmented head parsed")

    testCase "header names are case-insensitive and values trimmed" <| fun _ ->
      withServer (fun () ->
        let response = exchange [ get "/trim" "x-TEST: \t padded value \t\r\nX-Other:\r\n" ] 1
        Expect.stringContains response "GET|/trim||padded value|3" "Trimmed value, empty header kept")

    testCase "a head larger than the line buffer is still parsed" <| fun _ ->
      withServer (fun () ->
        let filler = String.replicate 40 (sprintf "X-Filler: %s\r\n" (String.replicate 300 "f"))
        let response = exchange [ get "/large" (filler + "X-Test: big\r\n") + get "/after" "" ] 2
        Expect.stringContains response "GET|/large||big|42" "Large head parsed"
        Expect.stringContains response "GET|/after||-|1" "Following request parsed")

    testCase "a request body is read before the next pipelined request" <| fun _ ->
      withServer (fun () ->
        let post = "POST /body HTTP/1.1\r\nHost: localhost\r\nContent-Length: 5\r\n\r\nhello"
        let response = exchange [ post + get "/next" "X-Test: after-body\r\n" ] 2
        Expect.stringContains response "body:hello" "Body read"
        Expect.stringContains response "GET|/next||after-body|2" "Next request parsed")

    testCase "response header names keep canonical or given casing" <| fun _ ->
      withServer (fun () ->
        let response = exchange [ get "/headers" "" ] 1
        Expect.stringContains response "\r\nContent-Type: text/plain\r\n" "Known names use canonical casing"
        Expect.stringContains response "\r\nX-Custom-Name: custom value\r\n" "Other names are written as given")

    testCase "a malformed header is rejected with 400" <| fun _ ->
      withServer (fun () ->
        let response = exchange [ get "/bad" "no colon here\r\n" ] 1
        Expect.stringContains response "HTTP/1.1 400" "Malformed header rejected")
  ]
