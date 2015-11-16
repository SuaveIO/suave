module Suave.Tests.HttpWriters

open Fuchu

open System
open System.IO
open System.Linq
open System.Net.Sockets

open Suave
open Suave.Http
open Suave.Http.Successful
open Suave.Http.Writers
open Suave.Types
open Suave.Utils

open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let cookies cfg =
  let runWithConfig = runWith cfg

  let basicCookie =
    { name      = "mycookie"
      value     = "42"
      expires   = None
      domain    = None
      path      = Some "/"
      httpOnly = false
      secure    = false }

  testList "Cookies basic tests" [
    testCase "cookie data makes round trip" <| fun _ ->
      Assert.Equal("expecting cookie value"
      , "42"
      , (reqCookies HttpMethod.GET "/" None
        (runWithConfig (Cookie.setCookie basicCookie >>= OK "test")))
          .GetCookies(Uri("http://127.0.0.1")).[0].Value)

    testCase "cookie name makes round trip" <| fun _ ->
      Assert.Equal("expecting cookie name"
      , "mycookie"
      , (reqCookies HttpMethod.GET "/" None
          (runWithConfig (Cookie.setCookie basicCookie >>= OK "test")))
          .GetCookies(Uri("http://127.0.0.1")).[0].Name)

    testCase "http_only cookie is http_only" <| fun _ ->
      Assert.Equal("expecting http_only"
      , true
      , (reqCookies HttpMethod.GET "/" None
        (runWithConfig (Cookie.setCookie { basicCookie with httpOnly = true } >>= OK "test")))
          .GetCookies(Uri("http://127.0.0.1")).[0].HttpOnly)
  ]

[<Tests>]
let headers cfg =
  let runWithConfig = runWith cfg

  let requestHeaders () =
    use client = new TcpClient("127.0.0.1",8083)
    let outputData = ASCII.bytes "GET / HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: Close\r\n\r\n"
    use stream = client.GetStream()
    stream.Write(outputData, 0, outputData.Length)

    use streamReader = new StreamReader(stream)
    
    let splitHeader (line: string) =
      let ind = line.IndexOf(':')
      let name = line.Substring(0, ind)
      let value = line.Substring(ind + 1)
      name.Trim(), value.Trim()

    // skip 200 OK
    streamReader.ReadLine() |> ignore

    // read header lines
    let rec loop hdrs =
      let line = streamReader.ReadLine()
      if line.Equals("") then (List.rev hdrs)
      else
        let name, value = splitHeader line
        loop ((name, value) :: hdrs)
    loop []

  testList "Headers basic tests" [
    testCase "setHeader adds header if it was not there" <| fun _ ->
      let ctx = runWithConfig (Writers.setHeader "X-Custom-Header" "value" >>= OK "test")
      withContext (fun _ ->
        let hdrs = requestHeaders ()
        Assert.Equal("expecting header value"
          , ["X-Custom-Header", "value"]
          , hdrs |> List.filter (fun (n,_) -> n = "X-Custom-Header"))) ctx

    testCase "setHeader rewrites all instances of header with new single value" <| fun _ ->
      let ctx = runWithConfig (
                  Writers.setHeader "X-Custom-Header" "first"
                  >>= Writers.setHeader "X-Custom-Header" "second"
                  >>= Writers.setHeader "X-Custom-Header" "third"
                  >>= OK "test")
      withContext (fun _ ->
        let hdrs = requestHeaders ()
        Assert.Equal("expecting header value"
          , ["X-Custom-Header", "third"]
          , hdrs |> List.filter (fun (n,_) -> n = "X-Custom-Header"))) ctx

    testCase "addHeader adds header and preserve order" <| fun _ ->
      let ctx = runWithConfig (
                  Writers.addHeader "X-Custom-Header" "first"
                  >>= Writers.addHeader "X-Custom-Header" "second"
                  >>= OK "test")
      withContext (fun _ ->
        let hdrs = requestHeaders ()
        Assert.Equal("expecting headers value"
          , ["X-Custom-Header", "first"; "X-Custom-Header", "second"]
          , hdrs |> List.filter (fun (n,_) -> n = "X-Custom-Header"))) ctx
  ]
