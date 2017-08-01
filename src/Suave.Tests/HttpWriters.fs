module Suave.Tests.HttpWriters

open Expecto

open System
open System.IO
open System.Linq
open System.Net.Sockets

open Suave
open Suave.Operators
open Suave.Successful
open Suave.Writers
open Suave.Utils

open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let cookies cfg =
  let runWithConfig = runWith cfg

  let basicCookie =
    { name     = "mycookie"
      value    = "42"
      expires  = None
      domain   = None
      path     = Some "/"
      httpOnly = false
      secure   = false
      sameSite = None }

  let ip, port =
    let binding = SuaveConfig.firstBinding cfg
    string binding.socketBinding.ip,
    int binding.socketBinding.port

  testList "Cookies basic tests" [
    testCase "cookie data makes round trip" <| fun _ ->
      Assert.Equal("expecting cookie value"
      , "42"
      , (reqCookies HttpMethod.GET "/" None
        (runWithConfig (Cookie.setCookie basicCookie >=> OK "test")))
          .GetCookies(Uri(sprintf "http://%s" ip)).[0].Value)

    testCase "cookie name makes round trip" <| fun _ ->
      Assert.Equal("expecting cookie name"
      , "mycookie"
      , (reqCookies HttpMethod.GET "/" None
          (runWithConfig (Cookie.setCookie basicCookie >=> OK "test")))
          .GetCookies(Uri(sprintf "http://%s" ip)).[0].Name)

    testCase "http_only cookie is http_only" <| fun _ ->
      Assert.Equal("expecting http_only"
      , true
      , (reqCookies HttpMethod.GET "/" None
        (runWithConfig (Cookie.setCookie { basicCookie with httpOnly = true } >=> OK "test")))
          .GetCookies(Uri(sprintf "http://%s" ip)).[0].HttpOnly)
  ]

[<Tests>]
let headers cfg =
  let runWithConfig = runWith cfg

  let requestHeaders () =
    let ip, port =
      let binding = SuaveConfig.firstBinding cfg
      string binding.socketBinding.ip,
      int binding.socketBinding.port

    use client = new TcpClient(ip, port)
    let outputData = ASCII.bytes (sprintf "GET / HTTP/1.1\r\nHost: %s\r\nConnection: Close\r\n\r\n" ip)
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

  let getRespHeaders key =
    List.filter (fst >> (String.equalsCaseInsensitive key))

  let getRespHeader key =
    getRespHeaders key >> List.head

  testList "addHeader,setHeader,setHeaderValue tests" [
    testCase "setHeader adds header if it was not there" <| fun _ ->
      let ctx = runWithConfig (Writers.setHeader "X-Custom-Header" "value" >=> OK "test")

      withContext (fun _ ->
        let hdrs = requestHeaders ()
        Assert.Equal(
          "expecting header value",
          [ "X-Custom-Header", "value" ],
          hdrs |> getRespHeaders "X-Custom-Header"))
        ctx

    testCase "setHeader rewrites all instances of header with new single value" <| fun _ ->
      let ctx =
        runWithConfig
          (Writers.setHeader "X-Custom-Header" "first"
           >=> Writers.setHeader "X-Custom-Header" "second"
           >=> Writers.setHeader "x-custom-header" "third"
           >=> OK "test")

      withContext (fun _ ->
        let hdrs = requestHeaders ()
        Assert.Equal(
          "expecting header value",
          "third",
          hdrs |> getRespHeader "X-Custom-Header" |> snd))
        ctx

    testCase "addHeader adds header and preserves the order" <| fun _ ->
      let ctx =
        runWithConfig
          (Writers.addHeader "X-Custom-Header" "first"
           >=> Writers.addHeader "X-Custom-Header" "second"
           >=> OK "test")

      withContext (fun _ ->
        let hdrs = requestHeaders ()
        Assert.Equal(
          "expecting headers value",
          [ "X-Custom-Header", "first"
            "X-Custom-Header", "second"],
          hdrs |> getRespHeaders "X-Custom-Header"))
        ctx

    testCase "setHeaderValue sets the first by-key found header's value so it includes the value" <| fun _ ->
      let ctx =
        runWithConfig
          (Writers.addHeader "Vary" "Accept-Encoding"
           // e.g. in Suave.Locale:
           >=> Writers.setHeaderValue "Vary" "Accept-Language"
           // later, e.g. in Logibit.Hawk, since this turned out to be authenticated
           // content:
           >=> Writers.setHeaderValue "Vary" "Authorization"
           // note on the above:
           // with Hawk it will turn out to be a cache-busting mechanism since
           // the Authorization header includes a nonce and a timestamp
           // but it's the semantically correct interpretation.
           // Meanwhile, the Cookie header gets changed as the cookie ages and
           // expires.
           >=> Writers.setHeaderValue "Vary" "Cookie"
           // Note: it's up to the client to use optimistic concurrency control
           // on its side for data requested under Hawk authorization
           >=> OK "test")

      withContext (fun _ ->
        let hdrs = requestHeaders ()
        Assert.Equal(
          "expecting headers value",
          ["Vary", "Accept-Encoding,Accept-Language,Authorization,Cookie"],
          hdrs |> getRespHeaders "Vary"))
        ctx

    testCase "setHeaderValue only modifies ONE of the found headers; the first one" <| fun _ ->
      let ctx =
        runWithConfig
          (Writers.addHeader "Vary" "Accept-Encoding"
           >=> Writers.addHeader "vary" "Accept-Language"
           >=> Writers.setHeaderValue "Vary" "Authorization"
           >=> Writers.setHeaderValue "vary" "Cookie"
           >=> OK "test")

      withContext (fun _ ->
        let hdrs = requestHeaders ()
        Assert.Equal(
          "expecting headers value",
          [ "vary", "Accept-Encoding,Authorization,Cookie"
            "vary", "Accept-Language"
          ],
          hdrs |> getRespHeaders "Vary"))
        ctx
  ]
