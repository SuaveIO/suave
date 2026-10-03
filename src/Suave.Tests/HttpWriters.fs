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
let errorWrapper (_ : SuaveConfig) =
  let createOutput errorHandler =
    let runtime = { HttpRuntime.empty with errorHandler = errorHandler }
    HttpOutput(Unchecked.defaultof<_>, runtime), runtime
  let run workflow = Async.RunSynchronously(workflow, timeout = 5000)

  testList "HttpOutput error wrapper" [
    testCase "execution stays lazy and repeatable for handled and unhandled results" <| fun _ ->
      let mutable executions = 0
      let mutable errors = 0
      let output, _ = createOutput (fun _ _ _ -> errors <- errors + 1; async.Return None)
      let wrapped = output.executeTask (async {
        executions <- executions + 1
        return Some HttpContext.empty
      })
      Expect.equal executions 0 "Constructing the wrapper must not execute the workflow"
      for iteration in 1 .. 2 do
        Expect.isSome (run wrapped) "Preserve handled results"
        Expect.equal executions iteration "Each execution runs the workflow again"
      Expect.isNone (run (output.executeTask (async.Return None))) "Preserve unhandled results"
      Expect.equal errors 0 "Successful workflows must not invoke error handling"

    testCase "sync and delayed faults preserve handler arguments and recovery" <| fun _ ->
      for original in [ InvalidOperationException("fault") :> exn; OperationCanceledException("raised exception") :> exn ] do
        for delayed in [ false; true ] do
          let mutable calls = 0
          let mutable capturedContext = None
          let output, runtime = createOutput (fun error message context ->
            calls <- calls + 1
            Expect.isTrue (Object.ReferenceEquals(error, original)) "Pass the original exception"
            Expect.equal message "request failed" "Preserve the diagnostic message"
            capturedContext <- Some context
            async {
              do! Async.Sleep 1
              return Some context
            })
          let result =
            output.executeTask (async {
              if delayed then do! Async.Sleep 1
              return raise original
            }) |> run
          Expect.isSome result "Await and return the handler's recovery workflow"
          Expect.equal calls 1 "Invoke the error handler exactly once"
          let context = capturedContext |> Option.get
          Expect.isTrue (Object.ReferenceEquals(context.runtime, runtime)) "Use the configured runtime"
          Expect.isTrue (Object.ReferenceEquals(context.connection, output.Connection)) "Use the current connection"
          Expect.equal context.request.rawPath HttpContext.empty.request.rawPath "Keep the existing empty error context"

    testCase "faults in the error handler propagate without recursive handling" <| fun _ ->
      for delayed in [ false; true ] do
        let mutable calls = 0
        let handlerError = InvalidOperationException("handler fault")
        let output, _ = createOutput (fun _ _ _ ->
          calls <- calls + 1
          if delayed then async {
            do! Async.Sleep 1
            return raise handlerError
          }
          else raise handlerError)
        let caught =
          try
            output.executeTask (async { return raise (Exception "workflow fault") }) |> run |> ignore
            None
          with error -> Some error
        Expect.isTrue (caught |> Option.exists (fun error -> Object.ReferenceEquals(error, handlerError))) "Propagate the handler exception"
        Expect.equal calls 1 "Do not recursively invoke the handler"

    testCase "cancellation continuations bypass the error handler" <| fun _ ->
      let mutable calls = 0
      let output, _ = createOutput (fun _ _ _ -> calls <- calls + 1; async.Return None)
      let workflow = Async.FromContinuations(fun (_, _, cancel) -> cancel (OperationCanceledException "cancel"))
      Expect.throwsT<OperationCanceledException> (fun () -> output.executeTask workflow |> run |> ignore) "Preserve cancellation"
      Expect.equal calls 0 "Cancellation must not become an error response"

    testCase "ambient cancellation is preserved before and during execution" <| fun _ ->
      for preCanceled in [ false; true ] do
        use cancellation = new System.Threading.CancellationTokenSource()
        let mutable calls = 0
        let mutable started = false
        let output, _ = createOutput (fun _ _ _ -> calls <- calls + 1; async.Return None)
        if preCanceled then cancellation.Cancel()
        let pending = Async.StartImmediateAsTask(output.executeTask (async {
          started <- true
          do! Async.Sleep 10000
          return Some HttpContext.empty
        }), cancellationToken = cancellation.Token)
        Expect.equal started (not preCanceled) "Respect cancellation before entering the workflow"
        cancellation.Cancel()
        Expect.throwsT<System.Threading.Tasks.TaskCanceledException>
          (fun () -> pending.WaitAsync(TimeSpan.FromSeconds 5.).GetAwaiter().GetResult() |> ignore)
          "Cancel without waiting for the sleep to finish"
        Expect.isTrue pending.IsCanceled "Keep the Task canceled rather than faulted"
        Expect.equal calls 0 "Ambient cancellation bypasses the handler"

    testCase "execution context is captured at execution rather than construction" <| fun _ ->
      let ambient = System.Threading.AsyncLocal<string>()
      let output, _ = createOutput (fun error _ _ -> async { return raise error })
      ambient.Value <- "construction"
      let wrapped = output.executeTask (async {
        do! Async.Sleep 1
        Expect.equal ambient.Value "execution" "Flow the caller's execution context through suspension"
        return None
      })
      ambient.Value <- "execution"
      Expect.isNone (run wrapped) "The workflow completes normally"
  ]

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

    testCase "setHeader adds Server header with hideHeader = true" <| fun _ ->
      let ctx = runWith { cfg with hideHeader = true } (Writers.setHeader "Server" "My custom value" >=> OK "test")

      withContext (fun _ ->
        let hdrs = requestHeaders ()
        Assert.Equal(
          "expecting Server header value",
          [ "Server", "My custom value" ],
          hdrs |> getRespHeaders "Server"))
        ctx
  ]