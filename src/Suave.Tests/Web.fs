module Suave.Tests.Web

open Expecto

open Suave
open Suave.Operators
open Suave.Testing
open Suave.Sockets
open System
open System.Collections.Generic
open System.Net


[<Tests>]
let transferEncodingChunkedTests (cfg : SuaveConfig) =
  let writeChunks (conn:Connection) = task {

    do! conn.writeChunk "h"B
    do! conn.writeChunk "e"B
    do! conn.writeChunk "l"B
    do! conn.writeChunk "l"B
    do! conn.writeChunk "o"B
    do! conn.writeChunk " "B
    do! conn.writeChunk "w"B
    do! conn.writeChunk "o"B
    do! conn.writeChunk "r"B
    do! conn.writeChunk "l"B
    do! conn.writeChunk "d"B
    do! conn.writeChunk [||]
  }

  let webPart =
    Filters.path "/" >=> TransferEncoding.chunked writeChunks

  testList "when using chunked transfer encoding" [
    testCase "should encode data as chunked" <| fun _ ->
      let ctx = runWith cfg webPart
      let res = req HttpMethod.GET "/" None ctx
      Expect.equal res "hello world" "should return hello world"
  ]
  
[<Tests>]
let keepAliveTests (cfg : SuaveConfig) =
  let runWithConfig = runWith cfg

  let setUserState (state : string) : WebPart =
    context (fun ctx ->
      Writers.setUserData "state" state)

  let getUserState =
    context (fun ctx -> 
      match ctx.userState.TryGetValue "state"  with
      | true, value -> Successful.OK (value.ToString ())
      | false, _ -> Successful.OK "unexist")

  let webPart =
    choose [
      Filters.path "/set" >=> setUserState "exist" >=> getUserState
      Filters.path "/get" >=> getUserState
      ]

  let keepAliveUserDataLifetimeTest =
    testCase "userData Persists per Connection Instead of per Request" <| fun _ ->
      let ctx = runWithConfig webPart

      let reqResp client ctx path =
        let defaultTimeout = TimeSpan.FromSeconds 10.
        use request = createRequest HttpMethod.GET path "" None (endpointUri ctx.suaveConfig)
        request.Headers.ConnectionClose <- Nullable(false)
        use result = request |> send client defaultTimeout ctx
        contentString result

      withContext (fun ctx ->
        use handler = createHandler DecompressionMethods.None None
        use client = createClient handler
        let res = reqResp client ctx "set"
        Expect.equal res "exist" "should return exist"
        let res = reqResp client ctx "get"
        Expect.equal res "unexist" "should return unexist") ctx

  let genKeepAliveTest httpVersion connectionHeader shouldAddKeepAlive =
    let connectionHeaderDesc, reqHeaders =
      match connectionHeader with
      | Some v -> sprintf "a 'Connection: %s' header" v, List<_>([("connection", v)])
      | None -> "no Connection header", List<_>()
    testCase (sprintf "for an %s request with %s" httpVersion connectionHeaderDesc) <| fun _ ->
      let reqContext = { HttpContext.empty with request = { HttpContext.empty.request with httpVersion = httpVersion; headers = reqHeaders } }
      let message, expected =
        if shouldAddKeepAlive then
          "should add a keep-alive header to the response", { reqContext with response = { reqContext.response with headers = ("Connection","Keep-Alive") :: reqContext.response.headers } }
        else
          "should not modify the response headers", reqContext
      let actual = HttpContext.addKeepAliveHeader reqContext
      Expect.equal actual.response.headers expected.response.headers message


  testList "when processing keep-alive directives" [
    genKeepAliveTest "HTTP/1.0" None false
    genKeepAliveTest "HTTP/1.0" (Some "close") false
    genKeepAliveTest "HTTP/1.0" (Some "Keep-Alive") true
    genKeepAliveTest "HTTP/1.1" None false
    genKeepAliveTest "HTTP/1.1" (Some "close") false
    genKeepAliveTest "HTTP/1.1" (Some "Keep-Alive") false
    keepAliveUserDataLifetimeTest
  ]


[<Tests>]
let errorHandlerTests (cfg : SuaveConfig) =
  // Requests come from 127.0.0.1, as they do behind a reverse proxy on the same host
  let throwing : WebPart =
    fun _ -> async { return failwith "secret <detail>" }

  let respond config =
    let ctx = runWith config throwing
    reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None id
      (fun r -> statusCode r, contentString r) ctx

  testList "error handlers" [
    testCase "defaultErrorHandler does not disclose the exception to loopback clients" <| fun _ ->
      let status, body = respond { cfg with errorHandler = defaultErrorHandler }
      Expect.equal status HttpStatusCode.InternalServerError "should return 500"
      Expect.equal body HTTP_500.message "should return only the status message"

    testCase "developmentErrorHandler returns the HTML-encoded exception" <| fun _ ->
      let status, body = respond { cfg with errorHandler = developmentErrorHandler }
      Expect.equal status HttpStatusCode.InternalServerError "should return 500"
      Expect.stringContains body "secret &lt;detail&gt;" "should contain the encoded message"
      Expect.isFalse (body.Contains "secret <detail>") "should not contain the raw message"
  ]

[<Tests>]
let bindFailureTests (cfg : SuaveConfig) =
  // Holds a loopback port for the duration of a test, so that Suave's bind fails
  let withHeldPort f =
    let held = new Sockets.TcpListener(IPAddress.Loopback, 0)
    held.Start()
    try
      let port = (held.LocalEndpoint :?> IPEndPoint).Port
      f { cfg with
            bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" port ]
            hideStartupMessage = true }
    finally
      held.Stop()

  // The bind is retried for a few tens of milliseconds; anything near this
  // means the failure is not being reported
  let timeout = TimeSpan.FromSeconds 10.
  let completesPromptly (t : Threading.Tasks.Task) = Threading.Tasks.Task.WaitAny([| t |], timeout) = 0

  let rec isSocketException (ex : exn) =
    match ex with
    | :? Sockets.SocketException -> true
    | :? AggregateException as ae -> ae.InnerExceptions.Count = 1 && isSocketException ae.InnerException
    | _ -> false

  testList "when a binding cannot be bound" [
    testCase "startWebServer throws the SocketException" <| fun _ ->
      withHeldPort (fun config ->
        let run = Threading.Tasks.Task.Run(fun () -> startWebServer config Successful.NO_CONTENT)
        Expect.isTrue (completesPromptly run) "startWebServer should return promptly"
        Expect.isTrue run.IsFaulted "startWebServer should throw"
        Expect.isTrue (run.Exception.InnerException :? Sockets.SocketException)
          (sprintf "should throw a SocketException, got %O" run.Exception.InnerException))

    testCase "listening raises the SocketException and the server task is faulted" <| fun _ ->
      withHeldPort (fun config ->
        let listening, server = startWebServerAsync config Successful.NO_CONTENT
        let started = Async.StartAsTask listening
        Expect.isTrue (completesPromptly started) "listening should complete promptly"
        Expect.isTrue started.IsFaulted "listening should fail"
        Expect.isTrue (isSocketException started.Exception)
          (sprintf "listening should raise a SocketException, got %O" started.Exception)
        Expect.isTrue (completesPromptly server) "the server task should complete promptly"
        Expect.isTrue server.IsFaulted "the server task should be faulted"
        Expect.isTrue (isSocketException server.Exception)
          (sprintf "the server task should fail with a SocketException, got %O" server.Exception))

    testCase "startWebServer throws and stops the bindings that did start" <| fun _ ->
      withHeldPort (fun config ->
        // A port that is free: bind it, note it, release it
        let probe = new Sockets.TcpListener(IPAddress.Loopback, 0)
        probe.Start()
        let freePort = (probe.LocalEndpoint :?> IPEndPoint).Port
        probe.Stop()
        let config =
          { config with bindings = HttpBinding.createSimple HTTP "127.0.0.1" freePort :: config.bindings }
        let run = Threading.Tasks.Task.Run(fun () -> startWebServer config Successful.NO_CONTENT)
        Expect.isTrue (completesPromptly run) "startWebServer should return promptly"
        Expect.isTrue (run.Exception.InnerException :? Sockets.SocketException)
          (sprintf "should throw a SocketException, got %O" run.Exception)
        // The binding that started has been stopped and its port released
        let rebind = new Sockets.TcpListener(IPAddress.Loopback, freePort)
        rebind.Start()
        rebind.Stop())

    testCase "a cancelled server still stops cleanly" <| fun _ ->
      use cts = new Threading.CancellationTokenSource()
      let config = { cfg with cancellationToken = cts.Token; hideStartupMessage = true }
      let listening, server = startWebServerAsync config Successful.NO_CONTENT
      listening |> Async.RunSynchronously |> ignore
      cts.Cancel()
      Expect.isTrue (completesPromptly server) "the server task should complete"
      Expect.equal server.Status Threading.Tasks.TaskStatus.RanToCompletion "the server task should not be faulted"
  ]
