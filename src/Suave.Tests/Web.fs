module Suave.Tests.Web

open Expecto

open Suave
open Suave.Operators
open Suave.Logging
open Suave.Testing
open Suave.Sockets.Control
open Suave.Sockets.AsyncSocket
open System
open System.Net
open System.Net.Http
open System.Threading
open Suave.Http

let private (=>) a b = a, b

[<Tests>]
let parsing_tests (_: SuaveConfig) =
  testList "when parsing headers for tracing" [
    // https://github.com/twitter/zipkin/blob/master/doc/collector-api.md#http
    // Parsing these headers are as good as anything, we don't have to use ZipKin
    // but it's the only reference implementation I've found for how to represent spans
    // and traces over http, except dapper.
    //
    // In a similar vein to how 'username' and 'password' are hard-coded
    // in the request record, let's have these there until we need to branch out with
    // more variants.
    testCase "parsing full span/trace headers" <| fun _ ->
      let headers =
        [ "x-b3-spanid"       => "1234567"
          "x-b3-traceid"      => "7654321"
          "x-b3-parentspanid" => "1818181" ]
          // these are not supported:
          //"X-B3-Flags"        => ""
          //"X-B3-Sampled"      => "true" ]

      // the server generates a new one with the client's as the parent?
      // is the semantics that client sends its SpanId and that
      let expected = TraceHeader.create (Some 7654321UL) (Some 1234567UL)
      Expect.equal (TraceHeader.parseTraceHeaders headers).traceId expected.traceId "should parse trace id"
      Expect.equal (TraceHeader.parseTraceHeaders headers).reqParentId expected.reqParentId "should parse span id to parent span id"
    ]
open Suave
open Suave.Sockets

[<Tests>]
let transferEncodingChunkedTests (cfg : SuaveConfig) =
  let writeChunks (conn:Connection) = socket {

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
      | Some v -> sprintf "a 'Connection: %s' header" v, [("connection", v)]
      | None -> "no Connection header", []
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
