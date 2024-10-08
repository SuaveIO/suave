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
