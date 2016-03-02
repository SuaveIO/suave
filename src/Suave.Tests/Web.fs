module Suave.Tests.Web

open Fuchu

open Suave
open Suave.ParsingAndControl
open Suave.Logging

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
      let expected = TraceHeader.mk (Some 7654321UL) (Some 1234567UL)
      Assert.Equal("should parse trace id", expected.traceId, (parseTraceHeaders headers).traceId)
      Assert.Equal("should parse span id to parent span id", expected.reqParentId, (parseTraceHeaders headers).reqParentId)
    ]

[<Tests>]
let keepAliveTests =
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
      let actual = HttpOutput.addKeepAliveHeader reqContext
      Assert.Equal(message, expected.response.headers, actual.response.headers)

  testList "when processing keep-alive directives" [
    genKeepAliveTest "HTTP/1.0" None false
    genKeepAliveTest "HTTP/1.0" (Some "close") false
    genKeepAliveTest "HTTP/1.0" (Some "Keep-Alive") true
    genKeepAliveTest "HTTP/1.1" None false
    genKeepAliveTest "HTTP/1.1" (Some "close") false
    genKeepAliveTest "HTTP/1.1" (Some "Keep-Alive") false
  ]
