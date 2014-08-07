module Suave.Tests.Web

open Fuchu

open Suave.Web.ParsingAndControl
open Suave.Log

let private (=>) a b = a, b

[<Tests>]
let parsing_tests =
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
      Assert.Equal("should parse trace id", expected.trace_id, (parse_trace_headers headers).trace_id)
      Assert.Equal("should parse span id to parent span id", expected.req_parent_id, (parse_trace_headers headers).req_parent_id)
    ]