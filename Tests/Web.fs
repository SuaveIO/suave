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
        [ "X-B3-SpanId" => "1234567"
          ""            => "7654321" ]
        |> Map.ofList
      Assert.Equal(
        "shold parse trace id, span id and parent span id",
        parse_trace_headers headers,
        TraceHeader.NewTrace())
    ]