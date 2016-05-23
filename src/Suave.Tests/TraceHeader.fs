module Suave.Tests.TraceHeader

open Suave
open Suave.Logging
open Suave.Testing

open Fuchu

open FsCheck

open Tests.TestUtilities

[<Tests>]
let mk (_ : SuaveConfig) =
  testList "mk" [
    testCase "mk works with both ids given" <| fun _ ->
      let hdr = TraceHeader.mk (Some 82UL) (Some 44UL)
      Assert.Equal("traceId should have been 82", 82UL, hdr.traceId)
      Assert.Equal("reqId should not be 0", true, 0UL <> hdr.reqId)
      Assert.Equal("reqParentId should have been 44", Some 44UL, hdr.reqParentId)
    testCase "mk works with trace id missing" <| fun _ ->
      let hdr = TraceHeader.mk None (Some 86UL)
      Assert.Equal("traceId should not be 0", true, 0UL <> hdr.traceId)
      Assert.Equal("traceId should have defaulted to reqId", hdr.reqId, hdr.traceId)
      Assert.Equal("reqParentId should have been 86", Some 86UL, hdr.reqParentId)
    testCase "mk works with parent id missing" <| fun _ ->
      let hdr = TraceHeader.mk (Some 14UL) None
      Assert.Equal("traceId should have been 14", 14UL, hdr.traceId)
      Assert.Equal("reqId should not be 0", true, 0UL <> hdr.reqId)
      Assert.Equal("reqParentId should have been None", None, hdr.reqParentId)
    testCase "mk works with both ids missing" <| fun _ ->
      let hdr = TraceHeader.mk None None
      Assert.Equal("traceId should not be 0", true, 0UL <> hdr.traceId)
      Assert.Equal("traceId should have defaulted to reqId", hdr.reqId, hdr.traceId)
      Assert.Equal("reqParentId should have been None", None, hdr.reqParentId)
  ]

[<Tests>]
let parseTraceHeaders (_ : SuaveConfig) =
  testList "parseTraceHeaders" [
    testCase "parseTraceHeaders works when there are no headers" <| fun _ ->
      let hdr = TraceHeader.parseTraceHeaders []
      Assert.Equal("traceId should not be 0", true, 0UL <> hdr.traceId)
      Assert.Equal("traceId should have defaulted to reqId", hdr.reqId, hdr.traceId)
      Assert.Equal("reqParentId should have been None", None, hdr.reqParentId)
    testCase "parseTraceHeaders works when there is only a traceId header" <| fun _ ->
      let hdr = TraceHeader.parseTraceHeaders [ "x-b3-traceid", "55" ]
      Assert.Equal("traceId should be 55", 55UL, hdr.traceId)
      Assert.Equal("reqId should not be 0", true, 0UL <> hdr.reqId)
      Assert.Equal("reqParentId should be None", None, hdr.reqParentId)
    testCase "parseTraceHeaders works where there is only a parent (span) Id" <| fun _ ->
      let hdr = TraceHeader.parseTraceHeaders [ "x-b3-spanid", "22" ]
      Assert.Equal("traceId should not be 0", true, 0UL <> hdr.traceId)
      Assert.Equal("traceId should have defaulted to reqId", hdr.reqId, hdr.traceId)
      Assert.Equal("reqParentId should have been 22", Some 22UL, hdr.reqParentId)
    testCase "parseTraceHeaders works when there are both Ids" <| fun _ ->
      let hdr = TraceHeader.parseTraceHeaders [ "x-b3-traceid", "91"; "x-b3-spanid", "27" ]
      Assert.Equal("traceId should be 91", 91UL, hdr.traceId)
      Assert.Equal("reqId should not be 0", true, 0UL <> hdr.reqId)
      Assert.Equal("reqParentId should have been 27", Some 27UL, hdr.reqParentId)
    testCase "parseTraceHeaders works when the traceId is unparseable" <| fun _ ->
      let hdr = TraceHeader.parseTraceHeaders [ "x-b3-traceid", "crab" ]
      Assert.Equal("traceId should not be 0", true, 0UL <> hdr.traceId)
      Assert.Equal("traceId should have defaulted to reqId", hdr.reqId, hdr.traceId)
      Assert.Equal("reqParentId should have been None", None, hdr.reqParentId)
    testCase "parseTraceHeaders works when the parent (span) Id is unparseable" <| fun _ ->
      let hdr = TraceHeader.parseTraceHeaders [ "x-b3-traceid", "41"; "x-b3-spanid", "lobster" ]
      Assert.Equal("traceId should be 41", 41UL, hdr.traceId)
      Assert.Equal("reqId should not be 0", true, 0UL <> hdr.reqId)
      Assert.Equal("reqParentId should have been None", None, hdr.reqParentId)
  ]