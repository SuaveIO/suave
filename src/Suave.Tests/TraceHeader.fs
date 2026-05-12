module Suave.Tests.TraceHeader

open Suave
open Suave.Logging
open Suave.Testing
open Expecto
open FsCheck
open Tests.TestUtilities

[<Tests>]
let create (_ : SuaveConfig) =
  testList "create" [
    testCase "create works with both ids given" <| fun _ ->
      let hdr = TraceHeader.create (Some 82UL) (Some 44UL)
      Expect.equal hdr.traceId 82UL "traceId should have been 82"
      Expect.isTrue (0UL <> hdr.reqId) "reqId should not be 0"
      Expect.equal hdr.reqParentId (Some 44UL) "reqParentId should have been 44"

    testCase "create works with trace id missing" <| fun _ ->
      let hdr = TraceHeader.create None (Some 86UL)
      Expect.isTrue (0UL <> hdr.traceId) "traceId should not be 0"
      Expect.equal hdr.traceId hdr.reqId "traceId should have defaulted to reqId"
      Expect.equal hdr.reqParentId (Some 86UL) "reqParentId should have been 86"

    testCase "create works with parent id missing" <| fun _ ->
      let hdr = TraceHeader.create (Some 14UL) None
      Expect.equal hdr.traceId 14UL "traceId should have been 14"
      Expect.isTrue (0UL <> hdr.reqId) "reqId should not be 0"
      Expect.equal hdr.reqParentId None "reqParentId should have been None"

    testCase "create works with both ids missing" <| fun _ ->
      let hdr = TraceHeader.create None None
      Expect.isTrue (0UL <> hdr.traceId) "traceId should not be 0"
      Expect.equal hdr.traceId hdr.reqId "traceId should have defaulted to reqId"
      Expect.equal hdr.reqParentId None "reqParentId should have been None"
  ]

[<Tests>]
let parseTraceHeaders (_ : SuaveConfig) =
  testList "parseTraceHeaders" [
    testCase "parseTraceHeaders works when there are no headers" <| fun _ ->
      let hdr = TraceHeader.parseTraceHeaders []
      Expect.isTrue (0UL <> hdr.traceId) "traceId should not be 0"
      Expect.equal hdr.traceId hdr.reqId "traceId should have defaulted to reqId"
      Expect.equal hdr.reqParentId None "reqParentId should have been None"

    testCase "parseTraceHeaders works when there is only a traceId header" <| fun _ ->
      let hdr = TraceHeader.parseTraceHeaders [ "x-b3-traceid", "55" ]
      Expect.equal hdr.traceId 55UL "traceId should be 55"
      Expect.isTrue (0UL <> hdr.reqId) "reqId should not be 0"
      Expect.isNone hdr.reqParentId "reqParentId should be None"

    testCase "parseTraceHeaders works where there is only a parent (span) Id" <| fun _ ->
      let hdr = TraceHeader.parseTraceHeaders [ "x-b3-spanid", "22" ]
      Expect.isTrue (0UL <> hdr.traceId) "traceId should not be 0"
      Expect.equal hdr.traceId hdr.reqId "traceId should have defaulted to reqId"
      Expect.equal hdr.reqParentId (Some 22UL) "reqParentId should have been 22"

    testCase "parseTraceHeaders works when there are both Ids" <| fun _ ->
      let hdr = TraceHeader.parseTraceHeaders [ "x-b3-traceid", "91"; "x-b3-spanid", "27" ]
      Expect.equal hdr.traceId 91UL "traceId should be 91"
      Expect.isTrue (0UL <> hdr.reqId) "reqId should not be 0"
      Expect.equal hdr.reqParentId (Some 27UL) "reqParentId should have been 27"

    testCase "parseTraceHeaders works when the traceId is unparseable" <| fun _ ->
      let hdr = TraceHeader.parseTraceHeaders [ "x-b3-traceid", "crab" ]
      Expect.isTrue (0UL <> hdr.traceId) "traceId should not be 0"
      Expect.equal hdr.traceId hdr.reqId "traceId should have defaulted to reqId"
      Expect.equal hdr.reqParentId None "reqParentId should have been None"

    testCase "parseTraceHeaders works when the parent (span) Id is unparseable" <| fun _ ->
      let hdr = TraceHeader.parseTraceHeaders [ "x-b3-traceid", "41"; "x-b3-spanid", "lobster" ]
      Expect.equal hdr.traceId 41UL "traceId should be 41"
      Expect.isTrue (0UL <> hdr.reqId) "reqId should not be 0"
      Expect.equal hdr.reqParentId None "reqParentId should have been None"
  ]
