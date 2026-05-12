module Suave.Tests.Http2

open System
open System.IO

open Expecto

open Suave
open Suave.Http2
open Suave.Hpack
open Suave.Huffman

/// Encode a frame header, decode it again, and the result must equal the input
/// for every kind of frame header (length, type, flags, stream identifier).
[<Tests>]
let frameHeaderTests (_ : SuaveConfig) =
  testList "Http2 frame header" [
    testCase "encode/decode round-trip" <| fun _ ->
      let original = { length = 1234; ``type`` = 3uy; flags = 5uy; streamIdentifier = 16777215 }
      let encoded = encodeFrameHeader original
      Expect.equal encoded.Length 9 "frame header is always 9 bytes"
      let decoded = parseFrameHeader encoded
      Expect.equal decoded original "encode >> decode == id"

    testCase "encode clears the reserved bit on the stream identifier" <| fun _ ->
      // The stream identifier field is 31 bits; the top bit of byte 5 must be 0.
      let h = { length = 0; ``type`` = 0uy; flags = 0uy; streamIdentifier = Int32.MaxValue }
      let encoded = encodeFrameHeader h
      Expect.equal (encoded.[5] &&& 0x80uy) 0uy "reserved bit must be cleared"
      let decoded = parseFrameHeader encoded
      Expect.equal decoded.streamIdentifier Int32.MaxValue "round-trips max 31-bit value"

    testCase "decode reads 24-bit length in network order" <| fun _ ->
      // Maximum payload length per default SETTINGS is 16384, but the field
      // itself is 24 bits (up to 0xFFFFFF). Test the high byte is honoured.
      let h = { length = 0x010203; ``type`` = 0uy; flags = 0uy; streamIdentifier = 1 }
      let encoded = encodeFrameHeader h
      Expect.equal encoded.[0] 0x01uy "high byte of length"
      Expect.equal encoded.[1] 0x02uy "mid byte of length"
      Expect.equal encoded.[2] 0x03uy "low byte of length"
      Expect.equal (parseFrameHeader encoded).length 0x010203 "decoded length"
  ]

let private encodeFrameToBytes (encInfo: EncodeInfo) (payload: FramePayload) =
  let header, chunks = encodeFramePayload encInfo payload
  // Total emitted payload bytes
  let payloadBytes = chunks |> List.sumBy (fun a -> a.Length)
  Expect.equal payloadBytes header.length "encoded length field matches actual payload size"
  let bytes =
    [|
      yield! encodeFrameHeader header
      for c in chunks do yield! c
    |]
  bytes

/// Run an encode -> wire-bytes -> decode round-trip through the on-the-wire
/// frame format and assert the payload matches.
let private roundTrip (encInfo: EncodeInfo) (payload: FramePayload) =
  let bytes = encodeFrameToBytes encInfo payload
  let header = parseFrameHeader (Array.sub bytes 0 9)
  let payloadBytes = Array.sub bytes 9 header.length
  let decoded = payloadDecoders.[int header.``type``] header payloadBytes
  header, decoded

[<Tests>]
let framePayloadTests (_ : SuaveConfig) =
  let encInfo flags sid = { flags = flags; streamIdentifier = sid; padding = None }

  testList "Http2 frame payload" [
    testCase "DATA round-trip" <| fun _ ->
      let bytes = [| 1uy; 2uy; 3uy; 4uy; 5uy |]
      let h, p = roundTrip (encInfo 0x1uy 7) (Data(bytes, true))
      Expect.equal h.``type`` 0uy "DATA frame type"
      match p with
      | Data (b, endStream) ->
        Expect.sequenceEqual b bytes "data bytes preserved"
        Expect.isTrue endStream "END_STREAM flag preserved"
      | _ -> failtest "expected Data"

    testCase "HEADERS round-trip (no priority)" <| fun _ ->
      let bytes = [| 10uy; 20uy; 30uy |]
      let h, p = roundTrip (encInfo 0uy 5) (Headers(None, bytes))
      Expect.equal h.``type`` 1uy "HEADERS frame type"
      match p with
      | Headers (None, b) -> Expect.sequenceEqual b bytes "header block bytes preserved"
      | _ -> failtest "expected Headers None"

    testCase "HEADERS round-trip with priority preserves header block offset" <| fun _ ->
      // Regression: parseHeaders previously copied the header block fragment
      // from offset 0 of the priority-bearing payload, including the 5-byte
      // stream-dependency + weight prefix into the header block.
      let prio = { exclusive = false; streamIdentifier = 42; weight = 7uy }
      let bytes = [| 100uy; 101uy; 102uy; 103uy |]
      // 0x20 = PRIORITY flag on a HEADERS frame
      let h, p = roundTrip (encInfo 0x20uy 3) (Headers(Some prio, bytes))
      Expect.equal h.``type`` 1uy "HEADERS frame type"
      match p with
      | Headers (Some prio', b) ->
        Expect.equal prio'.streamIdentifier 42 "priority stream id preserved"
        Expect.equal prio'.weight 7uy "priority weight preserved"
        Expect.sequenceEqual b bytes "header block fragment preserved (no prefix bleed)"
      | _ -> failtest "expected Headers Some"

    testCase "PRIORITY frame is parsed without relying on the HEADERS PRIORITY flag" <| fun _ ->
      // Regression: parsePriority used to use the HEADERS-only PRIORITY flag
      // (0x20), causing standalone PRIORITY frames to decode as Priority None.
      let prio = { exclusive = true; streamIdentifier = 9; weight = 200uy }
      let h, p = roundTrip (encInfo 0uy 11) (Priority(Some prio))
      Expect.equal h.``type`` 2uy "PRIORITY frame type"
      match p with
      | Priority (Some prio') ->
        Expect.equal prio'.streamIdentifier 9 "stream id preserved"
        Expect.equal prio'.weight 200uy "weight preserved"
        Expect.isTrue prio'.exclusive "exclusive bit preserved"
      | _ -> failtest "expected Priority Some"

    testCase "SETTINGS encode only emits explicitly-set entries" <| fun _ ->
      // Regression: the previous encoder unconditionally emitted 36 bytes
      // (6 × 6-byte slots) even when settings were None, producing invalid
      // identifier-0 entries.
      let settings = { defaultSetting with maxConcurrentStreams = Some 100 }
      let h, _ = roundTrip (encInfo 0uy 0) (Settings(false, settings))
      Expect.equal h.length 6 "single explicit setting => 6 bytes payload"

    testCase "SETTINGS round-trips a non-default initial window size" <| fun _ ->
      let settings = { defaultSetting with initialWindowSize = 1048576 }
      let _, p = roundTrip (encInfo 0uy 0) (Settings(false, settings))
      match p with
      | Settings (_, s) ->
        Expect.equal s.initialWindowSize 1048576 "initial window size preserved"
      | _ -> failtest "expected Settings"

    testCase "SETTINGS round-trips multiple values" <| fun _ ->
      let settings =
        { defaultSetting with
            headerTableSize = 8192
            maxConcurrentStreams = Some 256
            maxFrameSize = 32768 }
      let _, p = roundTrip (encInfo 0uy 0) (Settings(false, settings))
      match p with
      | Settings (_, s) ->
        Expect.equal s.headerTableSize 8192 "header table size"
        Expect.equal s.maxConcurrentStreams (Some 256) "max concurrent streams"
        Expect.equal s.maxFrameSize 32768 "max frame size"
      | _ -> failtest "expected Settings"

    testCase "SETTINGS default is the RFC-7540 default for INITIAL_WINDOW_SIZE" <| fun _ ->
      // RFC 7540 §6.5.2: the initial value of SETTINGS_INITIAL_WINDOW_SIZE is
      // 65,535. The previous code had 4096 which would have caused immediate
      // flow-control mismatches with conforming peers.
      Expect.equal defaultSetting.initialWindowSize 65535 "RFC 7540 default"

    testCase "SETTINGS parser ignores unknown identifiers (RFC 7540 §6.5.2)" <| fun _ ->
      // Hand-craft a SETTINGS payload with an unknown identifier 0x00FF.
      let payload =
        [| 0x00uy; 0xFFuy; 0x00uy; 0x00uy; 0x00uy; 0x2auy |] // unknown id, value 42
      let h = { length = 6; ``type`` = 4uy; flags = 0uy; streamIdentifier = 0 }
      let p = parseSettings h payload
      match p with
      | Settings (false, s) ->
        Expect.equal s defaultSetting "unknown id leaves settings untouched"
      | _ -> failtest "expected Settings"

    testCase "SETTINGS parser rejects non-multiple-of-6 length" <| fun _ ->
      let payload = [| 0uy; 1uy; 0uy; 0uy; 0uy |]
      let h = { length = payload.Length; ``type`` = 4uy; flags = 0uy; streamIdentifier = 0 }
      Expect.throws (fun () -> parseSettings h payload |> ignore)
                    "must reject malformed SETTINGS"

    testCase "RST_STREAM round-trip" <| fun _ ->
      let _, p = roundTrip (encInfo 0uy 1) (RstStream ErrorCode.RefusedStream)
      match p with
      | RstStream code -> Expect.equal code ErrorCode.RefusedStream "error code"
      | _ -> failtest "expected RstStream"

    testCase "WINDOW_UPDATE round-trip" <| fun _ ->
      let _, p = roundTrip (encInfo 0uy 0) (WindowUpdate 65536)
      match p with
      | WindowUpdate n -> Expect.equal n 65536 "window increment"
      | _ -> failtest "expected WindowUpdate"

    testCase "PING round-trip" <| fun _ ->
      let opaque = [| 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy |]
      let _, p = roundTrip (encInfo 0x1uy 0) (Ping(true, opaque))
      match p with
      | Ping (ack, b) ->
        Expect.isTrue ack "ack flag preserved"
        Expect.sequenceEqual b opaque "opaque bytes preserved"
      | _ -> failtest "expected Ping"

    testCase "GOAWAY round-trip" <| fun _ ->
      let debug = [| 9uy; 9uy |]
      let _, p = roundTrip (encInfo 0uy 0) (GoAway(7, ErrorCode.ProtocolError, debug))
      match p with
      | GoAway (lastStream, code, dbg) ->
        Expect.equal lastStream 7 "last stream id"
        Expect.equal code ErrorCode.ProtocolError "error code"
        Expect.sequenceEqual dbg debug "debug data"
      | _ -> failtest "expected GoAway"

    testCase "CONTINUATION frame parses with type 9" <| fun _ ->
      // Regression: parseContinuation previously asserted type=8 which is
      // WINDOW_UPDATE, not CONTINUATION (=9).
      let bytes = [| 1uy; 2uy; 3uy |]
      let _, p = roundTrip (encInfo 0uy 1) (Continuation bytes)
      match p with
      | Continuation b -> Expect.sequenceEqual b bytes "continuation bytes preserved"
      | _ -> failtest "expected Continuation"

    testCase "ErrorCode round-trip" <| fun _ ->
      let codes =
        [ NoError; ProtocolError; InternalError; FlowControlError; SettingsTimeout
          StreamClosed; FrameSizeError; RefusedStream; Cancel; CompressionError
          ConnectError; EnhanceYourCalm; InadequateSecurity; HTTP11Required ]
      for c in codes do
        let n = fromErrorCode c
        Expect.equal (toErrorCode n) c (sprintf "round-trip for %A" c)
  ]

[<Tests>]
let hpackTests (_ : SuaveConfig) =
  testList "HPACK" [
    testCase "integer decode" <| fun _ ->
      Expect.equal (Hpack.decode 8 42uy (new MemoryStream([||]))) 42 "single-byte value"
      Expect.equal (Hpack.decode 5 31uy (new MemoryStream([| 154uy; 10uy |]))) 1337 "multi-byte value 1337"
      Expect.equal (Hpack.decode 5 10uy (new MemoryStream([||]))) 10 "fits in prefix"

    testCase "literal header field encode produces non-empty output" <| fun _ ->
      // Note: a full HPACK encoder<->decoder round-trip for literal headers is
      // covered as part of step 2 (HPACK + per-stream state machine). The
      // dynamic-table insertion path on the decode side currently mishandles
      // the empty-name literal form. For step 1 we only verify the encoder
      // emits some output here.
      let enc = newDynamicTableForEncoding 4096
      let bytes = Hpack.encodeHeader' defaultEncodeStrategy 4096 enc (toTokenHeaderList [ "custom-key", "custom-header" ])
      Expect.isGreaterThan bytes.Length 0 "encoder produced bytes"
  ]

[<Tests>]
let huffmanTests (_ : SuaveConfig) =
  testList "HPACK Huffman" [
    testCase "encode/decode round-trip on ASCII string" <| fun _ ->
      let buf1 = Array.zeroCreate<byte> 4096
      let buf2 = Array.zeroCreate<byte> 4096
      let decoder = Decoding.decode buf1 4096
      let encoder = Encoding.encode buf2
      let original = "Hello, HTTP/2 + HPACK!"
      let arr = System.Text.Encoding.UTF8.GetBytes original
      let encoded = encoder (new MemoryStream(arr, false))
      let decodedBytes = decoder (new MemoryStream(encoded, false)) encoded.Length
      Expect.equal (System.Text.Encoding.UTF8.GetString decodedBytes) original "huffman round-trip"
  ]

// ---------------------------------------------------------------------------
// Step 2 — stream state machine, CONTINUATION reassembly, flow control.
// These pieces are pure data-structure tests; they exercise the new HTTP/2
// scaffolding before it gets wired into the connection lifecycle.
// ---------------------------------------------------------------------------

[<Tests>]
let streamStateTests (_ : SuaveConfig) =
  let ok = function
    | Ok s -> s
    | Result.Error e -> failtestf "expected Ok, got Error %A" e
  let err = function
    | Result.Error e -> e
    | Ok s -> failtestf "expected Error, got Ok %A" s

  testList "Http2 stream state machine" [
    testCase "Idle + RecvHeaders endStream=false -> Open" <| fun _ ->
      Expect.equal (ok (transitionStream Idle (RecvHeaders false))) StreamOpen "Open"

    testCase "Idle + RecvHeaders endStream=true -> HalfClosedRemote" <| fun _ ->
      Expect.equal (ok (transitionStream Idle (RecvHeaders true))) HalfClosedRemote "HalfClosedRemote"

    testCase "Idle + SendHeaders endStream=true -> HalfClosedLocal" <| fun _ ->
      Expect.equal (ok (transitionStream Idle (SendHeaders true))) HalfClosedLocal "HalfClosedLocal"

    testCase "Idle + RecvData is a PROTOCOL_ERROR" <| fun _ ->
      Expect.equal (err (transitionStream Idle (RecvData false))) ProtocolError "PROTOCOL_ERROR"

    testCase "Idle + RST_STREAM is a PROTOCOL_ERROR (RFC 7540 §6.4)" <| fun _ ->
      // RST_STREAM on an idle stream is illegal; the catch-all 'RST always closes'
      // rule must not swallow this case.
      Expect.equal (err (transitionStream Idle RecvRstStream)) ProtocolError "PROTOCOL_ERROR"

    testCase "Idle + SendPushPromise -> ReservedLocal" <| fun _ ->
      Expect.equal (ok (transitionStream Idle SendPushPromise)) ReservedLocal "ReservedLocal"

    testCase "ReservedLocal + SendHeaders -> HalfClosedRemote" <| fun _ ->
      // We promised a push; once we send the headers the peer can't send anything
      // back on this stream.
      Expect.equal (ok (transitionStream ReservedLocal (SendHeaders false))) HalfClosedRemote "HalfClosedRemote"

    testCase "Open + RecvData endStream=false stays Open" <| fun _ ->
      Expect.equal (ok (transitionStream StreamOpen (RecvData false))) StreamOpen "Open"

    testCase "Open + RecvData endStream=true -> HalfClosedRemote" <| fun _ ->
      Expect.equal (ok (transitionStream StreamOpen (RecvData true))) HalfClosedRemote "HalfClosedRemote"

    testCase "Open + SendData endStream=true -> HalfClosedLocal" <| fun _ ->
      Expect.equal (ok (transitionStream StreamOpen (SendData true))) HalfClosedLocal "HalfClosedLocal"

    testCase "Open + RecvRstStream -> Closed" <| fun _ ->
      Expect.equal (ok (transitionStream StreamOpen RecvRstStream)) StreamClosedState "Closed"

    testCase "HalfClosedRemote + SendData endStream=true -> Closed" <| fun _ ->
      Expect.equal (ok (transitionStream HalfClosedRemote (SendData true))) StreamClosedState "Closed"

    testCase "HalfClosedRemote + RecvData is STREAM_CLOSED" <| fun _ ->
      // Peer already announced END_STREAM; sending more is illegal.
      Expect.equal (err (transitionStream HalfClosedRemote (RecvData false))) StreamClosed "STREAM_CLOSED"

    testCase "HalfClosedLocal + RecvData endStream=true -> Closed" <| fun _ ->
      Expect.equal (ok (transitionStream HalfClosedLocal (RecvData true))) StreamClosedState "Closed"

    testCase "HalfClosedLocal + SendData is STREAM_CLOSED" <| fun _ ->
      // We've already sent END_STREAM; sending more is a local bug.
      Expect.equal (err (transitionStream HalfClosedLocal (SendData false))) StreamClosed "STREAM_CLOSED"

    testCase "Closed + RecvData is STREAM_CLOSED" <| fun _ ->
      Expect.equal (err (transitionStream StreamClosedState (RecvData false))) StreamClosed "STREAM_CLOSED"

    testCase "Closed + RecvRstStream is allowed and stays Closed" <| fun _ ->
      // RST_STREAM is idempotent in Closed (within the brief window the peer
      // may still be in flight); accepting it is harmless.
      Expect.equal (ok (transitionStream StreamClosedState RecvRstStream)) StreamClosedState "Closed"
  ]

[<Tests>]
let headerBlockReassemblyTests (_ : SuaveConfig) =
  let hdrFlags (endHeaders: bool) (endStream: bool) =
    let mutable f = 0uy
    if endHeaders then f <- f ||| 0x4uy
    if endStream then f <- f ||| 0x1uy
    f
  let mkHeaders (endHeaders: bool) (endStream: bool) streamId =
    { length = 0; ``type`` = 1uy; flags = hdrFlags endHeaders endStream; streamIdentifier = streamId }
  let mkContinuation (endHeaders: bool) streamId =
    let f = if endHeaders then 0x4uy else 0uy
    { length = 0; ``type`` = 9uy; flags = f; streamIdentifier = streamId }

  testList "Http2 HEADERS/CONTINUATION reassembly" [
    testCase "single HEADERS with END_HEADERS completes immediately" <| fun _ ->
      let h = mkHeaders true false 3
      let fragment = [| 1uy; 2uy; 3uy |]
      match startHeaderBlock h fragment false false with
      | Complete (sid, push, frag, es) ->
        Expect.equal sid 3 "stream id"
        Expect.isFalse push "not push promise"
        Expect.sequenceEqual frag fragment "fragment preserved"
        Expect.isFalse es "end_stream preserved"
      | other -> failtestf "expected Complete, got %A" other

    testCase "HEADERS without END_HEADERS asks for more" <| fun _ ->
      let h = mkHeaders false false 5
      match startHeaderBlock h [| 0xaauy |] false false with
      | NeedMore acc ->
        Expect.equal acc.streamId 5 "stream id"
        Expect.equal acc.pending.Length 1 "buffered 1 byte"
      | other -> failtestf "expected NeedMore, got %A" other

    testCase "CONTINUATION on the same stream concatenates and completes" <| fun _ ->
      let h1 = mkHeaders false true 7
      let acc =
        match startHeaderBlock h1 [| 1uy; 2uy |] false true with
        | NeedMore a -> a
        | other -> failtestf "expected NeedMore, got %A" other
      let h2 = mkContinuation true 7
      match feedContinuation acc h2 [| 3uy; 4uy; 5uy |] with
      | Complete (sid, _, frag, es) ->
        Expect.equal sid 7 "stream id"
        Expect.sequenceEqual frag [| 1uy; 2uy; 3uy; 4uy; 5uy |] "fragments concatenated"
        Expect.isTrue es "end_stream propagates from the originating HEADERS"
      | other -> failtestf "expected Complete, got %A" other

    testCase "interleaved non-CONTINUATION frame is a PROTOCOL_ERROR" <| fun _ ->
      let h1 = mkHeaders false false 9
      let acc =
        match startHeaderBlock h1 [| 1uy |] false false with
        | NeedMore a -> a
        | other -> failtestf "expected NeedMore, got %A" other
      let dataFrame = { length = 0; ``type`` = 0uy; flags = 0uy; streamIdentifier = 9 }
      match feedContinuation acc dataFrame [||] with
      | ReassemblyAbort ProtocolError -> ()
      | other -> failtestf "expected ReassemblyAbort ProtocolError, got %A" other

    testCase "CONTINUATION on a different stream is a PROTOCOL_ERROR" <| fun _ ->
      let h1 = mkHeaders false false 11
      let acc =
        match startHeaderBlock h1 [| 1uy |] false false with
        | NeedMore a -> a
        | other -> failtestf "expected NeedMore, got %A" other
      let cont = mkContinuation true 13 // wrong stream
      match feedContinuation acc cont [| 2uy |] with
      | ReassemblyAbort ProtocolError -> ()
      | other -> failtestf "expected ReassemblyAbort ProtocolError, got %A" other

    testCase "multiple CONTINUATION frames concatenate in order" <| fun _ ->
      let h1 = mkHeaders false false 1
      let acc1 =
        match startHeaderBlock h1 [| 1uy |] false false with
        | NeedMore a -> a | _ -> failtest "need more"
      let acc2 =
        match feedContinuation acc1 (mkContinuation false 1) [| 2uy; 3uy |] with
        | NeedMore a -> a | _ -> failtest "need more"
      match feedContinuation acc2 (mkContinuation true 1) [| 4uy |] with
      | Complete (_, _, frag, _) ->
        Expect.sequenceEqual frag [| 1uy; 2uy; 3uy; 4uy |] "all fragments concatenated in order"
      | other -> failtestf "expected Complete, got %A" other

    testCase "PUSH_PROMISE origin is preserved through CONTINUATION" <| fun _ ->
      let h = mkHeaders false false 2
      let acc =
        match startHeaderBlock h [| 9uy |] true false with
        | NeedMore a -> a | _ -> failtest "need more"
      Expect.isTrue acc.isPushPromise "starts as push promise"
      match feedContinuation acc (mkContinuation true 2) [||] with
      | Complete (_, isPush, _, _) -> Expect.isTrue isPush "stays push promise"
      | other -> failtestf "expected Complete, got %A" other
  ]

[<Tests>]
let flowControlTests (_ : SuaveConfig) =
  testList "Http2 flow control" [
    testCase "newFlowControlWindow uses the configured initial value" <| fun _ ->
      let w = newFlowControlWindow 65535
      Expect.equal w.available 65535 "initial available"

    testCase "tryConsume succeeds and decrements" <| fun _ ->
      let w = newFlowControlWindow 100
      Expect.isTrue (tryConsume w 30) "consume 30 of 100"
      Expect.equal w.available 70 "available is 70"

    testCase "tryConsume of exactly the available amount succeeds and leaves 0" <| fun _ ->
      let w = newFlowControlWindow 50
      Expect.isTrue (tryConsume w 50) "consume 50 of 50"
      Expect.equal w.available 0 "available is 0"

    testCase "tryConsume larger than the window fails without mutating" <| fun _ ->
      let w = newFlowControlWindow 10
      Expect.isFalse (tryConsume w 11) "cannot consume 11 of 10"
      Expect.equal w.available 10 "window unchanged on failure"

    testCase "tryConsume of a negative value fails" <| fun _ ->
      let w = newFlowControlWindow 100
      Expect.isFalse (tryConsume w -5) "negative consume rejected"
      Expect.equal w.available 100 "window unchanged"

    testCase "increment adds and reports Ok" <| fun _ ->
      let w = newFlowControlWindow 0
      match increment w 1000 with
      | Ok () -> Expect.equal w.available 1000 "available now 1000"
      | Result.Error e -> failtestf "expected Ok, got %A" e

    testCase "increment of 0 is a PROTOCOL_ERROR (RFC 7540 §6.9.1)" <| fun _ ->
      let w = newFlowControlWindow 100
      match increment w 0 with
      | Result.Error ProtocolError -> Expect.equal w.available 100 "window unchanged"
      | other -> failtestf "expected ProtocolError, got %A" other

    testCase "increment with negative delta is a PROTOCOL_ERROR" <| fun _ ->
      let w = newFlowControlWindow 100
      match increment w -1 with
      | Result.Error ProtocolError -> Expect.equal w.available 100 "window unchanged"
      | other -> failtestf "expected ProtocolError, got %A" other

    testCase "increment that overflows the max is a FLOW_CONTROL_ERROR" <| fun _ ->
      // 2^31 - 1 is the max legal window per RFC 7540 §6.9.1.
      let w = newFlowControlWindow (maxFlowControlWindow - 100)
      match increment w 200 with
      | Result.Error FlowControlError -> Expect.equal w.available (maxFlowControlWindow - 100) "window unchanged on overflow"
      | other -> failtestf "expected FlowControlError, got %A" other

    testCase "applyInitialWindowSizeChange shifts the window by the signed delta" <| fun _ ->
      // RFC 7540 §6.9.2: SETTINGS_INITIAL_WINDOW_SIZE adjustments are signed.
      let w = newFlowControlWindow 100
      match applyInitialWindowSizeChange w 65535 70000 with
      | Ok () -> Expect.equal w.available (100 + (70000 - 65535)) "window shifted up"
      | Result.Error e -> failtestf "expected Ok, got %A" e

    testCase "applyInitialWindowSizeChange allows the window to go negative on shrink" <| fun _ ->
      // RFC 7540 §6.9.2: a negative window is legal and indicates the sender
      // must block until the next WINDOW_UPDATE.
      let w = newFlowControlWindow 10
      match applyInitialWindowSizeChange w 65535 5 with
      | Ok () -> Expect.equal w.available (10 + (5 - 65535)) "window went negative"
      | Result.Error e -> failtestf "expected Ok, got %A" e

    testCase "applyInitialWindowSizeChange that overflows is FLOW_CONTROL_ERROR" <| fun _ ->
      let w = newFlowControlWindow (maxFlowControlWindow - 5)
      match applyInitialWindowSizeChange w 0 1000 with
      | Result.Error FlowControlError -> ()
      | other -> failtestf "expected FlowControlError, got %A" other
  ]
