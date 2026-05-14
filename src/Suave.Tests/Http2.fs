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
    yield testCase "integer decode" <| fun _ ->
      Expect.equal (Hpack.decode 8 42uy (new MemoryStream([||]))) 42 "single-byte value"
      Expect.equal (Hpack.decode 5 31uy (new MemoryStream([| 154uy; 10uy |]))) 1337 "multi-byte value 1337"
      Expect.equal (Hpack.decode 5 10uy (new MemoryStream([||]))) 10 "fits in prefix"

    yield testCase "literal header field encode produces non-empty output" <| fun _ ->
      // Note: a full HPACK encoder<->decoder round-trip for literal headers is
      // covered as part of step 2 (HPACK + per-stream state machine). The
      // dynamic-table insertion path on the decode side currently mishandles
      // the empty-name literal form. For step 1 we only verify the encoder
      // emits some output here.
      let enc = newDynamicTableForEncoding 4096
      let bytes = Hpack.encodeHeader' defaultEncodeStrategy 4096 enc (toTokenHeaderList [ "custom-key", "custom-header" ])
      Expect.isGreaterThan bytes.Length 0 "encoder produced bytes"

    // The following four tests exercise the encoder/decoder round-trip for
    // each of the three encoding strategies (Naive, Static, Linear), and for
    // both Huffman-encoded and literal string forms. They guard against the
    // historical regressions where:
    //   * `encodeString` mis-computed the length-prefix width and produced
    //     garbage output in the Huffman path (Naive),
    //   * `lookupStaticRevIndex` had an empty body so Static/Linear emitted
    //     nothing when a header's name was in the static table,
    //   * `encodeTokenHeader` ignored `strategy.useHuffman` and always picked
    //     the Huffman path regardless of caller preference.
    let roundTrip strategy headers =
      let enc = newDynamicTableForEncoding 4096
      let bytes = Hpack.encodeHeader strategy 4096 enc headers
      Expect.isGreaterThan bytes.Length 0 "encoder produced bytes"
      let dec = newDynamicTableForDecoding 4096 4096
      Hpack.decodeHeader dec bytes

    yield testCase "Naive strategy round-trips a literal-name header" <| fun _ ->
      let strategy = { algorithm = Naive; useHuffman = false }
      let decoded = roundTrip strategy [ "custom-key", "custom-header" ]
      Expect.equal decoded [ "custom-key", "custom-header" ] "Naive (literal) round-trips"

    yield testCase "Naive strategy round-trips a Huffman-encoded header" <| fun _ ->
      let strategy = { algorithm = Naive; useHuffman = true }
      // "www.example.com" is the canonical Huffman example from RFC 7541
      // Appendix C.4.1 — its Huffman form (12 bytes) is shorter than the
      // literal form (15 bytes), so the encoder must select the H-bit branch.
      let decoded = roundTrip strategy [ "custom-key", "www.example.com" ]
      Expect.equal decoded [ "custom-key", "www.example.com" ] "Naive (Huffman) round-trips"

    yield testCase "Static strategy round-trips a static-table header" <| fun _ ->
      let strategy = { algorithm = Static; useHuffman = false }
      // ":path" is entry 4 (":path: /") and entry 5 (":path: /index.html") in
      // the static table — sending "/foo" forces the "literal with indexed
      // name" branch in `lookupStaticRevIndex`, exercising the formerly-empty
      // function body.
      let decoded = roundTrip strategy [ ":path", "/foo" ]
      Expect.equal decoded [ ":path", "/foo" ] "Static (indexed name) round-trips"

    yield testCase "Linear strategy round-trips a fully-indexed static header" <| fun _ ->
      let strategy = { algorithm = Linear; useHuffman = false }
      // ":method: GET" is entry 2 in the static table — it must be emitted as
      // a single "indexed" byte (0x82). Previously Linear produced an empty
      // block for this header.
      let decoded = roundTrip strategy [ ":method", "GET" ]
      Expect.equal decoded [ ":method", "GET" ] "Linear (fully indexed) round-trips"

    yield testCase "decode literal header field with incremental indexing, new name (RFC 7541 C.2.1)" <| fun _ ->
      // Regression for the "Empty header value" bug on literal-name decode:
      // extractByteString used to read from offset 0 of the underlying
      // MemoryStream buffer and never advanced Position, so the second
      // headerStuff call inside insertNewName ended up consuming the very
      // first bytes of the input as the value-length prefix, decoding the
      // header value as empty.
      //
      // RFC 7541, Appendix C.2.1 hex dump:
      //   400a 6375 7374 6f6d 2d6b 6579 0d63 7573
      //   746f 6d2d 6865 6164 6572
      let bytes =
        [| 0x40uy; 0x0auy
           0x63uy; 0x75uy; 0x73uy; 0x74uy; 0x6fuy; 0x6duy; 0x2duy; 0x6buy; 0x65uy; 0x79uy
           0x0duy
           0x63uy; 0x75uy; 0x73uy; 0x74uy; 0x6fuy; 0x6duy; 0x2duy; 0x68uy; 0x65uy; 0x61uy; 0x64uy; 0x65uy; 0x72uy |]
      let dec = newDynamicTableForDecoding 4096 4096
      let decoded = Hpack.decodeHeader dec bytes
      Expect.equal decoded [ "custom-key", "custom-header" ] "decoded literal new-name header round-trip"

    yield testCase "decode accepts two consecutive dynamic table size updates (RFC 7541 §4.2)" <| fun _ ->
      // h2spec generic/5/15 — the encoder MAY emit up to two dynamic table
      // size updates at the start of a header block (e.g. to acknowledge a
      // reduction and then settle on a new value). The decoder must accept
      // both, including the case where the second update restores the size to
      // the advertised maximum. Previously `isSuitableSize` used a strict `<`
      // so size==maximum was rejected and the second 0x20-prefixed octet tore
      // down the connection.
      //
      // Wire format:
      //   0x20            — table size update, size = 0  (mask5 = 0)
      //   0x3F 0xE1 0x1F  — table size update, size = 4096 (5-bit prefix
      //                     value 31 + 4065 = 31 + 0x61 + (0x1F << 7))
      //   0x82            — indexed header field, static index 2 (":method: GET")
      let bytes = [| 0x20uy; 0x3Fuy; 0xE1uy; 0x1Fuy; 0x82uy |]
      let dec = newDynamicTableForDecoding 4096 4096
      let decoded = Hpack.decodeHeader dec bytes
      Expect.equal decoded [ ":method", "GET" ] "two size updates followed by a header decode"

    yield testCase "decode rejects an indexed header with an out-of-range index (RFC 7541 §2.3.3)" <| fun _ ->
      // h2spec hpack/2.3.3/1 — the index address space is the union of the
      // static and dynamic tables. A reference beyond the populated portion
      // (e.g. into the empty dynamic table) MUST surface as a decoding error
      // that the dispatch loop translates into a connection-level
      // GOAWAY(COMPRESSION_ERROR). Previously the dynamic branch was
      // unbounded and silently returned a stale circular-buffer entry, so the
      // request was served instead of the connection being torn down.
      //
      // 0xBE = 0x80 | 62 — indexed header, idx = 62 (one past the static
      // table, into a brand-new empty dynamic table).
      let bytes = [| 0xBEuy |]
      let dec = newDynamicTableForDecoding 4096 4096
      Expect.throws (fun () -> Hpack.decodeHeader dec bytes |> ignore)
                    "out-of-range indexed reference must raise a decoding error"

      // Index 0 (0x80) is reserved and must also be rejected even in Release
      // builds where the `assert` in `indexed` is compiled out.
      let dec2 = newDynamicTableForDecoding 4096 4096
      Expect.throws (fun () -> Hpack.decodeHeader dec2 [| 0x80uy |] |> ignore)
                    "indexed reference with index 0 must raise a decoding error"
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
      Expect.equal (ok (transitionStream Idle (RecvHeaders false))) Open "Open"

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
      Expect.equal (ok (transitionStream Open (RecvData false))) Open "Open"

    testCase "Open + RecvData endStream=true -> HalfClosedRemote" <| fun _ ->
      Expect.equal (ok (transitionStream Open (RecvData true))) HalfClosedRemote "HalfClosedRemote"

    testCase "Open + SendData endStream=true -> HalfClosedLocal" <| fun _ ->
      Expect.equal (ok (transitionStream Open (SendData true))) HalfClosedLocal "HalfClosedLocal"

    testCase "Open + RecvRstStream -> Closed" <| fun _ ->
      Expect.equal (ok (transitionStream Open RecvRstStream)) Closed "Closed"

    testCase "HalfClosedRemote + SendData endStream=true -> Closed" <| fun _ ->
      Expect.equal (ok (transitionStream HalfClosedRemote (SendData true))) Closed "Closed"

    testCase "HalfClosedRemote + RecvData is STREAM_CLOSED" <| fun _ ->
      // Peer already announced END_STREAM; sending more is illegal.
      Expect.equal (err (transitionStream HalfClosedRemote (RecvData false))) StreamClosed "STREAM_CLOSED"

    testCase "HalfClosedLocal + RecvData endStream=true -> Closed" <| fun _ ->
      Expect.equal (ok (transitionStream HalfClosedLocal (RecvData true))) Closed "Closed"

    testCase "HalfClosedLocal + SendData is STREAM_CLOSED" <| fun _ ->
      // We've already sent END_STREAM; sending more is a local bug.
      Expect.equal (err (transitionStream HalfClosedLocal (SendData false))) StreamClosed "STREAM_CLOSED"

    testCase "Closed + RecvData is STREAM_CLOSED" <| fun _ ->
      Expect.equal (err (transitionStream Closed (RecvData false))) StreamClosed "STREAM_CLOSED"

    testCase "Closed + RecvRstStream is allowed and stays Closed" <| fun _ ->
      // RST_STREAM is idempotent in Closed (within the brief window the peer
      // may still be in flight); accepting it is harmless.
      Expect.equal (ok (transitionStream Closed RecvRstStream)) Closed "Closed"
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

// ---------------------------------------------------------------------------
// Step 5 — frame read/dispatch loop pure helpers.
//
// The dispatch loop itself is a stateful object on top of a transport and is
// exercised end-to-end via the H2SpecHost fixture. The pure helpers below
// (HPACK response encoding + request-header pseudo-header extraction) are
// unit-tested here to lock in their semantics.
// ---------------------------------------------------------------------------

[<Tests>]
let dispatchLoopHelperTests (_ : SuaveConfig) =
  testList "Http2 dispatch loop helpers" [

    testCase "encodeHpackHeaderBlock round-trips through the in-tree decoder" <| fun _ ->
      // The dispatch loop hands header lists to the full HPACK encoder in
      // `Suave.Hpack` via a per-connection encoder dynamic table. The output
      // must be parseable by a peer decoder; we round-trip it through the
      // in-tree decoder here to guard against future encoder regressions.
      let encTbl = newDynamicTableForEncoding 4096
      let encoded =
        Http2.encodeHpackHeaderBlock encTbl
          [ ":status", "200"
            "content-type", "text/plain"
            "content-length", "5" ]
      let dt = newDynamicTableForDecoding 4096 4096
      let decoded = Hpack.decodeHeader dt encoded
      Expect.equal decoded
        [ ":status", "200"
          "content-type", "text/plain"
          "content-length", "5" ]
        "HPACK header block round-trips"

    testCase "encodeHpackHeaderBlock preserves header order across calls on the same encoder table" <| fun _ ->
      // HPACK is stateful: a second call on the same encoder dynamic table may
      // emit indexed references to entries inserted by the first call. The
      // peer's decoder, sharing state, must still decode the same headers.
      let encTbl = newDynamicTableForEncoding 4096
      let decTbl = newDynamicTableForDecoding 4096 4096
      let first  = Http2.encodeHpackHeaderBlock encTbl [ ":status", "200"; "content-type", "text/plain" ]
      let second = Http2.encodeHpackHeaderBlock encTbl [ ":status", "200"; "content-type", "text/plain" ]
      Expect.equal (Hpack.decodeHeader decTbl first)
                   [ ":status", "200"; "content-type", "text/plain" ]
                   "first block decodes"
      Expect.equal (Hpack.decodeHeader decTbl second)
                   [ ":status", "200"; "content-type", "text/plain" ]
                   "second block decodes with shared dynamic-table state"

    testCase "encodeHpackHeaderBlock lowercases header names on the wire" <| fun _ ->
      // HTTP/2 (RFC 7540 §8.1.2) requires lowercase field names on the wire.
      // `Hpack.toToken` lowercases names before they reach the encoder.
      let encTbl = newDynamicTableForEncoding 4096
      let encoded = Http2.encodeHpackHeaderBlock encTbl [ "Content-Type", "text/plain" ]
      let decTbl = newDynamicTableForDecoding 4096 4096
      match Hpack.decodeHeader decTbl encoded with
      | [ name, _ ] -> Expect.equal name "content-type" "name was lowercased"
      | other -> failtestf "expected single header, got %A" other

    testCase "extractRequestPseudoHeaders splits pseudo and regular headers" <| fun _ ->
      let headers =
        [ ":method", "GET"
          ":scheme", "http"
          ":path", "/foo?q=1"
          ":authority", "example.com"
          "accept", "*/*" ]
      match Http2.extractRequestPseudoHeaders headers with
      | Ok (m, p, s, a, regular) ->
        Expect.equal m "GET"             ":method"
        Expect.equal p "/foo?q=1"        ":path"
        Expect.equal s "http"            ":scheme"
        Expect.equal a "example.com"     ":authority"
        Expect.equal regular [ "accept", "*/*" ] "regular headers"
      | Result.Error err -> failtestf "expected Ok, got %A" err

    testCase "extractRequestPseudoHeaders rejects a missing :method" <| fun _ ->
      let headers =
        [ ":scheme", "http"
          ":path", "/" ]
      match Http2.extractRequestPseudoHeaders headers with
      | Result.Error ProtocolError -> ()
      | other -> failtestf "expected ProtocolError, got %A" other

    testCase "extractRequestPseudoHeaders rejects pseudo-header after regular" <| fun _ ->
      // RFC 7540 §8.1.2.1: pseudo-header fields MUST appear before regular
      // header fields.
      let headers =
        [ ":method", "GET"
          "accept", "*/*"
          ":path", "/"
          ":scheme", "http" ]
      match Http2.extractRequestPseudoHeaders headers with
      | Result.Error ProtocolError -> ()
      | other -> failtestf "expected ProtocolError, got %A" other

    testCase "extractRequestPseudoHeaders rejects a duplicate :path" <| fun _ ->
      let headers =
        [ ":method", "GET"
          ":scheme", "http"
          ":path", "/a"
          ":path", "/b" ]
      match Http2.extractRequestPseudoHeaders headers with
      | Result.Error ProtocolError -> ()
      | other -> failtestf "expected ProtocolError, got %A" other

    testCase "extractRequestPseudoHeaders rejects connection-specific fields" <| fun _ ->
      // RFC 7540 §8.1.2.2: connection-specific header fields MUST be treated
      // as malformed.
      let headers =
        [ ":method", "GET"
          ":scheme", "http"
          ":path", "/"
          "connection", "close" ]
      match Http2.extractRequestPseudoHeaders headers with
      | Result.Error ProtocolError -> ()
      | other -> failtestf "expected ProtocolError, got %A" other

    testCase "extractRequestPseudoHeaders accepts TE: trailers only" <| fun _ ->
      // RFC 7540 §8.1.2.2: the TE header field MAY appear in an HTTP/2
      // request, but the only allowed value is "trailers".
      let headersOk =
        [ ":method", "GET"
          ":scheme", "http"
          ":path", "/"
          "te", "trailers" ]
      match Http2.extractRequestPseudoHeaders headersOk with
      | Ok _ -> ()
      | other -> failtestf "expected Ok, got %A" other

    testCase "extractRequestPseudoHeaders rejects TE with non-trailers value" <| fun _ ->
      let headers =
        [ ":method", "GET"
          ":scheme", "http"
          ":path", "/"
          "te", "gzip" ]
      match Http2.extractRequestPseudoHeaders headers with
      | Result.Error ProtocolError -> ()
      | other -> failtestf "expected ProtocolError, got %A" other

    testCase "newStreamData starts in Idle with configured windows" <| fun _ ->
      let s = Http2.newStreamData 12345 65535
      Expect.equal s.state StreamState.Idle "state Idle"
      Expect.equal s.outboundWindow.available 12345 "outbound = peer initial"
      Expect.equal s.inboundWindow.available 65535 "inbound = local initial"
      Expect.equal s.requestHeaders [] "no headers yet"
      Expect.equal s.trailers [] "no trailers yet"
      Expect.isFalse s.endStreamReceived "END_STREAM not yet seen"
      Expect.isFalse s.dispatched "not dispatched"
  ]

// ---------------------------------------------------------------------------
// Step 3 — h2c upgrade detection and HTTP2-Settings decoding (RFC 7540 §3.2).
// These tests only cover the pure pieces of the upgrade path; the actual
// connection handoff is exercised through the existing Web.fs integration
// tests (which still pass — non-h2c traffic is unaffected).
// ---------------------------------------------------------------------------

[<Tests>]
let h2cUpgradeTests (_ : SuaveConfig) =
  let mkRequest (headers: (string * string) list) =
    { HttpRequest.empty with
        headers = System.Collections.Generic.List<_>(headers) }

  // The minimal client SETTINGS payload is 0 bytes — RFC 7540 allows it.
  // base64url("") = ""
  let validUpgradeHeaders : (string * string) list =
    [ "Host", "example.com"
      "Connection", "Upgrade, HTTP2-Settings"
      "Upgrade", "h2c"
      "HTTP2-Settings", "" ]

  testList "Http2 h2c upgrade" [
    testCase "decodeBase64Url accepts standard base64" <| fun _ ->
      let bytes = H2cUpgrade.decodeBase64Url "AAQAAP__"
      Expect.isSome bytes "should decode"
      Expect.equal (Option.get bytes).Length 6 "6-byte SETTINGS entry"

    testCase "decodeBase64Url accepts unpadded input" <| fun _ ->
      // base64url has no padding; "AA" decodes to a single 0 byte once we
      // re-pad to "AA==".
      let bytes = H2cUpgrade.decodeBase64Url "AA"
      Expect.isSome bytes "should decode"
      Expect.equal (Option.get bytes) [| 0uy |] "single 0 byte"

    testCase "decodeBase64Url translates the URL-safe alphabet" <| fun _ ->
      // 0xFB 0xFF decodes from standard base64 "+/8=" and base64url "-_8".
      let std = H2cUpgrade.decodeBase64Url "+/8="
      let url = H2cUpgrade.decodeBase64Url "-_8"
      Expect.equal std url "url-safe and standard alphabets agree"

    testCase "decodeBase64Url returns None on garbage" <| fun _ ->
      Expect.isNone (H2cUpgrade.decodeBase64Url "!!!not base64!!!") "rejected"

    testCase "tryDecodeHttp2SettingsHeader accepts an empty payload" <| fun _ ->
      // RFC 7540 §3.2.1: an empty SETTINGS payload is legal — the client
      // accepts the server defaults.
      match H2cUpgrade.tryDecodeHttp2SettingsHeader "" with
      | Some s -> Expect.equal s defaultSetting "empty -> defaults"
      | None -> failtest "should accept empty payload"

    testCase "tryDecodeHttp2SettingsHeader decodes a single SETTINGS entry" <| fun _ ->
      // Encode SETTINGS_INITIAL_WINDOW_SIZE (id=4) = 0xFFFF as the on-wire
      // SETTINGS body, then base64url it (no padding).
      let payload = [| 0x00uy; 0x04uy; 0x00uy; 0x00uy; 0xFFuy; 0xFFuy |]
      let b64 = System.Convert.ToBase64String(payload).TrimEnd('=')
      match H2cUpgrade.tryDecodeHttp2SettingsHeader b64 with
      | Some s -> Expect.equal s.initialWindowSize 0xFFFF "initial window size decoded"
      | None -> failtest "should accept valid SETTINGS"

    testCase "tryDecodeHttp2SettingsHeader rejects non-multiple-of-6 length" <| fun _ ->
      // 5 bytes -> base64 -> tryDecode should reject.
      let bad = System.Convert.ToBase64String([| 1uy; 2uy; 3uy; 4uy; 5uy |]).TrimEnd('=')
      Expect.isNone (H2cUpgrade.tryDecodeHttp2SettingsHeader bad) "must reject"

    testCase "isH2cUpgradeRequest recognises a well-formed upgrade" <| fun _ ->
      Expect.isTrue
        (ConnectionFacade.isH2cUpgradeRequest (mkRequest validUpgradeHeaders))
        "all three signals present => h2c upgrade"

    testCase "isH2cUpgradeRequest is case-insensitive on token values" <| fun _ ->
      let req = mkRequest [
        "Host", "example.com"
        "Connection", "upgrade, http2-settings"
        "Upgrade", "H2C"
        "HTTP2-Settings", ""
      ]
      Expect.isTrue (ConnectionFacade.isH2cUpgradeRequest req)
                    "tokens are case-insensitive"

    testCase "isH2cUpgradeRequest rejects WebSocket upgrade" <| fun _ ->
      // Regression: WebSocket Upgrade flow must still go through the user web
      // part. The h2c classifier must not match it.
      let req = mkRequest [
        "Host", "example.com"
        "Connection", "Upgrade"
        "Upgrade", "websocket"
        "Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ=="
      ]
      Expect.isFalse (ConnectionFacade.isH2cUpgradeRequest req)
                     "websocket upgrade must not be intercepted as h2c"

    testCase "isH2cUpgradeRequest requires HTTP2-Settings header" <| fun _ ->
      let req = mkRequest [
        "Host", "example.com"
        "Connection", "Upgrade, HTTP2-Settings"
        "Upgrade", "h2c"
      ]
      Expect.isFalse (ConnectionFacade.isH2cUpgradeRequest req)
                     "missing HTTP2-Settings header"

    testCase "isH2cUpgradeRequest requires Connection header to mention HTTP2-Settings" <| fun _ ->
      let req = mkRequest [
        "Host", "example.com"
        "Connection", "Upgrade"
        "Upgrade", "h2c"
        "HTTP2-Settings", ""
      ]
      Expect.isFalse (ConnectionFacade.isH2cUpgradeRequest req)
                     "RFC 7540 §3.2 requires both 'Upgrade' and 'HTTP2-Settings' tokens"

    testCase "isH2cUpgradeRequest rejects a plain HTTP/1.1 GET" <| fun _ ->
      Expect.isFalse
        (ConnectionFacade.isH2cUpgradeRequest (mkRequest [ "Host", "example.com" ]))
        "no upgrade headers => regular request"

    testCase "register installs the upgrade handler hook" <| fun _ ->
      // The hook is registered eagerly by Tcp.createPools; calling register
      // again is idempotent and must leave a Some in place so any future
      // ConnectionFacade respects h2c upgrades.
      H2cUpgrade.register ()
      Expect.isTrue ConnectionFacade.Http2UpgradeHandler.IsSome
                    "Http2UpgradeHandler must be set"
  ]

// ---------------------------------------------------------------------------
// Step 3b — h2c prior-knowledge detection (RFC 7540 §3.4).
// A client with prior knowledge that the server speaks HTTP/2 cleartext
// opens the connection with the literal 24-byte preface
// ("PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"). Detection is done by parsing the
// first line as if it were an HTTP/1.1 request line and matching the
// (method, target, version) triple — no other HTTP/1.1 request can look
// like this. These tests cover the pure detector plus the registration
// hook; the actual connection handoff is exercised end-to-end by the
// prior-knowledge integration test below.
// ---------------------------------------------------------------------------

[<Tests>]
let h2cPriorKnowledgeTests (_ : SuaveConfig) =
  testList "Http2 h2c prior-knowledge" [
    testCase "isHttp2PriorKnowledgePreface matches the canonical request line" <| fun _ ->
      Expect.isTrue
        (ConnectionFacade.isHttp2PriorKnowledgePreface "PRI" "*" "HTTP/2.0")
        "PRI * HTTP/2.0 is the HTTP/2 connection preface"

    testCase "isHttp2PriorKnowledgePreface rejects ordinary HTTP/1.1 GET" <| fun _ ->
      Expect.isFalse
        (ConnectionFacade.isHttp2PriorKnowledgePreface "GET" "/" "HTTP/1.1")
        "regular requests are not the preface"

    testCase "isHttp2PriorKnowledgePreface requires HTTP/2.0 version exactly" <| fun _ ->
      // OPTIONS * is legal in HTTP/1.1 but must not be mistaken for the preface.
      Expect.isFalse
        (ConnectionFacade.isHttp2PriorKnowledgePreface "OPTIONS" "*" "HTTP/1.1")
        "OPTIONS * HTTP/1.1 must remain HTTP/1.1"
      // The method must be the literal "PRI" — no other token can substitute.
      Expect.isFalse
        (ConnectionFacade.isHttp2PriorKnowledgePreface "GET" "*" "HTTP/2.0")
        "method must be PRI"
      // The path/target must be the literal "*".
      Expect.isFalse
        (ConnectionFacade.isHttp2PriorKnowledgePreface "PRI" "/" "HTTP/2.0")
        "target must be *"

    testCase "isHttp2PriorKnowledgePreface is case-sensitive on method" <| fun _ ->
      // RFC 7540 §3.5 specifies the preface bytes literally; lowercase "pri"
      // is not the connection preface and would also not match HTTP method
      // tokens (which are case-sensitive per RFC 9110).
      Expect.isFalse
        (ConnectionFacade.isHttp2PriorKnowledgePreface "pri" "*" "HTTP/2.0")
        "method comparison is byte-exact"

    testCase "register installs the prior-knowledge handler hook" <| fun _ ->
      // The hook is registered eagerly by Tcp.createPools; calling register
      // again is idempotent and must leave a Some in place.
      H2cPriorKnowledge.register ()
      Expect.isTrue ConnectionFacade.Http2PriorKnowledgeHandler.IsSome
                    "Http2PriorKnowledgeHandler must be set"
  ]

// ---------------------------------------------------------------------------
// Step 3c — h2c prior-knowledge end-to-end.
// Boots a real cleartext Suave server on a loopback port, sends the
// literal HTTP/2 connection preface over raw TCP, and confirms that the
// server responds with the HTTP/2 server preface (a SETTINGS frame). The
// test does not parse full HTTP/2 responses — it only verifies that the
// protocol switch happens (i.e. the server replies with HTTP/2 framing,
// not "HTTP/1.1 400 Bad Request").
// ---------------------------------------------------------------------------

[<Tests>]
let h2cPriorKnowledgeIntegrationTests (_ : SuaveConfig) =
  let getFreePort () =
    let l = new System.Net.Sockets.TcpListener(System.Net.IPAddress.Loopback, 0)
    l.Start()
    let port = (l.LocalEndpoint :?> System.Net.IPEndPoint).Port
    l.Stop()
    port

  let withHttpServer (body: int -> unit) =
    let port = getFreePort ()
    let cts = new System.Threading.CancellationTokenSource()
    let cfg =
      { defaultConfig with
          bindings = [ HttpBinding.create HTTP System.Net.IPAddress.Loopback (uint16 port) ]
          cancellationToken = cts.Token }
    let ready, serverTask =
      Web.startWebServerAsync cfg (Suave.Successful.OK "h2c prior-knowledge")
    try
      ready |> Async.RunSynchronously |> ignore
      body port
    finally
      cts.Cancel()
      try serverTask.Wait(System.TimeSpan.FromSeconds 5.0) |> ignore with _ -> ()

  testList "Http2 h2c prior-knowledge integration" [
    testCase "server responds to the HTTP/2 preface with a SETTINGS frame" <| fun _ ->
      withHttpServer (fun port ->
        use client = new System.Net.Sockets.TcpClient()
        client.Connect(System.Net.IPAddress.Loopback, port)
        let stream = client.GetStream()
        // Send the literal 24-byte HTTP/2 connection preface, then an empty
        // SETTINGS frame (9-byte header, zero payload) so the server has a
        // complete frame to read after its own SETTINGS.
        let preface = System.Text.Encoding.ASCII.GetBytes connectionPreface
        let emptySettings : byte[] =
          [| 0uy; 0uy; 0uy   // length = 0
             4uy             // type = SETTINGS
             0uy             // flags = 0
             0uy; 0uy; 0uy; 0uy |] // stream id = 0
        stream.Write(preface, 0, preface.Length)
        stream.Write(emptySettings, 0, emptySettings.Length)
        stream.Flush()

        // Read the first 9 bytes of the server's response — that's a frame
        // header. RFC 7540 §3.5 mandates the server's first frame after the
        // preface be SETTINGS (type = 4). Anything else (in particular, a
        // textual "HTTP/1.1 400…" response) means the protocol switch failed.
        let buf : byte[] = Array.zeroCreate 9
        stream.ReadTimeout <- 5000
        let mutable offset = 0
        while offset < buf.Length do
          let n = stream.Read(buf, offset, buf.Length - offset)
          if n <= 0 then failtest "server closed before sending its SETTINGS frame"
          offset <- offset + n
        Expect.equal buf.[3] 4uy
          "server's first frame must be SETTINGS (type=4) per RFC 7540 §3.5"
        Expect.equal buf.[4] 0uy
          "server's initial SETTINGS frame must not have the ACK flag set"
        // The stream identifier of any SETTINGS frame is 0 (RFC 7540 §6.5).
        Expect.equal buf.[5] 0uy "stream id high byte = 0"
        Expect.equal buf.[6] 0uy "stream id mid-hi byte = 0"
        Expect.equal buf.[7] 0uy "stream id mid-lo byte = 0"
        Expect.equal buf.[8] 0uy "stream id low byte = 0")
  ]

// ---------------------------------------------------------------------------
// Step 4 — ALPN h2 negotiation over TLS.
// Spins up a real HTTPS Suave server bound to an ephemeral port using the
// test certificate, then runs a `.NET` `SslStream` client that advertises
// ALPN and asserts the server selects "h2" when offered (and "http/1.1"
// otherwise). This validates that `SslTransport` advertises h2 first and
// honours the client's preference list (RFC 7301 / RFC 7540 §3.3).
// ---------------------------------------------------------------------------

[<Tests>]
let alpnTests (_ : SuaveConfig) =
  /// Build an in-memory self-signed certificate so the test does not depend
  /// on the (SHA-1, 1024-bit) `suave.p12` shipped for legacy compatibility,
  /// which modern OpenSSL rejects under default security levels.
  let loadTestCertificate () =
    use rsa = System.Security.Cryptography.RSA.Create(2048)
    let req =
      System.Security.Cryptography.X509Certificates.CertificateRequest(
        "CN=localhost",
        rsa,
        System.Security.Cryptography.HashAlgorithmName.SHA256,
        System.Security.Cryptography.RSASignaturePadding.Pkcs1)
    let sanBuilder =
      System.Security.Cryptography.X509Certificates.SubjectAlternativeNameBuilder()
    sanBuilder.AddIpAddress(System.Net.IPAddress.Loopback)
    sanBuilder.AddDnsName("localhost")
    req.CertificateExtensions.Add(sanBuilder.Build())
    let notBefore = System.DateTimeOffset.UtcNow.AddMinutes(-5.0)
    let notAfter = System.DateTimeOffset.UtcNow.AddHours(1.0)
    let cert = req.CreateSelfSigned(notBefore, notAfter)
    // Round-trip through PKCS#12 so the private key handle survives on every OS.
    let pfx = cert.Export(System.Security.Cryptography.X509Certificates.X509ContentType.Pkcs12)
    System.Security.Cryptography.X509Certificates.X509CertificateLoader
      .LoadPkcs12(pfx, (null : string))

  /// Grab a free localhost TCP port (0-binding then release).
  let getFreePort () =
    let l = new System.Net.Sockets.TcpListener(System.Net.IPAddress.Loopback, 0)
    l.Start()
    let port = (l.LocalEndpoint :?> System.Net.IPEndPoint).Port
    l.Stop()
    port

  let connectAndNegotiate (port: int) (offered: string list) =
    let protocols =
      System.Collections.Generic.List<System.Net.Security.SslApplicationProtocol>(
        offered |> List.map (fun p -> System.Net.Security.SslApplicationProtocol p))
    use client = new System.Net.Sockets.TcpClient()
    client.Connect(System.Net.IPAddress.Loopback, port)
    let netStream = client.GetStream()
    // Accept the self-signed test certificate.
    let validate =
      System.Net.Security.RemoteCertificateValidationCallback(fun _ _ _ _ -> true)
    use ssl = new System.Net.Security.SslStream(netStream, false, validate)
    let opts = System.Net.Security.SslClientAuthenticationOptions()
    opts.TargetHost <- "localhost"
    opts.ApplicationProtocols <- protocols
    opts.EnabledSslProtocols <-
      System.Security.Authentication.SslProtocols.Tls12 |||
      System.Security.Authentication.SslProtocols.Tls13
    let cts = new System.Threading.CancellationTokenSource(System.TimeSpan.FromSeconds 10.0)
    ssl.AuthenticateAsClientAsync(opts, cts.Token).GetAwaiter().GetResult()
    ssl.NegotiatedApplicationProtocol

  /// Boot an HTTPS Suave server on an ephemeral loopback port, invoke `body`
  /// with that port, and tear the server down deterministically.
  let withTlsServer (body: int -> unit) =
    let cert = loadTestCertificate ()
    let port = getFreePort ()
    let cts = new System.Threading.CancellationTokenSource()
    let cfg =
      { defaultConfig with
          bindings = [ HttpBinding.create (HTTPS cert) System.Net.IPAddress.Loopback (uint16 port) ]
          cancellationToken = cts.Token }
    let ready, serverTask =
      Web.startWebServerAsync cfg (Suave.Successful.OK "alpn test")
    try
      ready |> Async.RunSynchronously |> ignore
      body port
    finally
      cts.Cancel()
      try serverTask.Wait(System.TimeSpan.FromSeconds 5.0) |> ignore with _ -> ()

  testList "Http2 ALPN" [
    testCase "server selects h2 when client offers [h2, http/1.1]" <| fun _ ->
      withTlsServer (fun port ->
        let negotiated = connectAndNegotiate port [ "h2"; "http/1.1" ]
        Expect.equal negotiated
                     System.Net.Security.SslApplicationProtocol.Http2
                     "server must select h2 when client offers it")

    testCase "server falls back to http/1.1 when client offers only http/1.1" <| fun _ ->
      withTlsServer (fun port ->
        let negotiated = connectAndNegotiate port [ "http/1.1" ]
        Expect.equal negotiated
                     System.Net.Security.SslApplicationProtocol.Http11
                     "server must select http/1.1 when h2 is not offered")
  ]

// ---------------------------------------------------------------------------
// Step 5 — Trailers (RFC 7540 §8.1, RFC 9113 §8.1).
// Covers the pure validator (each rejection case) and the public WebPart
// combinators that record response trailers on the HttpContext.
// ---------------------------------------------------------------------------

/// Shared between the trailer and push test lists: run a WebPart on a fresh
/// empty context and assert that it succeeded.
let private runWebPartOnEmpty (part: WebPart) =
  let ctx = HttpContext.empty
  match Async.RunSynchronously (part ctx) with
  | Some c -> c
  | None -> failwith "WebPart returned None"

[<Tests>]
let trailersTests (_ : SuaveConfig) =
  let runPart = runWebPartOnEmpty

  testList "Http2 trailers" [
    testCase "validate accepts an empty list" <| fun _ ->
      Expect.equal (Http2.Trailers.validate []) (Ok ()) "no fields = trivially valid"

    testCase "validate accepts ordinary fields" <| fun _ ->
      let result = Http2.Trailers.validate [ "x-checksum", "abc"; "expires", "0" ]
      Expect.equal result (Ok ()) "non-pseudo, non-connection-specific fields are allowed"

    testCase "validate rejects pseudo-header fields (RFC 7540 §8.1.2.1)" <| fun _ ->
      let result = Http2.Trailers.validate [ ":status", "200" ]
      Expect.equal result (Result.Error ProtocolError)
                   "pseudo-header in trailer block is a PROTOCOL_ERROR"

    testCase "validate rejects an empty name" <| fun _ ->
      Expect.equal (Http2.Trailers.validate [ "", "x" ]) (Result.Error ProtocolError)
                   "empty field name is a PROTOCOL_ERROR"

    testCase "validate rejects connection-specific field 'connection'" <| fun _ ->
      let result = Http2.Trailers.validate [ "Connection", "close" ]
      Expect.equal result (Result.Error ProtocolError)
                   "RFC 7540 §8.1.2.2 forbids 'Connection' in any header block"

    testCase "validate rejects connection-specific field 'transfer-encoding'" <| fun _ ->
      let result = Http2.Trailers.validate [ "transfer-encoding", "chunked" ]
      Expect.equal result (Result.Error ProtocolError) "RFC 7540 §8.1.2.2"

    testCase "validate rejects connection-specific field 'keep-alive'" <| fun _ ->
      let result = Http2.Trailers.validate [ "keep-alive", "timeout=5" ]
      Expect.equal result (Result.Error ProtocolError) "RFC 7540 §8.1.2.2"

    testCase "validate accepts TE: trailers" <| fun _ ->
      Expect.equal (Http2.Trailers.validate [ "TE", "trailers" ]) (Ok ())
                   "RFC 7540 §8.1.2.2: TE: trailers is the only legal TE value"

    testCase "validate accepts TE: TRAILERS (case-insensitive)" <| fun _ ->
      Expect.equal (Http2.Trailers.validate [ "te", "TRAILERS" ]) (Ok ())
                   "TE value comparison is case-insensitive"

    testCase "validate rejects TE with any other value" <| fun _ ->
      Expect.equal (Http2.Trailers.validate [ "te", "gzip" ])
                   (Result.Error ProtocolError)
                   "TE: gzip is forbidden in HTTP/2"

    testCase "validate reports the first violation" <| fun _ ->
      // Connection-specific is hit before the pseudo-header — the validator
      // walks the list in order.
      let result = Http2.Trailers.validate [ "Connection", "close"; ":status", "200" ]
      Expect.equal result (Result.Error ProtocolError) "first failure short-circuits"

    testCase "set records a trailer on the response" <| fun _ ->
      let ctx = runPart (Http2.Trailers.set "x-checksum" "abc")
      Expect.equal (Http2.Trailers.get ctx)
                   [ "x-checksum", "abc" ]
                   "set stores the (name,value) pair on userState"

    testCase "set replaces an earlier value with the same name (case-insensitive)" <| fun _ ->
      let ctx =
        HttpContext.empty
        |> (fun c -> Async.RunSynchronously (Http2.Trailers.set "X-Checksum" "old" c))
        |> Option.get
        |> (fun c -> Async.RunSynchronously (Http2.Trailers.set "x-checksum" "new" c))
        |> Option.get
      Expect.equal (Http2.Trailers.get ctx) [ "x-checksum", "new" ]
                   "later set with the same name wins, comparison is case-insensitive"

    testCase "set preserves insertion order across distinct names" <| fun _ ->
      let ctx =
        HttpContext.empty
        |> (fun c -> Async.RunSynchronously (Http2.Trailers.set "x-first" "1" c))
        |> Option.get
        |> (fun c -> Async.RunSynchronously (Http2.Trailers.set "x-second" "2" c))
        |> Option.get
      Expect.equal (Http2.Trailers.get ctx)
                   [ "x-first", "1"; "x-second", "2" ]
                   "trailers are returned in the order they were set"

    testCase "setMany replaces the entire list" <| fun _ ->
      let initial =
        HttpContext.empty
        |> (fun c -> Async.RunSynchronously (Http2.Trailers.set "x-old" "v" c))
        |> Option.get
      let ctx = Async.RunSynchronously (Http2.Trailers.setMany [ "x-new", "v" ] initial)
      Expect.equal (Http2.Trailers.get (Option.get ctx))
                   [ "x-new", "v" ]
                   "setMany overwrites previously-recorded trailers"

    testCase "get on a fresh context returns the empty list" <| fun _ ->
      Expect.equal (Http2.Trailers.get HttpContext.empty) []
                   "no trailers recorded yet"

    testCase "getRequest on a fresh context returns the empty list" <| fun _ ->
      Expect.equal (Http2.Trailers.getRequest HttpContext.empty) []
                   "no request trailers received yet"
  ]

// ---------------------------------------------------------------------------
// Step 5 — Server push (RFC 7540 §8.2). Pure helpers + WebPart recording.
// The wire-format emission of PUSH_PROMISE is gated on the prerequisite
// dispatch loop and is therefore exercised by future end-to-end tests.
// ---------------------------------------------------------------------------

[<Tests>]
let pushTests (_ : SuaveConfig) =
  let runPart = runWebPartOnEmpty

  testList "Http2 server push" [
    testCase "canPush honours peer SETTINGS_ENABLE_PUSH=true (default)" <| fun _ ->
      Expect.isTrue (Http2.Push.canPush defaultSetting)
                    "RFC 7540 §6.5.2: SETTINGS_ENABLE_PUSH defaults to 1"

    testCase "canPush returns false when peer disabled push" <| fun _ ->
      let s = { defaultSetting with enablePush = false }
      Expect.isFalse (Http2.Push.canPush s)
                     "SETTINGS_ENABLE_PUSH=0 forbids server push"

    testCase "withinMaxConcurrentStreams is unbounded when peer advertises no limit" <| fun _ ->
      Expect.isTrue (Http2.Push.withinMaxConcurrentStreams 1000 None)
                    "no SETTINGS_MAX_CONCURRENT_STREAMS = unlimited"

    testCase "withinMaxConcurrentStreams permits one more when under the limit" <| fun _ ->
      Expect.isTrue (Http2.Push.withinMaxConcurrentStreams 5 (Some 10))
                    "5 active streams < limit 10 = may push"

    testCase "withinMaxConcurrentStreams refuses at the limit" <| fun _ ->
      Expect.isFalse (Http2.Push.withinMaxConcurrentStreams 10 (Some 10))
                     "10 active streams == limit 10 = MUST NOT push"

    testCase "withinMaxConcurrentStreams refuses beyond the limit" <| fun _ ->
      Expect.isFalse (Http2.Push.withinMaxConcurrentStreams 11 (Some 10))
                     "back-pressure must be respected"

    testCase "nextPromisedStreamId starts at 2 and is monotonic" <| fun _ ->
      let counter = ref 0
      Expect.equal (Http2.Push.nextPromisedStreamId counter) (Some 2)
                   "RFC 7540 §5.1.1: server-initiated streams use even ids; first is 2"
      Expect.equal (Http2.Push.nextPromisedStreamId counter) (Some 4)
                   "second allocation is 4"
      Expect.equal (Http2.Push.nextPromisedStreamId counter) (Some 6)
                   "third allocation is 6"

    testCase "nextPromisedStreamId reports overflow as None" <| fun _ ->
      // Set the counter close to the legal max so the next +2 step overflows.
      let counter = ref 0x7ffffffe
      Expect.equal (Http2.Push.nextPromisedStreamId counter) None
                   "stream id space exhausted => GOAWAY territory"

    testCase "push records a promise on the response" <| fun _ ->
      let ctx = runPart (Http2.Push.push "/style.css" [ "accept", "text/css" ])
      let promises = Http2.Push.get ctx
      Expect.equal (List.length promises) 1 "exactly one promise"
      Expect.equal promises.[0].path "/style.css" "path preserved"
      Expect.equal promises.[0].headers [ "accept", "text/css" ] "headers preserved"

    testCase "push appends multiple promises in order" <| fun _ ->
      let ctx =
        HttpContext.empty
        |> (fun c -> Async.RunSynchronously (Http2.Push.push "/a.css" [] c))
        |> Option.get
        |> (fun c -> Async.RunSynchronously (Http2.Push.push "/b.js" [] c))
        |> Option.get
      let paths = Http2.Push.get ctx |> List.map (fun p -> p.path)
      Expect.equal paths [ "/a.css"; "/b.js" ] "promises recorded in order"

    testCase "get on a fresh context returns the empty list" <| fun _ ->
      Expect.equal (Http2.Push.get HttpContext.empty) []
                   "no promises recorded yet"
  ]

// ---------------------------------------------------------------------------
// Frame-level validation tests (RFC 7540 §4.1, §4.2, §6.x — covers the
// invariants enforced by the read loop before any per-type decoder runs).
// ---------------------------------------------------------------------------

[<Tests>]
let frameValidationTests (_ : SuaveConfig) =
  let hdr (t: byte) (flags: byte) (sid: int32) (len: int) =
    { length = len; ``type`` = t; flags = flags; streamIdentifier = sid }
  let noIdle (_: int32) = false
  let allIdle (_: int32) = true
  let maxFrame = 16384l

  testList "Http2 frame validation" [

    testCase "oversized frame is a connection FRAME_SIZE_ERROR" <| fun _ ->
      // DATA frame whose payload exceeds SETTINGS_MAX_FRAME_SIZE.
      let h = hdr 0uy 0uy 1 (int maxFrame + 1)
      Expect.equal (validateFrame h h.length maxFrame noIdle)
                   (FvConnError FrameSizeError)
                   "length > SETTINGS_MAX_FRAME_SIZE → GOAWAY FRAME_SIZE_ERROR"

    testCase "unknown frame type is silently ignored" <| fun _ ->
      let h = hdr 0xFFuy 0uy 1 4
      Expect.equal (validateFrame h h.length maxFrame noIdle) FvIgnoreUnknown
                   "RFC 7540 §4.1/§5.5: unknown frames are discarded"

    testCase "PRIORITY on stream 0 is a connection PROTOCOL_ERROR" <| fun _ ->
      let h = hdr 2uy 0uy 0 5
      Expect.equal (validateFrame h h.length maxFrame noIdle)
                   (FvConnError ProtocolError) ""

    testCase "PRIORITY with length != 5 is a stream FRAME_SIZE_ERROR" <| fun _ ->
      let h = hdr 2uy 0uy 7 6
      Expect.equal (validateFrame h h.length maxFrame noIdle)
                   (FvStreamError (7, FrameSizeError)) ""

    testCase "RST_STREAM on stream 0 is a connection PROTOCOL_ERROR" <| fun _ ->
      let h = hdr 3uy 0uy 0 4
      Expect.equal (validateFrame h h.length maxFrame noIdle)
                   (FvConnError ProtocolError) ""

    testCase "RST_STREAM on an idle stream is a connection PROTOCOL_ERROR" <| fun _ ->
      let h = hdr 3uy 0uy 5 4
      Expect.equal (validateFrame h h.length maxFrame allIdle)
                   (FvConnError ProtocolError)
                   "RFC 7540 §6.4: RST_STREAM on idle → PROTOCOL_ERROR"

    testCase "RST_STREAM with length != 4 is a connection FRAME_SIZE_ERROR" <| fun _ ->
      let h = hdr 3uy 0uy 1 5
      Expect.equal (validateFrame h h.length maxFrame noIdle)
                   (FvConnError FrameSizeError) ""

    testCase "SETTINGS on non-zero stream is a connection PROTOCOL_ERROR" <| fun _ ->
      let h = hdr 4uy 0uy 1 0
      Expect.equal (validateFrame h h.length maxFrame noIdle)
                   (FvConnError ProtocolError) ""

    testCase "SETTINGS ACK with payload is a FRAME_SIZE_ERROR" <| fun _ ->
      let h = hdr 4uy 0x1uy 0 6
      Expect.equal (validateFrame h h.length maxFrame noIdle)
                   (FvConnError FrameSizeError) ""

    testCase "SETTINGS payload not a multiple of 6 is a FRAME_SIZE_ERROR" <| fun _ ->
      let h = hdr 4uy 0uy 0 7
      Expect.equal (validateFrame h h.length maxFrame noIdle)
                   (FvConnError FrameSizeError) ""

    testCase "PUSH_PROMISE from client is a connection PROTOCOL_ERROR" <| fun _ ->
      let h = hdr 5uy 0uy 1 12
      Expect.equal (validateFrame h h.length maxFrame noIdle)
                   (FvConnError ProtocolError) ""

    testCase "PING with length != 8 is a connection FRAME_SIZE_ERROR" <| fun _ ->
      let h = hdr 6uy 0uy 0 6
      Expect.equal (validateFrame h h.length maxFrame noIdle)
                   (FvConnError FrameSizeError) ""

    testCase "PING with non-zero stream is a connection PROTOCOL_ERROR" <| fun _ ->
      let h = hdr 6uy 0uy 1 8
      Expect.equal (validateFrame h h.length maxFrame noIdle)
                   (FvConnError ProtocolError) ""

    testCase "WINDOW_UPDATE with length != 4 is a FRAME_SIZE_ERROR" <| fun _ ->
      let h = hdr 8uy 0uy 0 3
      Expect.equal (validateFrame h h.length maxFrame noIdle)
                   (FvConnError FrameSizeError) ""

    testCase "DATA on stream 0 is a connection PROTOCOL_ERROR" <| fun _ ->
      let h = hdr 0uy 0uy 0 4
      Expect.equal (validateFrame h h.length maxFrame noIdle)
                   (FvConnError ProtocolError) ""

    testCase "valid HEADERS is accepted" <| fun _ ->
      let h = hdr 1uy 0x4uy 1 8
      Expect.equal (validateFrame h h.length maxFrame noIdle) FvAccept ""

    // Build a 6-byte SETTINGS entry (id, value) in network byte order.
    let entry (id: int) (value: uint32) =
      [| byte (id >>> 8); byte id
         byte (value >>> 24); byte (value >>> 16)
         byte (value >>> 8); byte value |]

    testCase "validateSettingsValues: ENABLE_PUSH = 0 is OK" <| fun _ ->
      Expect.equal (validateSettingsValues (entry 2 0u)) (Ok ()) ""

    testCase "validateSettingsValues: ENABLE_PUSH = 1 is OK" <| fun _ ->
      Expect.equal (validateSettingsValues (entry 2 1u)) (Ok ()) ""

    testCase "validateSettingsValues: ENABLE_PUSH = 2 is PROTOCOL_ERROR" <| fun _ ->
      Expect.equal (validateSettingsValues (entry 2 2u))
                   (Result.Error ProtocolError) ""

    testCase "validateSettingsValues: MAX_FRAME_SIZE < 2^14 is PROTOCOL_ERROR" <| fun _ ->
      Expect.equal (validateSettingsValues (entry 5 1u))
                   (Result.Error ProtocolError) ""

    testCase "validateSettingsValues: MAX_FRAME_SIZE > 2^24-1 is PROTOCOL_ERROR" <| fun _ ->
      Expect.equal (validateSettingsValues (entry 5 16777216u))
                   (Result.Error ProtocolError) ""

    testCase "validateSettingsValues: MAX_FRAME_SIZE at boundaries is OK" <| fun _ ->
      Expect.equal (validateSettingsValues (entry 5 16384u)) (Ok ()) "lower bound"
      Expect.equal (validateSettingsValues (entry 5 16777215u)) (Ok ()) "upper bound"

    testCase "validateSettingsValues: INITIAL_WINDOW_SIZE > 2^31-1 is FLOW_CONTROL_ERROR" <| fun _ ->
      Expect.equal (validateSettingsValues (entry 4 2147483648u))
                   (Result.Error FlowControlError) ""

    testCase "validateSettingsValues: unknown identifiers are ignored" <| fun _ ->
      // RFC 7540 §6.5.2: unknown identifiers MUST be ignored.
      Expect.equal (validateSettingsValues (entry 0xFF 12345u)) (Ok ()) ""
  ]
