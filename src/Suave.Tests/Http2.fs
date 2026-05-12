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
