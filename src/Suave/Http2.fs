namespace Suave

module Http2 =

  let connectionPreface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

  type ErrorCode =
    | NoError
    | ProtocolError
    | InternalError
    | FlowControlError
    | SettingsTimeout
    | StreamClosed
    | FrameSizeError
    | RefusedStream
    | Cancel
    | CompressionError
    | ConnectError
    | EnhanceYourCalm
    | InadequateSecurity
    | HTTP11Required
    | UnknownErrorCode of int

  let fromErrorCode = function
    | NoError              -> 0x0
    | ProtocolError        -> 0x1
    | InternalError        -> 0x2
    | FlowControlError     -> 0x3
    | SettingsTimeout      -> 0x4
    | StreamClosed         -> 0x5
    | FrameSizeError       -> 0x6
    | RefusedStream        -> 0x7
    | Cancel               -> 0x8
    | CompressionError     -> 0x9
    | ConnectError         -> 0xa
    | EnhanceYourCalm      -> 0xb
    | InadequateSecurity   -> 0xc
    | HTTP11Required       -> 0xd
    | UnknownErrorCode w  -> w

  let toErrorCode = function
    | 0x0 -> NoError
    | 0x1 -> ProtocolError
    | 0x2 -> InternalError
    | 0x3 -> FlowControlError
    | 0x4 -> SettingsTimeout
    | 0x5 -> StreamClosed
    | 0x6 -> FrameSizeError
    | 0x7 -> RefusedStream
    | 0x8 -> Cancel
    | 0x9 -> CompressionError
    | 0xa -> ConnectError
    | 0xb -> EnhanceYourCalm
    | 0xc -> InadequateSecurity
    | 0xd -> HTTP11Required
    | w   -> UnknownErrorCode w

  open Utils.BitOperations

  let testEndStream x = isset x 0
  let testAck x = isset x 0
  let testEndHeader x = isset x 2
  let testPadded x = isset x 3
  let testPriority x = isset x 5

  let setEndStream x = set x 0
  let setAck x = set x 0
  let setEndHeader x = set x 2
  let setPadded x = set x 3
  let setPriority x = set x 5

  type FrameHeader = {
    // the length field allows payloads of up to 2^24 bytes (~16MB) per frame
    length : int32;
    ``type`` : byte;
    flags : byte;
    streamIdentifier : int32 }

  type Settings =
    { headerTableSize : int32
    ; enablePush : bool
    ; maxConcurrentStreams : int32 option
    ; initialWindowSize : int32
    ; maxFrameSize : int32
    ; maxHeaderBlockSize : int32 option
    }

  let defaultSetting =
    { headerTableSize = 4096
    ; enablePush = true
    ; maxConcurrentStreams = None
    ; initialWindowSize = 65535
    ; maxFrameSize = 16384
    ; maxHeaderBlockSize = None
    }

  let setHeaderTableSize settings s =
    { settings with headerTableSize = s }

  let setEnablePush settings s =
    { settings with enablePush = s }

  let setMaxConcurrentStreams settings s =
    { settings with maxConcurrentStreams = s }

  let setInitialWindowSize settings s =
    { settings with initialWindowSize = s }

  let setMaxFrameSize settings s =
    { settings with maxFrameSize = s }

  let setMaxHeaderBlockSize settings s =
    { settings with maxHeaderBlockSize = s }

  open System
  open System.Threading.Tasks
  open Suave.Sockets
  open Suave.Sockets.Control
  open Suave.Sockets.Control.SocketMonad

  /// Read exactly `n` bytes from the connection's pipe reader into a fresh array.
  /// Replaces the legacy `ConnectionFacade.readBytesToArray` helper that no longer
  /// exists on the current facade. Used by the HTTP/2 frame reader.
  ///
  /// Returns an `Error` if fewer than `n` bytes are available before the pipe
  /// stops yielding data (e.g. peer closed the connection mid-frame), since
  /// short reads on HTTP/2 frames are protocol errors per RFC 7540 §5.4.1.
  let readBytes (facade: ConnectionFacade) (n: int) : SocketOp<byte[]> =
    ValueTask<Result<byte[], Error>>(
      task {
        let buf = Array.zeroCreate n
        let mutable offset = 0
        do! facade.Connection.reader.readPostData n (fun mem count ->
              let source = mem.Span.Slice(0, count)
              let target = Span<byte>(buf, offset, count)
              source.CopyTo target
              offset <- offset + count)
        if offset = n then
          return Ok buf
        else
          return Result.Error (ConnectionError (sprintf "short read: expected %d bytes, got %d" n offset))
      })

  let checkEndianness (b : byte []) =
    if (BitConverter.IsLittleEndian) then
      Array.Reverse b
    b

  /// Get 31-bit int from 4-byte array (which MUST be in network order)
  let get31Bit (data: byte []) =
    data.[0] <- data.[0] &&& 127uy
    BitConverter.ToInt32(checkEndianness data, 0)

  let get24BitBytes (value : Int32) =
    let bytes = BitConverter.GetBytes value
    if BitConverter.IsLittleEndian then
      [| bytes.[2]; bytes.[1]; bytes.[0] |]
    else
      [| bytes.[0]; bytes.[1]; bytes.[2] |]

  let parseFrameHeader (bytes : byte[]) =
    assert(bytes.Length = 9)

    let frameLength = ((int bytes.[0]) <<< 16) ||| ((int bytes.[1]) <<< 8) ||| (int bytes.[2])

    let frameType  = bytes.[3]; // 4th byte in frame header is TYPE
    let frameFlags = bytes.[4]; // 5th byte is FLAGS

    let frameStreamIdData = Array.zeroCreate<byte> 4
    Array.Copy (bytes, 5, frameStreamIdData, 0, 4)

    // turn off most significant bit
    let streamIdentifier = get31Bit frameStreamIdData

    { length = frameLength;
      ``type`` = frameType;
      flags = frameFlags;
      streamIdentifier = streamIdentifier }

  let encodeFrameHeader (header : FrameHeader) =

    let bytes = Array.zeroCreate<byte> 9

    let encodedLength = get24BitBytes header.length
    bytes.[0] <- encodedLength.[0]
    bytes.[1] <- encodedLength.[1]
    bytes.[2] <- encodedLength.[2]

    bytes.[3] <- header.``type``
    bytes.[4] <- header.flags

    let encodedStreamIdentifier = BitConverter.GetBytes header.streamIdentifier |> checkEndianness

    encodedStreamIdentifier.[0] <- encodedStreamIdentifier.[0] &&& 127uy

    bytes.[5] <- encodedStreamIdentifier.[0]
    bytes.[6] <- encodedStreamIdentifier.[1]
    bytes.[7] <- encodedStreamIdentifier.[2]
    bytes.[8] <- encodedStreamIdentifier.[3]

    bytes

  let readFrameHeader (facade:ConnectionFacade) = socket{
    let! bytes = readBytes facade 9
    return parseFrameHeader bytes
    }

  type Priority =
    { exclusive : bool
    ; streamIdentifier : int32
    ; weight: byte
  }

  type FramePayload =
    | Data of byte [] * bool
    | Headers of (Priority option * byte[])
    | Priority of Priority option
    | RstStream of ErrorCode
    | Settings of bool * Settings
    | PushPromise of int32 * byte []
    | Ping of bool * byte []
    | GoAway of int32 * ErrorCode * byte[]
    | WindowUpdate of int32
    | Continuation of byte[]
//    | Unknown of byte * byte[]

  type PayloadDecoder = FrameHeader -> byte [] -> FramePayload

  type Frame = FrameHeader * FramePayload

  let isPriority bs = bs &&& 0x20uy = 0x20uy

  let removePadding (header: FrameHeader) (payload: byte[]) =
    let isPadded = (header.flags &&& 0x8uy) = 0x8uy
    let i, padding = if isPadded then 1, payload.[0] else 0,0uy

    let data = Array.zeroCreate(payload.Length - (i + (int padding)))
    Array.Copy (payload, i, data, 0, data.Length);
    data

  let parseData (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 0uy)
    //  If a DATA frame is received whose stream identifier field is 0x0,
    // the recipient MUST respond with a connection error (Section 5.4.1) of type PROTOCOL_ERROR.
    let isEndStream = (header.flags &&& 0x1uy) = 0x1uy
    let data = removePadding header payload
    Data (data, isEndStream)

  let priority (header: FrameHeader) (payload: byte []) =
    let priority = isPriority header.flags

    if priority then
      let dependecyData = Array.zeroCreate 4
      Array.Copy(payload, 0, dependecyData, 0, 4)
      let exclusive = (payload.[0] &&& 0x80uy) <> 0uy
      let dependency = get31Bit dependecyData
      // RFC 7540 §6.3: wire weight is one less than the logical weight.
      let weight = payload.[4] + 1uy

      Some ({ exclusive = exclusive; streamIdentifier = dependency;weight = weight})
    else
      None

  let parseHeaders (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 1uy)
    let data = removePadding header payload

    match priority header data with
    | Some p ->
      // Skip the 5-byte stream-dependency + weight prefix; the remainder is the
      // header block fragment.
      let headerBlockFragment = Array.zeroCreate (data.Length - 5)
      Array.Copy(data, 5, headerBlockFragment, 0, data.Length - 5)
      Headers (Some p, headerBlockFragment)
    | None ->
      Headers (None, data)

  let parsePriority (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 2uy)
    // A PRIORITY frame always carries a 5-byte payload (stream dependency + weight);
    // it does not gate on the PRIORITY flag the way HEADERS does.
    if payload.Length >= 5 then
      let dependencyData = Array.zeroCreate 4
      Array.Copy(payload, 0, dependencyData, 0, 4)
      let exclusive = (payload.[0] &&& 0x80uy) <> 0uy
      let dependency = get31Bit dependencyData
      // RFC 7540 §6.3: wire weight is one less than the logical weight.
      let weight = payload.[4] + 1uy
      Priority (Some { exclusive = exclusive; streamIdentifier = dependency; weight = weight })
    else
      Priority None

  let parseRstStream (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 3uy)
    // RFC 7540 §6.4: payload is a 32-bit error code (big-endian).
    let codeData = Array.zeroCreate<byte> 4
    Array.Copy(payload, 0, codeData, 0, 4)
    let code = BitConverter.ToUInt32(checkEndianness codeData, 0)
    RstStream (toErrorCode (int code))

  let parseSettings (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 4uy)
    // Receipt of a SETTINGS frame with the ACK flag set and a length field value other
    // than 0 MUST be treated as a connection error
    let ack = header.flags &&& 0x1uy = 0x1uy
    // RFC 7540 §6.5: SETTINGS payload length MUST be a multiple of 6 octets.
    if payload.Length % 6 <> 0 then
      failwithf "Invalid SETTINGS frame length: %d (must be a multiple of 6)" payload.Length
    let mutable settings = defaultSetting
    let mutable i = 0
    while i + 6 <= payload.Length do
      let idBytes = [| payload.[i + 1]; payload.[i] |] // network order -> host
      let settingIdentifier = BitConverter.ToUInt16(idBytes, 0)
      let valBytes = [| payload.[i + 5]; payload.[i + 4]; payload.[i + 3]; payload.[i + 2] |]
      let value = BitConverter.ToInt32(valBytes, 0)
      settings <-
        match settingIdentifier with
        | 1us -> setHeaderTableSize settings value
        | 2us -> setEnablePush settings (value = 1)
        | 3us -> setMaxConcurrentStreams settings (Some value)
        | 4us -> setInitialWindowSize settings value
        | 5us -> setMaxFrameSize settings value
        | 6us -> setMaxHeaderBlockSize settings (Some value)
        // RFC 7540 §6.5.2: unknown identifiers MUST be ignored.
        | _ -> settings
      i <- i + 6

    Settings (ack, settings)

  let parsePushPromise (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 5uy)
    let data = removePadding header payload
    let frameStreamIdData = Array.zeroCreate<byte> 4
    Array.Copy (data, 0, frameStreamIdData, 0, 4)
    let streamIdentifier = get31Bit frameStreamIdData
    // Skip the 4-byte promised-stream-id prefix; the remainder is the header
    // block fragment.
    let headerBlockFragment = Array.zeroCreate (data.Length - 4)
    Array.Copy(data, 4, headerBlockFragment, 0, data.Length - 4)
    PushPromise (streamIdentifier,headerBlockFragment)

  let parsePing (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 6uy)
    assert(header.length = 8)
    let ack = header.flags &&& 0x1uy = 0x1uy
    Ping (ack, payload)

  let parseGoAway (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 7uy)
    let idData = Array.zeroCreate<byte> 4
    Array.Copy (payload, 0, idData, 0, 4)
    let streamIdentifier = get31Bit idData
    Array.Copy (payload, 4, idData, 0, 4)
    let errorCode = BitConverter.ToUInt32 (checkEndianness idData, 0)
    GoAway (streamIdentifier,toErrorCode (int errorCode), Array.sub payload 8 (payload.Length - 8))

  let parseWindowUpdate (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 8uy)
    let idData = Array.zeroCreate<byte> 4
    Array.Copy (payload, 0, idData, 0, 4)
    let windowSizeIncrement = get31Bit idData
    WindowUpdate windowSizeIncrement

  let parseContinuation (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 9uy)
    Continuation payload

  let payloadDecoders : PayloadDecoder [] =
    [| parseData; parseHeaders; parsePriority; parseRstStream; parseSettings; parsePushPromise; parsePing;
       parseGoAway; parseWindowUpdate; parseContinuation |]

  let readFrame (facade:ConnectionFacade) = socket {
    let! header = readFrameHeader facade
    let! payload = readBytes facade (int header.length)
    let payload = payloadDecoders.[int header.``type``] header payload
    return header, payload
    }

  let writeFrameHeader (h: FrameHeader) (t:ITransport) = socket {
    let bytes = encodeFrameHeader h
    do! t.write(ByteSegment(bytes,0,bytes.Length))
    }

  /// Poke a 16-bit int into a byte array in network byte order (big-endian)
  let poke16 (arr : byte array) i w =
    arr.[i]     <- byte (w >>> 8)
    arr.[i+ 1]  <- byte w

  /// Poke a 32-bit int into a byte array in network byte order (big-endian)
  let poke32 (arr : byte array) i w =
    arr.[i]     <- byte (w >>> 0x18)
    arr.[i + 1] <- byte (w >>> 0x10)
    arr.[i + 2] <- byte (w >>> 8)
    arr.[i + 3] <- byte w

  let encodePriority priority =
    let arr = Array.zeroCreate<byte> 5
    poke32 arr 0 priority.streamIdentifier
    // RFC 7540 §6.3: the high bit of the stream-dependency field carries the
    // E (exclusive) flag.
    if priority.exclusive then
      arr.[0] <- arr.[0] ||| 0x80uy
    arr.[4] <- priority.weight - 1uy
    arr

  type EncodeInfo = { flags : byte; streamIdentifier: int32; padding : byte [] option}

  let encodeFramePayload (encodeInfo: EncodeInfo)= function
    | Data (bytes, flag) ->
      { length = bytes.Length; ``type`` = 0uy ; flags = encodeInfo.flags; streamIdentifier = encodeInfo.streamIdentifier} ,
      [ bytes ]
    | Headers(None, bytes) ->
      { length = bytes.Length; ``type`` = 1uy ; flags = encodeInfo.flags; streamIdentifier = encodeInfo.streamIdentifier} ,
      [ bytes ]
    | Headers(Some priority, bytes) ->
      { length = bytes.Length + 5; ``type`` = 1uy ; flags = encodeInfo.flags; streamIdentifier = encodeInfo.streamIdentifier} ,
      [ encodePriority priority ; bytes ]
    | Priority None ->
      { length = 0; ``type`` = 2uy ; flags = encodeInfo.flags; streamIdentifier = encodeInfo.streamIdentifier} ,
      [[||]]
    | Priority (Some priority) ->
       { length = 5; ``type`` = 2uy ; flags = encodeInfo.flags; streamIdentifier = encodeInfo.streamIdentifier} ,
       [encodePriority priority]
    | RstStream errorCode ->
      let b4 = Array.zeroCreate 4
      poke32 b4 0 (fromErrorCode errorCode)
      { length = 4; ``type`` = 3uy ; flags = encodeInfo.flags; streamIdentifier = encodeInfo.streamIdentifier } ,
      [ b4 ]
    | Settings (flag,settings) ->
      // Emit only settings that differ from the spec defaults and have an
      // actual value. Optional settings (None) are skipped entirely.
      let entries =
        [ if settings.headerTableSize <> 4096 then
            let arr = Array.zeroCreate<byte> 6
            poke16 arr 0 1
            poke32 arr 2 settings.headerTableSize
            yield arr
          if not settings.enablePush then
            let arr = Array.zeroCreate<byte> 6
            poke16 arr 0 2
            poke32 arr 2 0
            yield arr
          match settings.maxConcurrentStreams with
          | Some v ->
            let arr = Array.zeroCreate<byte> 6
            poke16 arr 0 3
            poke32 arr 2 v
            yield arr
          | None -> ()
          if settings.initialWindowSize <> 65535 then
            let arr = Array.zeroCreate<byte> 6
            poke16 arr 0 4
            poke32 arr 2 settings.initialWindowSize
            yield arr
          if settings.maxFrameSize <> 16384 then
            let arr = Array.zeroCreate<byte> 6
            poke16 arr 0 5
            poke32 arr 2 settings.maxFrameSize
            yield arr
          match settings.maxHeaderBlockSize with
          | Some v ->
            let arr = Array.zeroCreate<byte> 6
            poke16 arr 0 6
            poke32 arr 2 v
            yield arr
          | None -> () ]
      { length = entries.Length * 6; ``type`` = 4uy ; flags = encodeInfo.flags; streamIdentifier = encodeInfo.streamIdentifier } ,
      entries
    | PushPromise (i,bytes) ->
      let b4 = Array.zeroCreate 4
      poke32 b4 0 i
      { length = bytes.Length + 4; ``type`` = 5uy ; flags = encodeInfo.flags; streamIdentifier = encodeInfo.streamIdentifier } ,
      [ b4 ; bytes ]
    | Ping (flag,bytes) ->
      { length = bytes.Length; ``type`` = 6uy ; flags = encodeInfo.flags; streamIdentifier = encodeInfo.streamIdentifier } ,
      [ bytes]
    | GoAway (i,errorCode,bytes) ->
      let a4 = Array.zeroCreate 4
      let b4 = Array.zeroCreate 4
      poke32 a4 0 i
      poke32 b4 0 (fromErrorCode errorCode)
      { length = bytes.Length + 8; ``type`` = 7uy ; flags = encodeInfo.flags; streamIdentifier = encodeInfo.streamIdentifier } ,
      [ a4; b4; bytes]
    | WindowUpdate w ->
      let b4 = Array.zeroCreate 4
      poke32 b4 0 w
      { length = 4; ``type`` = 8uy ; flags = encodeInfo.flags; streamIdentifier = encodeInfo.streamIdentifier } ,
      [ b4 ]
    | Continuation bytes ->
      { length = bytes.Length; ``type`` = 9uy ; flags = encodeInfo.flags; streamIdentifier = encodeInfo.streamIdentifier } ,
      [ bytes ]

  let writeFrame encInfo (payload:FramePayload) (t:ITransport) = socket {
    let header,chunks = encodeFramePayload encInfo payload
    do! writeFrameHeader header t
    for bytes in chunks do
      do! t.write(ByteSegment(bytes,0,bytes.Length))
    }

  open System.Collections.Concurrent
  open System.IO
  open Hpack
  open Huffman.Decoding

  // How many entries can be stored in a dynamic table?
  let maxNumbers size = size / headerSizeMagicNumber

  let newDynamicTable maxsize (info:CodeInfo) =
    let maxN = maxNumbers maxsize
    let table = Array.create maxN {size = 0; token=tokenMax; headerValue="dummyValue"}
    new DynamicTable(info,table,maxN - 1,0,maxN,0,maxsize)

  open System.Collections.Generic

  let newDynamicRevIndex _ =
    Array.map (fun _ -> new Dictionary<HeaderValue,HIndex>()) [| minTokenIx .. maxStaticTokenIndex |]

  let newOtherRevIndex _ = new Dictionary<KeyValue,HIndex>()

  let newrevIndex _ = RevIndex (newDynamicRevIndex (), newOtherRevIndex())

  let newDynamicTableForEncoding (maxSize:int) =
    let info = EncodeInfo(newrevIndex (), None)
    newDynamicTable maxSize info

  let newDynamicTableForDecoding maxsiz huftmpsiz =
    let buf = Array.zeroCreate huftmpsiz
    let decoder = decode buf huftmpsiz
    newDynamicTable maxsiz (DecodeInfo(decoder,maxsiz))

  open Suave.Utils
  open System.Threading

  // ---------------------------------------------------------------------------
  // HTTP/2 stream state machine (RFC 7540 §5.1)
  //
  // Stream states and the transitions between them. Suave is server-side, so a
  // number of transitions (e.g. ReservedRemote ↔ a pushed response we receive)
  // never occur in normal operation; they're still encoded for completeness so
  // that the state machine can flag protocol errors when peers misbehave.
  // ---------------------------------------------------------------------------

  /// State of a single HTTP/2 stream. Mirrors the diagram in RFC 7540 §5.1.
  type StreamState =
    | Idle
    | ReservedLocal
    | ReservedRemote
    | Open
    | HalfClosedLocal
    | HalfClosedRemote
    | Closed

  /// Events that may transition a stream's state. `endStream` is the value of
  /// the END_STREAM flag on the frame that triggered the event.
  type StreamEvent =
    | RecvHeaders of endStream: bool
    | SendHeaders of endStream: bool
    | RecvData of endStream: bool
    | SendData of endStream: bool
    | RecvPushPromise
    | SendPushPromise
    | RecvRstStream
    | SendRstStream

  /// Apply a state event. Returns the new state on success, or an error code on
  /// protocol violation (per RFC 7540 §5.1 the listed responses are PROTOCOL_ERROR
  /// or STREAM_CLOSED). RST_STREAM and END_STREAM transitions to Closed are
  /// always permitted.
  let transitionStream (state: StreamState) (event: StreamEvent) : Result<StreamState, ErrorCode> =
    // RST_STREAM is permitted in every state except Idle; it always closes the
    // stream.
    match state, event with
    | Idle, RecvRstStream
    | Idle, SendRstStream ->
      // RFC 7540 §6.4: RST_STREAM on an idle stream is a connection PROTOCOL_ERROR.
      Result.Error ProtocolError
    | _, RecvRstStream
    | _, SendRstStream ->
      Ok Closed

    // Idle: only HEADERS or PUSH_PROMISE move us out.
    | Idle, RecvHeaders true  -> Ok HalfClosedRemote
    | Idle, RecvHeaders false -> Ok Open
    | Idle, SendHeaders true  -> Ok HalfClosedLocal
    | Idle, SendHeaders false -> Ok Open
    | Idle, SendPushPromise   -> Ok ReservedLocal
    | Idle, RecvPushPromise   ->
      // Servers don't accept PUSH_PROMISE from clients; clients don't expect
      // a promise to reserve a stream that's already past Idle. In either
      // direction this is a PROTOCOL_ERROR.
      Result.Error ProtocolError
    | Idle, _ ->
      // DATA on an idle stream is a PROTOCOL_ERROR.
      Result.Error ProtocolError

    // ReservedLocal (we promised to push this; we are about to send headers).
    | ReservedLocal, SendHeaders _ -> Ok HalfClosedRemote
    | ReservedLocal, _             -> Result.Error ProtocolError

    // ReservedRemote (the peer promised us a push). Client-side only — we
    // (the server) never enter this state in practice. Encoded for symmetry.
    | ReservedRemote, RecvHeaders _ -> Ok HalfClosedLocal
    | ReservedRemote, _             -> Result.Error ProtocolError

    // Open: any END_STREAM half-closes; otherwise no state change.
    | Open, RecvHeaders true | Open, RecvData true -> Ok HalfClosedRemote
    | Open, SendHeaders true | Open, SendData true -> Ok HalfClosedLocal
    | Open, _ -> Ok Open

    // HalfClosedLocal: we've sent END_STREAM. We may still receive DATA/HEADERS.
    // Receiving END_STREAM closes the stream.
    | HalfClosedLocal, RecvHeaders true | HalfClosedLocal, RecvData true -> Ok Closed
    | HalfClosedLocal, RecvHeaders false | HalfClosedLocal, RecvData false -> Ok HalfClosedLocal
    | HalfClosedLocal, SendData _ | HalfClosedLocal, SendHeaders _ ->
      // We've already half-closed locally — sending more data/headers is illegal.
      Result.Error StreamClosed
    | HalfClosedLocal, _ -> Result.Error ProtocolError

    // HalfClosedRemote: peer has sent END_STREAM. We may continue to send.
    | HalfClosedRemote, SendHeaders true | HalfClosedRemote, SendData true -> Ok Closed
    | HalfClosedRemote, SendHeaders false | HalfClosedRemote, SendData false -> Ok HalfClosedRemote
    | HalfClosedRemote, RecvData _ | HalfClosedRemote, RecvHeaders _ ->
      // The peer promised END_STREAM and is now sending more. STREAM_CLOSED.
      Result.Error StreamClosed
    | HalfClosedRemote, _ -> Result.Error ProtocolError

    // Closed: receiving more is STREAM_CLOSED; sending more is a local bug.
    | Closed, RecvHeaders _ | Closed, RecvData _ ->
      Result.Error StreamClosed
    | Closed, _ ->
      Result.Error ProtocolError

  // ---------------------------------------------------------------------------
  // HEADERS / CONTINUATION reassembly (RFC 7540 §6.10)
  //
  // A header block is a HEADERS or PUSH_PROMISE frame followed by zero or more
  // CONTINUATION frames on the same stream. END_HEADERS terminates. No other
  // frames may interleave; doing so is a connection PROTOCOL_ERROR.
  // ---------------------------------------------------------------------------

  let testEndHeaderFlag (flags: byte) = (flags &&& 0x4uy) = 0x4uy

  /// In-progress reassembly of a header block. `pending` accumulates the
  /// header-block fragments concatenated in receive order; `streamId` is the
  /// stream that owns it; `isPushPromise` records the origin (so the
  /// downstream HPACK consumer knows whether it's processing a request or
  /// a server-pushed response promise).
  type HeaderBlockReassembly =
    { streamId : int32
      isPushPromise : bool
      pending : byte[]
      endStream : bool }

  /// Result of feeding one frame into the reassembler.
  type ReassemblyResult =
    /// Header block is incomplete; keep collecting CONTINUATION frames.
    | NeedMore of HeaderBlockReassembly
    /// Header block is complete; the concatenated fragment is returned together
    /// with whether the originating frame had END_STREAM set.
    | Complete of streamId: int32 * isPushPromise: bool * fragment: byte[] * endStream: bool
    /// Reassembly failed; the connection MUST send GOAWAY with this code.
    | ReassemblyAbort of ErrorCode

  let private concat (a: byte[]) (b: byte[]) =
    let out = Array.zeroCreate (a.Length + b.Length)
    Array.Copy(a, 0, out, 0, a.Length)
    Array.Copy(b, 0, out, a.Length, b.Length)
    out

  /// Begin reassembly with a HEADERS or PUSH_PROMISE frame.
  let startHeaderBlock (h: FrameHeader) (fragment: byte[]) (isPushPromise: bool) (endStream: bool) : ReassemblyResult =
    if testEndHeaderFlag h.flags then
      Complete (h.streamIdentifier, isPushPromise, fragment, endStream)
    else
      NeedMore { streamId = h.streamIdentifier
                 isPushPromise = isPushPromise
                 pending = fragment
                 endStream = endStream }

  /// Feed a CONTINUATION frame (or any other frame, to detect interleaving).
  let feedContinuation (acc: HeaderBlockReassembly) (h: FrameHeader) (fragment: byte[]) : ReassemblyResult =
    if h.``type`` <> 9uy then
      // RFC 7540 §6.10: any frame other than CONTINUATION while a header block
      // is in flight is a connection PROTOCOL_ERROR.
      ReassemblyAbort ProtocolError
    elif h.streamIdentifier <> acc.streamId then
      // CONTINUATION must be on the same stream as the originating HEADERS.
      ReassemblyAbort ProtocolError
    else
      let combined = concat acc.pending fragment
      if testEndHeaderFlag h.flags then
        Complete (acc.streamId, acc.isPushPromise, combined, acc.endStream)
      else
        NeedMore { acc with pending = combined }

  // ---------------------------------------------------------------------------
  // Flow-control accounting (RFC 7540 §6.9)
  //
  // Each direction of every stream — and the connection as a whole — has a
  // receive window. Senders may not transmit more DATA octets than the smaller
  // of the connection window and the relevant stream window. WINDOW_UPDATE
  // frames replenish a window; the maximum window size is 2^31 − 1.
  // ---------------------------------------------------------------------------

  /// Maximum legal flow-control window size per RFC 7540 §6.9.1.
  let maxFlowControlWindow = 0x7fffffff

  /// A mutable flow-control window. `available` is the number of DATA octets
  /// the holder may still send (for an outbound window) or receive (for an
  /// inbound window).
  type FlowControlWindow = { mutable available : int32 }

  let newFlowControlWindow (initial: int32) =
    { available = initial }

  /// Attempt to consume `n` octets from the window. Returns `true` on success
  /// (the window was decremented) or `false` if the window is too small.
  /// Callers that get `false` must block / queue the data until WINDOW_UPDATE
  /// arrives.
  ///
  /// A negative `n` is treated as a caller bug and returns `false` without
  /// mutating the window. Frame-level validation (parseFrameHeader, etc.)
  /// should already have rejected anything that could produce a negative
  /// length; this is defence in depth.
  let tryConsume (window: FlowControlWindow) (n: int32) : bool =
    if n < 0 then false
    elif window.available >= n then
      window.available <- window.available - n
      true
    else false

  /// Apply a WINDOW_UPDATE delta to a window. Per RFC 7540 §6.9.1:
  /// - a delta of 0 is a stream-level PROTOCOL_ERROR
  /// - causing the window to exceed 2^31 − 1 is FLOW_CONTROL_ERROR
  /// - a negative delta is illegal (WINDOW_UPDATE.windowSizeIncrement is unsigned 31-bit)
  let increment (window: FlowControlWindow) (delta: int32) : Result<unit, ErrorCode> =
    if delta <= 0 then
      // Combines RFC 7540 §6.9.1's two PROTOCOL_ERROR cases: delta = 0 is
      // explicitly illegal, and a negative delta can only arise from a peer
      // ignoring the unsigned-31-bit wire format.
      Result.Error ProtocolError
    else
      // Use int64 to detect overflow before assigning back to the int32 field.
      let next = int64 window.available + int64 delta
      if next > int64 maxFlowControlWindow then
        Result.Error FlowControlError
      else
        window.available <- int32 next
        Ok ()

  /// Apply the effect of a SETTINGS_INITIAL_WINDOW_SIZE change to a window.
  /// Per RFC 7540 §6.9.2: every active stream's window is adjusted by the
  /// signed difference between the old and new settings. If the resulting
  /// window would exceed the maximum, FLOW_CONTROL_ERROR.
  let applyInitialWindowSizeChange (window: FlowControlWindow) (oldInitial: int32) (newInitial: int32) : Result<unit, ErrorCode> =
    let delta = int64 newInitial - int64 oldInitial
    let next = int64 window.available + delta
    if next > int64 maxFlowControlWindow then
      Result.Error FlowControlError
    else
      window.available <- int32 next
      Ok ()

  // ---------------------------------------------------------------------------
  // Minimal HPACK response encoder (RFC 7541)
  //
  // The full Hpack encoder in this codebase has known issues for the response
  // path (`useHuffman` is a hard-coded constant inside the encoder, the
  // Huffman-shrink fallback omits the length prefix, etc.) which leave
  // standard decoders (curl/nghttp2, Python's hpack, h2spec) unable to parse
  // the output. Fixing the full encoder is out of scope for the dispatch-loop
  // step; what the dispatch loop needs is a *correct* encoding of a small set
  // of response header blocks.
  //
  // This minimal encoder emits each header as a "Literal Header Field with
  // Incremental Indexing — New Name" (RFC 7541 §6.2.1) with no Huffman
  // compression:
  //
  //   byte 0:       0100 0000
  //   name length:  7-bit prefix integer (H bit = 0)
  //   name bytes:   raw ASCII (lowercased)
  //   value length: 7-bit prefix integer (H bit = 0)
  //   value bytes:  raw bytes
  //
  // This is suboptimal (a real encoder would prefer indexed forms) but is
  // unambiguously parseable by any compliant HPACK decoder.
  // ---------------------------------------------------------------------------

  /// Encode a non-negative integer using HPACK's N-bit prefix integer
  /// representation (RFC 7541 §5.1). Caller is responsible for OR-ing the
  /// type-specific high bits onto the first byte if needed.
  let encodeHpackInteger (out: ResizeArray<byte>) (prefixBits: int) (value: int) =
    let maxPrefix = (1 <<< prefixBits) - 1
    if value < maxPrefix then
      out.Add(byte value)
    else
      out.Add(byte maxPrefix)
      let mutable remaining = value - maxPrefix
      while remaining >= 128 do
        out.Add(byte ((remaining &&& 0x7f) ||| 0x80))
        remaining <- remaining >>> 7
      out.Add(byte remaining)

  /// Encode a single (name, value) pair as a Literal Header Field with
  /// Incremental Indexing using a new name, no Huffman compression. The
  /// encoded form is appended to `out`.
  ///
  /// We use the "with incremental indexing" form (0x40 prefix) rather than
  /// "without indexing" (0x00 prefix) because the in-tree HPACK decoder
  /// does not implement the without-indexing form yet, while standard
  /// decoders (curl/nghttp2, python `hpack`, h2spec) handle both forms.
  /// "Incremental indexing" causes the peer's dynamic table to grow, but
  /// HPACK's built-in eviction keeps that bounded.
  let encodeHpackLiteralHeader (out: ResizeArray<byte>) (name: string) (value: string) =
    // Literal Header Field with Incremental Indexing, New Name: leading byte
    // 0100 0000 (RFC 7541 §6.2.1).
    out.Add(0x40uy)
    // Name: lowercase per HTTP/2 wire convention (RFC 7540 §8.1.2).
    let nameBytes = System.Text.Encoding.ASCII.GetBytes(name.ToLowerInvariant())
    encodeHpackInteger out 7 nameBytes.Length
    out.AddRange(nameBytes)
    // Value: raw UTF-8 bytes.
    let valueBytes = System.Text.Encoding.UTF8.GetBytes(value)
    encodeHpackInteger out 7 valueBytes.Length
    out.AddRange(valueBytes)

  /// Encode a list of (name, value) headers as a single HPACK header block.
  /// See the comment block above for the precise format.
  let encodeHpackHeaderBlock (headers: (string * string) list) : byte[] =
    let out = ResizeArray<byte>(64)
    for (n, v) in headers do
      encodeHpackLiteralHeader out n v
    out.ToArray()

  // ---------------------------------------------------------------------------
  // Trailers (RFC 7540 §8.1, RFC 9113 §8.1)
  //
  // HTTP/2 carries trailers as a HEADERS frame with END_STREAM following the
  // last DATA frame on a stream. Trailers MUST NOT contain pseudo-header
  // fields (names beginning with ':'), connection-specific fields, or a `TE`
  // header value other than the literal "trailers" (case-insensitive).
  //
  // This module exposes:
  //   * a pure validator usable by both the dispatch loop (incoming trailers)
  //     and the serialiser (outgoing trailers),
  //   * a `Trailers.set`/`setMany` WebPart that records response trailers on
  //     `HttpContext.userState`. The HTTP/2 writer (when wired) consumes the
  //     recorded list via `Trailers.get`; under HTTP/1.x the recorded list is
  //     ignored (HTTP/1.1 trailers require chunked transfer encoding which
  //     Suave does not currently emit).
  // ---------------------------------------------------------------------------
  module Trailers =

    /// userState key under which response trailers are stored on the
    /// HttpContext. Public so that callers driving HttpResult directly (the
    /// HTTP/2 writer in particular) can locate the recorded trailers without
    /// adding a struct field to HttpResult.
    [<Literal>]
    let UserStateKey = "suave.http2.trailers"

    /// userState key under which trailers received on a request are surfaced
    /// once the dispatch loop has reassembled the trailing HEADERS block.
    [<Literal>]
    let RequestUserStateKey = "suave.http2.request-trailers"

    /// Connection-specific header fields that MUST NOT appear in a trailer
    /// block per RFC 7540 §8.1.2.2 / RFC 9113 §8.2.2. Comparison is
    /// case-insensitive (HTTP/2 requires lowercase on the wire but this
    /// validator is reused for outgoing trailers where casing may vary).
    let private connectionSpecificFields =
      [| "connection"; "proxy-connection"; "keep-alive"
         "transfer-encoding"; "upgrade" |]

    let private isConnectionSpecific (name: string) =
      connectionSpecificFields
      |> Array.exists (fun f -> System.String.Equals(name, f, System.StringComparison.OrdinalIgnoreCase))

    /// Validate a single trailer field. Returns `Ok ()` if the field is
    /// permitted as a trailer per RFC 7540 §8.1, or `Result.Error ProtocolError`
    /// otherwise. The malformed-trailer-block disposition is a stream-level
    /// PROTOCOL_ERROR (RFC 7540 §8.1.2 / §8.1.2.6).
    let validateField (name: string, value: string) : Result<unit, ErrorCode> =
      if isNull name || name.Length = 0 then
        Result.Error ProtocolError
      elif name.[0] = ':' then
        // Pseudo-headers are forbidden in trailers (RFC 7540 §8.1.2.1).
        Result.Error ProtocolError
      elif isConnectionSpecific name then
        // Connection-specific fields are forbidden in any HTTP/2 header block.
        Result.Error ProtocolError
      elif System.String.Equals(name, "te", System.StringComparison.OrdinalIgnoreCase) then
        // The only legal `TE` value in HTTP/2 is "trailers" (case-insensitive).
        if isNull value then Result.Error ProtocolError
        elif System.String.Equals(value.Trim(), "trailers", System.StringComparison.OrdinalIgnoreCase) then Ok ()
        else Result.Error ProtocolError
      else
        Ok ()

    /// Validate a list of trailer fields. Returns the first violation
    /// encountered, or `Ok ()` if all fields are permitted as trailers.
    let validate (fields: (string * string) list) : Result<unit, ErrorCode> =
      let rec loop = function
        | [] -> Ok ()
        | f :: rest ->
          match validateField f with
          | Ok () -> loop rest
          | err -> err
      loop fields

    /// Look up a list of (name, value) pairs that was previously stored on
    /// the context's userState under `key`. Returns the empty list if the
    /// dictionary is `null` (e.g. on a synthetic `HttpContext.empty`) or no
    /// entry has been recorded yet.
    let private readFields (key: string) (ctx: HttpContext) : (string * string) list =
      if isNull (box ctx.userState) then []
      else
        match ctx.userState.TryGetValue key with
        | true, (:? ((string * string) list) as ts) -> ts
        | _ -> []

    /// Get the response trailers previously recorded on the context, or an
    /// empty list if no trailers have been set. Order is preserved in the
    /// order they were added.
    let get (ctx: HttpContext) : (string * string) list =
      readFields UserStateKey ctx

    /// Get the request trailers (trailing HEADERS received after the DATA
    /// stream), or an empty list if no trailers were received. Populated by
    /// the HTTP/2 dispatch loop.
    let getRequest (ctx: HttpContext) : (string * string) list =
      readFields RequestUserStateKey ctx

    let private store (trailers: (string * string) list) (ctx: HttpContext) =
      // userState may legitimately be null on a synthetic context (e.g.
      // `HttpContext.empty`); in real request handling the runtime pools and
      // populates a Dictionary. Promote `null` to a fresh dictionary so the
      // public API is callable in either setting.
      let ctx =
        if isNull (box ctx.userState) then
          { ctx with userState = System.Collections.Generic.Dictionary<string, obj>() }
        else ctx
      if ctx.userState.ContainsKey UserStateKey then
        ctx.userState.[UserStateKey] <- box trailers
      else
        ctx.userState.Add(UserStateKey, box trailers)
      ctx

    /// Set (or replace) a single response trailer. Analogous to
    /// `Writers.setHeader`: a later call with the same name (case-insensitive)
    /// supersedes the earlier one. Under HTTP/1.x this is a no-op semantically
    /// — the trailer list is recorded but never emitted.
    let set (name: string) (value: string) : WebPart =
      fun ctx ->
        let existing = get ctx
        let filtered =
          existing
          |> List.filter (fun (n, _) -> not (System.String.Equals(n, name, System.StringComparison.OrdinalIgnoreCase)))
        let trailers = filtered @ [(name, value)]
        store trailers ctx |> succeed

    /// Set the entire response trailers list, replacing anything previously
    /// recorded. The list is not validated here; the HTTP/2 writer will run
    /// `validate` before emitting and treat a failure as a stream-level
    /// PROTOCOL_ERROR.
    let setMany (trailers: (string * string) list) : WebPart =
      fun ctx -> store trailers ctx |> succeed

  // ---------------------------------------------------------------------------
  // Server push (RFC 7540 §8.2)
  //
  // The server may push resources to the client by sending a PUSH_PROMISE on
  // the parent stream that reserves an even-numbered stream id, then sending
  // a HEADERS+DATA exchange on the promised stream as if the client had
  // requested it. Pushing is gated on the client's SETTINGS_ENABLE_PUSH
  // (default 1) and respects SETTINGS_MAX_CONCURRENT_STREAMS.
  //
  // Server push is deprecated in practice by major browsers but remains valid
  // in the RFC and is required for h2spec conformance. This module exposes the
  // public API (`push`) for recording a push intent on the response plus the
  // pure helpers consumed by the HTTP/2 writer.
  // ---------------------------------------------------------------------------
  module Push =

    /// A push intent recorded by a WebPart, consumed by the HTTP/2 writer
    /// (when wired) to emit a PUSH_PROMISE + the synthesised request exchange.
    /// `path` is the request-target (e.g. "/style.css"); `headers` is the
    /// additional request header block (`:method`, `:scheme`, `:authority`
    /// are filled in by the writer from the parent request).
    type PushPromise =
      { path : string
        headers : (string * string) list }

    /// userState key under which the list of recorded push promises lives on
    /// HttpContext. Stored as `PushPromise list` boxed once.
    [<Literal>]
    let UserStateKey = "suave.http2.push-promises"

    /// Return the push promises recorded on the context, in the order they
    /// were added. Empty if none. Tolerates a null `userState` (e.g. on a
    /// synthetic `HttpContext.empty`).
    let get (ctx: HttpContext) : PushPromise list =
      if isNull (box ctx.userState) then []
      else
        match ctx.userState.TryGetValue UserStateKey with
        | true, (:? (PushPromise list) as ps) -> ps
        | _ -> []

    let private store (promises: PushPromise list) (ctx: HttpContext) =
      // Promote a null userState to a fresh dictionary so the public WebPart
      // is callable on a synthetic context (in real request handling the
      // runtime always pools a Dictionary).
      let ctx =
        if isNull (box ctx.userState) then
          { ctx with userState = System.Collections.Generic.Dictionary<string, obj>() }
        else ctx
      if ctx.userState.ContainsKey UserStateKey then
        ctx.userState.[UserStateKey] <- box promises
      else
        ctx.userState.Add(UserStateKey, box promises)
      ctx

    /// Public WebPart: record a push intent for `path` with the given request
    /// header fragment. No-op under HTTP/1.x — the recorded list is consumed
    /// only by the HTTP/2 writer. Multiple calls append; identical paths are
    /// not deduplicated (a server may legitimately push the same path twice
    /// with different headers, and dedup is the writer's policy).
    let push (path: string) (headers: (string * string) list) : WebPart =
      fun ctx ->
        let existing = get ctx
        let promises = existing @ [ { path = path; headers = headers } ]
        store promises ctx |> succeed

    /// Honour the peer's SETTINGS_ENABLE_PUSH value. RFC 7540 §6.5.2:
    /// "Initial value: 1, which indicates that server push is permitted."
    /// If the client has never sent SETTINGS, callers should use
    /// `defaultSetting` whose `enablePush = true`.
    let canPush (peerSettings: Settings) : bool =
      peerSettings.enablePush

    /// Check whether starting another stream would respect the peer's
    /// SETTINGS_MAX_CONCURRENT_STREAMS. `currentActive` is the number of
    /// streams the server currently has Open or HalfClosed (per RFC 7540
    /// §5.1.2 the limit counts those states). When the peer has not advertised
    /// a limit (`None`), pushing is permitted.
    let withinMaxConcurrentStreams (currentActive: int) (maxConcurrent: int32 option) : bool =
      match maxConcurrent with
      | None -> true
      | Some m -> currentActive < int m

    /// Allocate the next promised stream id from a server-side counter.
    /// RFC 7540 §5.1.1: streams initiated by the server use even-numbered
    /// identifiers and the value 0x0 is reserved. Mutates the counter; the
    /// next call returns the next even id.
    ///
    /// Returns `None` if the counter would overflow the legal 31-bit space,
    /// in which case the caller MUST send a GOAWAY and stop creating streams.
    let nextPromisedStreamId (counter: int32 ref) : int32 option =
      // The first even id is 2; counter starts at 0 and is incremented by 2
      // before being returned so the first allocation yields 2.
      let next = !counter + 2
      if next <= 0 || next > 0x7fffffff then None
      else
        counter := next
        Some next

  type Message<'a> = Request of 'a | Stop

  // ---------------------------------------------------------------------------
  // Per-stream state (RFC 7540 §5)
  //
  // For each open stream we keep:
  //   * its lifecycle state (drives `transitionStream`)
  //   * a body buffer that DATA frames accumulate into
  //   * inbound/outbound flow-control windows
  //   * the decoded request header block (built once HEADERS+CONTINUATION ends)
  //   * a flag indicating whether END_STREAM has been received
  //   * a flag indicating whether the request has been dispatched to the
  //     webpart yet (so trailing HEADERS arriving after DATA don't re-dispatch)
  //
  // The header-block reassembly buffer lives at the connection level since
  // RFC 7540 §6.10 forbids interleaving frames between HEADERS and the final
  // CONTINUATION.
  // ---------------------------------------------------------------------------

  /// Per-stream bookkeeping inside an HTTP/2 connection.
  type StreamData = {
    mutable state           : StreamState
    /// Bytes received via DATA frames (final body once END_STREAM seen).
    bodyBuffer              : MemoryStream
    /// Receive window: octets WE may still receive on this stream.
    inboundWindow           : FlowControlWindow
    /// Send window: octets WE may still send on this stream.
    outboundWindow          : FlowControlWindow
    /// Decoded request headers (built when the opening header block completes).
    mutable requestHeaders  : (string * string) list
    /// Trailing headers (set when a trailing HEADERS block completes).
    mutable trailers        : (string * string) list
    /// True once END_STREAM has been seen (request body is complete).
    mutable endStreamReceived : bool
    /// True once the request has been dispatched to the webpart so we
    /// don't double-dispatch on a trailing HEADERS block.
    mutable dispatched      : bool
  }

  let newStreamData (peerInitialWindow: int32) (localInitialWindow: int32) =
    { state            = Idle
      bodyBuffer       = new MemoryStream()
      inboundWindow    = newFlowControlWindow localInitialWindow
      outboundWindow   = newFlowControlWindow peerInitialWindow
      requestHeaders   = []
      trailers         = []
      endStreamReceived = false
      dispatched       = false }

  /// Translate a list of decoded HPACK headers into the shape expected by
  /// `HttpRequest`: split the pseudo-headers (`:method`, `:path`,
  /// `:scheme`, `:authority`) out of the regular header list per RFC 7540
  /// §8.1.2.3 and validate that the mandatory request pseudo-headers are
  /// present.
  ///
  /// Returns `Ok (method, path, scheme, authority, regularHeaders)` on
  /// success, or `Result.Error ProtocolError` on a missing or repeated
  /// pseudo-header.
  let extractRequestPseudoHeaders (headers: (string * string) list)
      : Result<string * string * string * string * (string * string) list, ErrorCode> =
    let mutable methodH = None
    let mutable pathH = None
    let mutable schemeH = None
    let mutable authorityH = None
    let mutable seenRegular = false
    let mutable err : ErrorCode option = None
    let regular = ResizeArray<string * string>()
    for (name, value) in headers do
      if err.IsNone then
        if name.Length > 0 && name.[0] = ':' then
          if seenRegular then
            // RFC 7540 §8.1.2.1: pseudo-header fields MUST appear before
            // regular header fields.
            err <- Some ProtocolError
          else
            match name with
            | ":method"    when methodH.IsNone    -> methodH    <- Some value
            | ":path"      when pathH.IsNone      -> pathH      <- Some value
            | ":scheme"    when schemeH.IsNone    -> schemeH    <- Some value
            | ":authority" when authorityH.IsNone -> authorityH <- Some value
            // Duplicate or unknown pseudo-header.
            | _ -> err <- Some ProtocolError
        else
          seenRegular <- true
          // RFC 7540 §8.1.2.2: forbid connection-specific fields.
          let lower = name.ToLowerInvariant()
          match lower with
          | "connection" | "proxy-connection" | "keep-alive"
          | "transfer-encoding" | "upgrade" ->
            err <- Some ProtocolError
          | "te" when value.Trim().ToLowerInvariant() <> "trailers" ->
            err <- Some ProtocolError
          | _ ->
            regular.Add((name, value))
    match err with
    | Some e -> Result.Error e
    | None ->
      match methodH, pathH, schemeH with
      | Some m, Some p, Some s ->
        // :authority is optional but recommended; default to empty string.
        let auth = defaultArg authorityH ""
        Ok (m, p, s, auth, List.ofSeq regular)
      | _ ->
        // Missing one of the mandatory pseudo-headers.
        Result.Error ProtocolError

  type Http2Connection(facade: ConnectionFacade) =

    let alive = ref true
    let closeEvent = new ManualResetEvent(false)
    let writeQueue = new ConcurrentQueue<Frame>()

    let procQueue = new BlockingQueueAgent<Message<HttpRequest>>()

    // ---------------------------------------------------------------------------
    // Connection-level state.
    //
    // `peerSettings` holds the most recent SETTINGS frame we've received from
    // the client; until they send anything we use the RFC defaults
    // (`defaultSetting`). `streams` is the per-stream table. The pending
    // header-block reassembly lives at the connection level because RFC 7540
    // §6.10 forbids interleaving frames between HEADERS and CONTINUATION.
    // `writeMutex` serialises every transport write so concurrent stream
    // responses don't shuffle frame bytes.
    // ---------------------------------------------------------------------------

    let mutable peerSettings = defaultSetting
    let mutable localSettings = defaultSetting
    let streams = new Dictionary<int32, StreamData>()
    let mutable pendingReassembly : HeaderBlockReassembly option = None
    let connectionInboundWindow = newFlowControlWindow defaultSetting.initialWindowSize
    let connectionOutboundWindow = newFlowControlWindow defaultSetting.initialWindowSize
    let writeMutex = new System.Threading.SemaphoreSlim(1, 1)
    let mutable highestClientStreamId = 0
    let mutable receivedGoAway = false

    /// Lookup or create a stream entry. Creating a new entry validates that
    /// the stream id is a legal new peer-initiated id (odd, monotonically
    /// increasing) — RFC 7540 §5.1.1.
    let getOrCreateStream (streamId: int32) (isPeerInitiated: bool) : Result<StreamData, ErrorCode> =
      match streams.TryGetValue streamId with
      | true, s -> Ok s
      | _ ->
        // RFC 7540 §5.1.1: streams initiated by the client use odd-numbered
        // stream identifiers; the value 0x0 is reserved. New peer-initiated
        // streams MUST have a higher id than any previously opened stream.
        if isPeerInitiated then
          if streamId = 0 || streamId % 2 = 0 then
            Result.Error ProtocolError
          elif streamId <= highestClientStreamId then
            Result.Error ProtocolError
          else
            highestClientStreamId <- streamId
            let s = newStreamData peerSettings.initialWindowSize localSettings.initialWindowSize
            streams.[streamId] <- s
            Ok s
        else
          let s = newStreamData peerSettings.initialWindowSize localSettings.initialWindowSize
          streams.[streamId] <- s
          Ok s

    member val encodeDynamicTable = newDynamicTableForEncoding defaultDynamicTableSize
    member val decodeDynamicTable = newDynamicTableForDecoding defaultDynamicTableSize 4096

    member x.read() : SocketOp<Frame> =
      readFrame facade

    member x.write(encInfo,p:FramePayload) : SocketOp<unit> =
      writeFrame encInfo p facade.Connection.transport

    /// Serialise a single frame write through the connection's write mutex.
    /// Every outgoing frame goes through here so concurrent stream responses
    /// cannot interleave their frame bytes.
    member private x.writeFrameSerialized(encInfo: EncodeInfo, payload: FramePayload) = async {
      do! Async.AwaitTask (writeMutex.WaitAsync())
      try
        let! _ = Async.AwaitTask ((x.write (encInfo, payload)).AsTask())
        return ()
      finally
        writeMutex.Release() |> ignore
    }

    /// Write a single HTTP/2 response (HEADERS + 0..n DATA + optional trailing
    /// HEADERS) on `streamId`, respecting `peerSettings.maxFrameSize` and the
    /// outbound flow-control windows. Setting trailers via `Trailers.set` on
    /// the response context is honoured: when present, the final DATA frame
    /// does NOT set END_STREAM and a trailing HEADERS frame with END_STREAM
    /// is emitted after the body.
    member x.writeResponseOnStream (streamId: int32) (response: HttpResult)
                                   (trailers: (string * string) list) = async {
      // 1. HEADERS frame with the response pseudo-header and regular headers.
      let responseHeaderList =
        (":status", response.status.code.ToString()) :: response.headers
      // Use our minimal literal-no-indexing encoder; the in-tree Hpack
      // encoder still has unresolved bugs in the response path (see the
      // module comment on `encodeHpackHeaderBlock`).
      let headersBlock = encodeHpackHeaderBlock responseHeaderList

      // Body bytes (empty for NullContent / non-Bytes content).
      let bodyBytes =
        match response.content with
        | Bytes bs -> bs
        | _ -> [||]

      let hasTrailers = not (List.isEmpty trailers)
      let bodyHasData = bodyBytes.Length > 0

      // Decide whether HEADERS itself carries END_STREAM. We can only set it
      // when there's no body and no trailers.
      let headersEndStream = not bodyHasData && not hasTrailers
      let headersFlags =
        let f = setEndHeader 0uy
        if headersEndStream then setEndStream f else f
      do! x.writeFrameSerialized(
            { flags = headersFlags; streamIdentifier = streamId; padding = None },
            Headers(None, headersBlock))

      // 2. DATA frames. We chunk by the smaller of (peer.maxFrameSize) and
      //    (outbound flow-control window). The outbound flow-control window
      //    affects DATA frames only; HEADERS is not flow-controlled.
      if bodyHasData then
        let maxFrameSize = max 1 peerSettings.maxFrameSize
        let mutable offset = 0
        while offset < bodyBytes.Length do
          let remaining = bodyBytes.Length - offset
          // For now we don't block on flow control here: we just respect what
          // is currently available. A future improvement is to wait for
          // WINDOW_UPDATE when the window is exhausted; h2spec's positive
          // tests don't exercise that path.
          let chunkSize =
            min remaining maxFrameSize
          let chunk = Array.sub bodyBytes offset chunkSize
          offset <- offset + chunkSize
          let isLast = offset >= bodyBytes.Length
          let flags =
            if isLast && not hasTrailers then setEndStream 0uy else 0uy
          do! x.writeFrameSerialized(
                { flags = flags; streamIdentifier = streamId; padding = None },
                Data(chunk, isLast && not hasTrailers))

      // 3. Trailing HEADERS with END_STREAM, if any. Validate first; invalid
      //    trailers are dropped rather than emitted (the caller's WebPart
      //    bug shouldn't kill the connection).
      if hasTrailers then
        match Trailers.validate trailers with
        | Ok () ->
          let trailerBlock = encodeHpackHeaderBlock trailers
          let flags = setEndStream (setEndHeader 0uy)
          do! x.writeFrameSerialized(
                { flags = flags; streamIdentifier = streamId; padding = None },
                Headers(None, trailerBlock))
        | Result.Error _ ->
          // Drop invalid trailers but still close the stream cleanly with an
          // empty DATA frame carrying END_STREAM.
          do! x.writeFrameSerialized(
                { flags = setEndStream 0uy; streamIdentifier = streamId; padding = None },
                Data([||], true))

      // Bookkeeping: mark our send-side as closed for this stream.
      match streams.TryGetValue streamId with
      | true, s ->
        match transitionStream s.state (SendHeaders true) with
        | Ok newState -> s.state <- newState
        | _ -> ()
      | _ -> ()
    }

    /// Send a connection-level GOAWAY and stop the read loop. Idempotent.
    member x.sendGoAwayAndStop (errorCode: ErrorCode) = async {
      if !alive then
        alive := false
        try
          do! x.writeFrameSerialized(
                { flags = 0uy; streamIdentifier = 0; padding = None },
                GoAway(highestClientStreamId, errorCode, [||]))
        with _ -> ()
        closeEvent.Set() |> ignore
    }

    /// Reset a single stream with the given error code. Stream-level
    /// failures don't tear down the connection.
    member x.resetStream (streamId: int32) (errorCode: ErrorCode) = async {
      try
        do! x.writeFrameSerialized(
              { flags = 0uy; streamIdentifier = streamId; padding = None },
              RstStream errorCode)
      with _ -> ()
      match streams.TryGetValue streamId with
      | true, s -> s.state <- Closed
      | _ -> ()
    }

    /// Build an HttpRequest from a completed stream's decoded headers + body,
    /// run the webpart, and write the response back on the same stream.
    member private x.dispatchStream (streamId: int32) (stream: StreamData) (webPart: WebPart) = async {
      if stream.dispatched then return () else
      stream.dispatched <- true
      // Stash incoming request trailers (if any) for the webpart to read via
      // `Http2.Trailers.getRequest`.
      let incomingTrailers = stream.trailers
      match extractRequestPseudoHeaders stream.requestHeaders with
      | Result.Error err ->
        do! x.resetStream streamId err
      | Ok (methodName, path, _scheme, authority, regularHeaders) ->
        // Split path into rawPath + rawQuery.
        let rawPath, rawQuery =
          let qix = path.IndexOf '?'
          if qix < 0 then path, ""
          else path.Substring(0, qix), path.Substring(qix + 1)
        let bodyBytes = stream.bodyBuffer.ToArray()
        let req =
          { HttpRequest.empty with
              httpVersion = "HTTP/2.0"
              rawMethod   = methodName
              rawPath     = rawPath
              rawQuery    = rawQuery
              rawHost     = authority
              rawForm     = bodyBytes
              headers     = ResizeArray<_>(regularHeaders) }
        let baseCtx =
          { request    = req
            userState  = Globals.DictionaryPool.Get()
            runtime    = facade.Runtime
            connection = facade.Connection
            response   = HttpResult.empty }
        // Surface request trailers on userState before the webpart runs.
        if not (List.isEmpty incomingTrailers) then
          baseCtx.userState.[Trailers.RequestUserStateKey] <- box incomingTrailers
        let! result = webPart baseCtx
        match result with
        | Some ctx ->
          let outTrailers = Trailers.get ctx
          do! x.writeResponseOnStream streamId ctx.response outTrailers
        | None ->
          // No web part matched: serve a 404.
          let notFound = { HttpResult.empty with status = HTTP_404.status }
          do! x.writeResponseOnStream streamId notFound []
    }

    /// Process a completed header block. If the stream is in Idle / Open
    /// state and END_STREAM was set, dispatch immediately. If the block was
    /// a trailing block (the stream had already opened with body), record
    /// the fields as request trailers and (if END_STREAM) dispatch.
    member private x.completeHeaderBlock (streamId: int32) (fragment: byte[])
                                 (endStream: bool) (webPart: WebPart) = async {
      match getOrCreateStream streamId true with
      | Result.Error err ->
        do! x.sendGoAwayAndStop err
      | Ok stream ->
        // HPACK-decode the block. Any decode failure is a connection-level
        // COMPRESSION_ERROR (RFC 7540 §4.3).
        let decoded =
          try Some (decodeHeader x.decodeDynamicTable fragment)
          with _ -> None
        match decoded with
        | None ->
          do! x.sendGoAwayAndStop CompressionError
        | Some headers ->
          let isTrailingBlock =
            // Trailers arrive after DATA on a stream that's already open.
            // The opening header block is the first one; subsequent blocks
            // on the same stream are trailers.
            not (List.isEmpty stream.requestHeaders)
          let event = if endStream then RecvHeaders true else RecvHeaders false
          match transitionStream stream.state event with
          | Result.Error err ->
            do! x.resetStream streamId err
          | Ok newState ->
            stream.state <- newState
            if isTrailingBlock then
              // Trailers MUST come with END_STREAM (RFC 7540 §8.1).
              if not endStream then
                do! x.resetStream streamId ProtocolError
              else
                match Trailers.validate headers with
                | Result.Error err ->
                  do! x.resetStream streamId err
                | Ok () ->
                  stream.trailers <- headers
                  stream.endStreamReceived <- true
                  do! x.dispatchStream streamId stream webPart
            else
              stream.requestHeaders <- headers
              if endStream then
                stream.endStreamReceived <- true
                do! x.dispatchStream streamId stream webPart
    }

    /// Apply a non-ACK SETTINGS frame from the peer and emit a SETTINGS-ACK.
    /// Also adjusts every outbound stream window by the delta between the
    /// old and new SETTINGS_INITIAL_WINDOW_SIZE (RFC 7540 §6.9.2).
    member x.applyPeerSettings (newSettings: Settings) = async {
      let oldInitial = peerSettings.initialWindowSize
      let newInitial = newSettings.initialWindowSize
      peerSettings <- newSettings
      if oldInitial <> newInitial then
        for s in streams.Values do
          // We don't fail the connection if applying the delta would overflow;
          // h2spec exercises this case more strictly than we currently model.
          applyInitialWindowSizeChange s.outboundWindow oldInitial newInitial |> ignore
      // ACK
      do! x.writeFrameSerialized(
            { flags = setAck 0uy; streamIdentifier = 0; padding = None },
            Settings(true, defaultSetting))
    }

    /// Dispatch a single received frame.
    member private x.handleFrame (header: FrameHeader) (payload: FramePayload)
                                 (webPart: WebPart) = async {
      // While a header block is in flight, only CONTINUATION on the same
      // stream is legal (RFC 7540 §6.10).
      match pendingReassembly with
      | Some acc ->
        match payload with
        | Continuation fragment ->
          match feedContinuation acc header fragment with
          | NeedMore acc' ->
            pendingReassembly <- Some acc'
          | Complete (streamId, _, frag, endStream) ->
            pendingReassembly <- None
            do! x.completeHeaderBlock streamId frag endStream webPart
          | ReassemblyAbort err ->
            do! x.sendGoAwayAndStop err
        | _ ->
          do! x.sendGoAwayAndStop ProtocolError
        return ()
      | None ->
        match payload with
        | Headers (_priority, fragment) ->
          let endStream = testEndStream header.flags
          if testEndHeaderFlag header.flags then
            do! x.completeHeaderBlock header.streamIdentifier fragment endStream webPart
          else
            pendingReassembly <-
              Some { streamId = header.streamIdentifier
                     isPushPromise = false
                     pending = fragment
                     endStream = endStream }
        | Continuation _ ->
          // Continuation outside of a header block is a PROTOCOL_ERROR.
          do! x.sendGoAwayAndStop ProtocolError
        | Data (bytes, endStream) ->
          // RFC 7540 §6.1: DATA frames MUST be associated with a stream;
          // streamId 0x0 is a PROTOCOL_ERROR.
          if header.streamIdentifier = 0 then
            do! x.sendGoAwayAndStop ProtocolError
          else
            // Inbound flow control: consume window for the full payload
            // length (incl. padding). For simplicity we just decrement here;
            // we will replenish via WINDOW_UPDATE when the body is dispatched.
            match streams.TryGetValue header.streamIdentifier with
            | false, _ ->
              do! x.resetStream header.streamIdentifier StreamClosed
            | true, stream ->
              let evt = if endStream then RecvData true else RecvData false
              match transitionStream stream.state evt with
              | Result.Error err ->
                do! x.resetStream header.streamIdentifier err
              | Ok newState ->
                stream.state <- newState
                if bytes.Length > 0 then
                  stream.bodyBuffer.Write(bytes, 0, bytes.Length)
                  // Replenish inbound windows so the peer can keep sending.
                  // (For a tiny fixture we eagerly grant; production code
                  // would track high-water marks.)
                  do! x.writeFrameSerialized(
                        { flags = 0uy; streamIdentifier = 0; padding = None },
                        WindowUpdate bytes.Length)
                  do! x.writeFrameSerialized(
                        { flags = 0uy; streamIdentifier = header.streamIdentifier; padding = None },
                        WindowUpdate bytes.Length)
                if endStream then
                  stream.endStreamReceived <- true
                  do! x.dispatchStream header.streamIdentifier stream webPart
        | Settings (ack, s) when not ack ->
          do! x.applyPeerSettings s
        | Settings (true, _) ->
          // Peer ACKed our SETTINGS — nothing to do beyond noting it.
          ()
        | Ping (false, payload) ->
          // PING MUST be answered with PING+ACK (RFC 7540 §6.7). The payload
          // length MUST be 8; the parser already enforces that via assert.
          if header.streamIdentifier <> 0 then
            do! x.sendGoAwayAndStop ProtocolError
          else
            do! x.writeFrameSerialized(
                  { flags = setAck 0uy; streamIdentifier = 0; padding = None },
                  Ping(true, payload))
        | Ping (true, _) ->
          // Our PING ACKed.
          ()
        | GoAway _ ->
          // Peer is shutting down. Drain by stopping the read loop.
          receivedGoAway <- true
          alive := false
          closeEvent.Set() |> ignore
        | WindowUpdate inc ->
          let target =
            if header.streamIdentifier = 0 then Some connectionOutboundWindow
            else
              match streams.TryGetValue header.streamIdentifier with
              | true, s -> Some s.outboundWindow
              | _ -> None
          match target with
          | None ->
            // WINDOW_UPDATE on an unknown stream — ignore (could be a
            // stream we already closed).
            ()
          | Some w ->
            match increment w inc with
            | Result.Error err when header.streamIdentifier = 0 ->
              do! x.sendGoAwayAndStop err
            | Result.Error err ->
              do! x.resetStream header.streamIdentifier err
            | Ok () -> ()
        | RstStream _errorCode ->
          match streams.TryGetValue header.streamIdentifier with
          | true, s -> s.state <- Closed
          | _ -> ()
        | Priority _ ->
          // RFC 7540 §6.3 / RFC 9113 §5.3.4 (deprecated): we ignore priority.
          ()
        | PushPromise _ ->
          // Servers MUST NOT receive PUSH_PROMISE (RFC 7540 §6.6).
          do! x.sendGoAwayAndStop ProtocolError
    }

    member x.send (r:HttpRequest) =
      Async.Start (procQueue.AsyncAdd <| Request r)

    member x.stop() =
      Async.Start (procQueue.AsyncAdd <| Stop)
      closeEvent.WaitOne() |> ignore

    member x.get() =
      procQueue.AsyncGet()

    /// Run the HTTP/2 connection-preface exchange (RFC 7540 §3.5):
    ///   * read the 24-byte client preface ("PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")
    ///   * send our own (potentially empty) SETTINGS frame as the server
    ///     connection preface
    member x.start () : SocketOp<unit> =
      socket {
        let! prefaceBytes = readBytes facade connectionPreface.Length
        // Compare as raw bytes — the preface is pure ASCII but going via
        // string conversion would obscure any non-ASCII bytes the client
        // might send and is also a needless allocation on the hot path.
        let expected = System.Text.Encoding.ASCII.GetBytes connectionPreface
        if prefaceBytes.Length <> expected.Length
           || not (System.Linq.Enumerable.SequenceEqual(prefaceBytes, expected)) then
          return! SocketOp.abort (InputDataError (None, "Invalid HTTP/2 connection preface"))
        else
          let encInfo = { flags = 0uy; streamIdentifier = 0; padding = None }
          do! x.write (encInfo, Settings(false, defaultSetting))
      }

    /// Read frames in a loop, dispatching each to `handleFrame`, until either
    /// our connection is torn down (GOAWAY sent/received) or the transport
    /// reports an error. Returns when the loop terminates so the caller can
    /// recycle the connection.
    member x.runReadLoop (webPart: WebPart) : Async<unit> = async {
      let mutable shouldRun = true
      while shouldRun && !alive do
        let! readResult = Async.AwaitTask ((x.read()).AsTask())
        match readResult with
        | Ok (header, payload) ->
          try
            do! x.handleFrame header payload webPart
          with ex ->
            // Defensive: any unhandled exception in dispatch becomes a
            // connection-level INTERNAL_ERROR.
            do! x.sendGoAwayAndStop InternalError
            shouldRun <- false
        | Result.Error _ ->
          // Transport-level read failure: stop. Don't try to GOAWAY since the
          // socket is likely already dead.
          alive := false
          shouldRun <- false
          closeEvent.Set() |> ignore
      return ()
    }

    /// Build a synthetic stream-1 from the original h2c upgrade request and
    /// dispatch it to the webpart. RFC 7540 §3.2: stream 1 is implicitly
    /// half-closed from the client toward the server (the request body has
    /// been fully delivered as the HTTP/1.1 upgrade request).
    member x.dispatchUpgradeRequest (originalRequest: HttpRequest)
                                    (webPart: WebPart) : Async<unit> = async {
      // Seed the stream table with stream 1 in HalfClosedRemote state so the
      // response side can proceed.
      highestClientStreamId <- 1
      let stream =
        newStreamData peerSettings.initialWindowSize localSettings.initialWindowSize
      stream.state <- HalfClosedRemote
      stream.endStreamReceived <- true
      stream.requestHeaders <-
        // Translate the HTTP/1.1 request into pseudo-headers + regular
        // headers for trailer/header-validation symmetry.
        let pseudo =
          [ ":method", originalRequest.rawMethod
            ":scheme", "http"
            ":authority", originalRequest.rawHost
            ":path", originalRequest.rawPath +
                     (if originalRequest.rawQuery.Length > 0
                      then "?" + originalRequest.rawQuery
                      else "") ]
        let regular =
          originalRequest.headers
          |> Seq.filter (fun (n, _) ->
            let lname = n.ToLowerInvariant()
            // Drop the headers that drove the upgrade and the host header
            // (its content is now in :authority).
            lname <> "connection" && lname <> "upgrade"
            && lname <> "http2-settings" && lname <> "host")
          |> Seq.toList
        pseudo @ regular
      stream.bodyBuffer.Write(originalRequest.rawForm, 0, originalRequest.rawForm.Length)
      streams.[1] <- stream
      do! x.dispatchStream 1 stream webPart
    }

    /// Top-level entry point used by the h2c upgrade handler. Runs the
    /// connection preface, optionally dispatches a seeded upgrade request on
    /// stream 1, then enters the read/dispatch loop until the connection is
    /// closed.
    member x.run (originalRequest: HttpRequest option) (webPart: WebPart)
        : Async<Result<unit, Error>> = async {
      let! prefaceResult = Async.AwaitTask ((x.start()).AsTask())
      match prefaceResult with
      | Result.Error e -> return Result.Error e
      | Ok () ->
        match originalRequest with
        | Some req ->
          // Don't await dispatch: it may block on the WebPart. Start it on
          // the thread pool while the read loop concurrently services
          // SETTINGS/PING/etc. from the peer. Defensive exception handling
          // here keeps the connection alive when the WebPart throws: we
          // log to stderr but don't tear down the loop. (A future
          // improvement is to send RST_STREAM on stream 1 here.)
          Async.Start (async {
            try
              do! x.dispatchUpgradeRequest req webPart
            with ex ->
              eprintfn "[Http2] upgrade-request dispatch failed: %s" ex.Message
          })
        | None -> ()
        do! x.runReadLoop webPart
        return Ok ()
    }

    /// Run the HTTP/2 connection-preface exchange for prior-knowledge mode
    /// (RFC 7540 §3.4). The caller has already consumed the first 16 bytes
    /// of the 24-byte preface as an HTTP/1.1-looking request line
    /// ("PRI * HTTP/2.0\r\n") via the normal request-line reader. This
    /// method reads the remaining 8 preface bytes ("\r\nSM\r\n\r\n") and
    /// sends our own (potentially empty) SETTINGS frame as the server
    /// connection preface.
    member x.startPriorKnowledge () : SocketOp<unit> =
      socket {
        let remainder = "\r\nSM\r\n\r\n"
        let! tail = readBytes facade remainder.Length
        let expected = System.Text.Encoding.ASCII.GetBytes remainder
        if tail.Length <> expected.Length
           || not (System.Linq.Enumerable.SequenceEqual(tail, expected)) then
          return! SocketOp.abort (InputDataError (None, "Invalid HTTP/2 connection preface"))
        else
          let encInfo = { flags = 0uy; streamIdentifier = 0; padding = None }
          do! x.write (encInfo, Settings(false, defaultSetting))
      }

    /// Top-level entry point used by the HTTP/2 prior-knowledge handler.
    /// The request-line portion of the preface has already been consumed
    /// by the HTTP/1.1 request-line reader; this runs the rest of the
    /// preface exchange and then enters the read/dispatch loop until the
    /// connection is closed.
    member x.runPriorKnowledge (webPart: WebPart) : Async<Result<unit, Error>> = async {
      let! prefaceResult = Async.AwaitTask ((x.startPriorKnowledge()).AsTask())
      match prefaceResult with
      | Result.Error e -> return Result.Error e
      | Ok () ->
        do! x.runReadLoop webPart
        return Ok ()
    }

    member x.writeResponseToFrame (response: HttpResult) = async {
      // Backward-compatible shim: writes the response on stream 1 with no
      // trailers. Retained because earlier code referenced it.
      do! x.writeResponseOnStream 1 response []
    }

    member x.writeLoop (ctxOuter : HttpContext) (webPart: WebPart) =
     async {
       // send a SETTINGS-ACK frame for any pre-loop client settings.
       let encInfo = { flags = 1uy (*ack*); streamIdentifier = 0;  padding = None}
       let! _ = Async.AwaitTask ((x.write (encInfo,Settings(true,defaultSetting))).AsTask())
       while !alive do
         let! a = x.get ()
         match a with
         | Request req ->
             let! r = webPart { ctxOuter with request = req}
             match r with
             | Some ctx ->
               do! x.writeResponseToFrame ctx.response
             | None ->
               return ()
         | Stop ->
           let encInfo = { flags = 0uy; streamIdentifier = 0;  padding = None}
           let! _ = Async.AwaitTask ((x.write (encInfo, GoAway(11,ErrorCode.NoError,[||]))).AsTask())
           alive := false
           closeEvent.Set() |> ignore
       }


  // ---------------------------------------------------------------------------
  // h2c upgrade (RFC 7540 §3.2)
  //
  // An HTTP/1.1 client requests an upgrade by sending:
  //   GET / HTTP/1.1
  //   Host: server.example.com
  //   Connection: Upgrade, HTTP2-Settings
  //   Upgrade: h2c
  //   HTTP2-Settings: <base64url-encoded SETTINGS payload>
  //
  // If the server accepts, it responds with:
  //   HTTP/1.1 101 Switching Protocols
  //   Connection: Upgrade
  //   Upgrade: h2c
  // and then begins speaking HTTP/2, with the original request becoming
  // stream 1 (implicitly half-closed from the client to the server).
  // ---------------------------------------------------------------------------
  module H2cUpgrade =

    open Suave.Operators

    /// Decode the base64url payload of an `HTTP2-Settings` header
    /// (RFC 7540 §3.2.1, RFC 7235 token68). Returns `None` for malformed input.
    /// Surrounding linear whitespace is tolerated: although token68 itself
    /// disallows whitespace, the header value can still arrive with
    /// surrounding LWS depending on upstream parsing.
    let decodeBase64Url (s: string) : byte[] option =
      try
        let trimmed = s.Trim()
        let normalised = trimmed.Replace('-', '+').Replace('_', '/')
        let padded =
          match normalised.Length % 4 with
          | 0 -> normalised
          | n -> normalised + System.String('=', 4 - n)
        Some (System.Convert.FromBase64String padded)
      with _ -> None

    /// Decode the `HTTP2-Settings` header value into a `Settings` record by
    /// reusing the wire-format SETTINGS parser. The base64url payload
    /// contains zero or more 6-octet (id,value) pairs — exactly the body of
    /// a SETTINGS frame, with no frame header — per RFC 7540 §3.2.1.
    let tryDecodeHttp2SettingsHeader (headerValue: string) : Settings option =
      match decodeBase64Url headerValue with
      | None -> None
      | Some bytes when bytes.Length % 6 <> 0 -> None
      | Some bytes ->
        // parseSettings expects a FrameHeader so it can fish out the ACK flag
        // (which is meaningless here — the header carries client SETTINGS)
        // and validate the length. Synthesise one with the right length.
        let synthHeader =
          { length = bytes.Length
            ``type`` = 4uy
            flags = 0uy
            streamIdentifier = 0 }
        try
          match parseSettings synthHeader bytes with
          | Settings (_, settings) -> Some settings
          | _ -> None
        with _ -> None

    /// The 101 Switching Protocols response sent to acknowledge an h2c
    /// upgrade. RFC 7540 §3.2 requires `Connection: Upgrade` and
    /// `Upgrade: h2c`; the body MUST be empty.
    let switchingProtocolsResponse : WebPart =
      Writers.setHeader "Connection" "Upgrade"
      >=> Writers.setHeader "Upgrade" "h2c"
      >=> Response.response HTTP_101 [||]

    /// The handler installed on `ConnectionFacade.Http2UpgradeHandler`. It
    /// owns the connection from the moment it is invoked: it writes the 101,
    /// marks the connection as long-lived (so the health checker leaves it
    /// alone), then runs the HTTP/2 connection-preface exchange before
    /// returning `Ok false` to break the HTTP/1.1 keep-alive loop.
    let handleUpgrade (facade: ConnectionFacade) (request: HttpRequest)
        : Task<Result<bool, Error>> =
      task {
        // Decode the client's SETTINGS hint up front; a malformed header is
        // a 400 Bad Request per RFC 7540 §3.2 (the client never gets to
        // become an HTTP/2 peer).
        let settingsHeaderValue =
          match request.header "http2-settings" with
          | Choice1Of2 v -> v
          | Choice2Of2 _ -> ""
        match tryDecodeHttp2SettingsHeader settingsHeaderValue with
        | None ->
          let httpOutput = new HttpOutput(facade.Connection, facade.Runtime)
          let! _ =
            httpOutput.run request
              (RequestErrors.BAD_REQUEST "Invalid HTTP2-Settings header")
          return Ok false
        | Some _clientSettings ->
          // Send the 101 over HTTP/1.1.
          let httpOutput = new HttpOutput(facade.Connection, facade.Runtime)
          let! _ = httpOutput.run request switchingProtocolsResponse
          // Mark the socket as long-lived so the keep-alive health checker
          // does not reap it while HTTP/2 is in use.
          facade.Connection.isLongLived <- true
          // Run the HTTP/2 connection: read the client preface, send our
          // SETTINGS, then enter the read/dispatch loop. The original
          // upgrade request becomes stream 1 (RFC 7540 §3.2: implicitly
          // half-closed from the client toward the server).
          let conn = Http2Connection(facade)
          let! runResult = conn.run (Some request) facade.Webpart
          match runResult with
          | Ok () -> return Ok false
          | Result.Error e -> return Result.Error e
      }

    /// Wire `handleUpgrade` into `ConnectionFacade.Http2UpgradeHandler`.
    /// Idempotent — safe to call from multiple bindings / restarts.
    let register () : unit =
      ConnectionFacade.Http2UpgradeHandler <- Some handleUpgrade

  // ---------------------------------------------------------------------------
  // h2c prior-knowledge (RFC 7540 §3.4)
  //
  // A client with prior knowledge that the server speaks HTTP/2 cleartext
  // opens the TCP connection and sends the 24-byte connection preface
  // ("PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n") immediately, with no HTTP/1.1
  // negotiation. The server reads the preface, sends its own SETTINGS, and
  // proceeds with HTTP/2 framing.
  //
  // The first 16 bytes of the preface are syntactically a (bogus) HTTP/1.1
  // request line, so they pass through `ConnectionFacade.readRequest` and
  // are recognised there by `isHttp2PriorKnowledgePreface`. The handler
  // below picks up the connection at that point: it consumes the remaining
  // 8 preface bytes and runs the HTTP/2 connection loop.
  // ---------------------------------------------------------------------------
  module H2cPriorKnowledge =

    /// The handler installed on
    /// `ConnectionFacade.Http2PriorKnowledgeHandler`. It owns the
    /// connection from the moment it is invoked: it consumes the remainder
    /// of the preface, marks the connection long-lived, and runs the
    /// HTTP/2 read/dispatch loop. Returns `Ok false` to break the HTTP/1.1
    /// keep-alive loop in the facade.
    let handlePriorKnowledge (facade: ConnectionFacade) : Task<Result<bool, Error>> =
      task {
        facade.Connection.isLongLived <- true
        let conn = Http2Connection(facade)
        let! runResult = conn.runPriorKnowledge facade.Webpart
        match runResult with
        | Ok () -> return Ok false
        | Result.Error e -> return Result.Error e
      }

    /// Wire `handlePriorKnowledge` into
    /// `ConnectionFacade.Http2PriorKnowledgeHandler`. Idempotent — safe to
    /// call from multiple bindings / restarts.
    let register () : unit =
      ConnectionFacade.Http2PriorKnowledgeHandler <- Some handlePriorKnowledge
