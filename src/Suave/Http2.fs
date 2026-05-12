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
    | StreamOpen
    | HalfClosedLocal
    | HalfClosedRemote
    | StreamClosedState

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
      Ok StreamClosedState

    // Idle: only HEADERS or PUSH_PROMISE move us out.
    | Idle, RecvHeaders true  -> Ok HalfClosedRemote
    | Idle, RecvHeaders false -> Ok StreamOpen
    | Idle, SendHeaders true  -> Ok HalfClosedLocal
    | Idle, SendHeaders false -> Ok StreamOpen
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
    | StreamOpen, RecvHeaders true | StreamOpen, RecvData true -> Ok HalfClosedRemote
    | StreamOpen, SendHeaders true | StreamOpen, SendData true -> Ok HalfClosedLocal
    | StreamOpen, _ -> Ok StreamOpen

    // HalfClosedLocal: we've sent END_STREAM. We may still receive DATA/HEADERS.
    // Receiving END_STREAM closes the stream.
    | HalfClosedLocal, RecvHeaders true | HalfClosedLocal, RecvData true -> Ok StreamClosedState
    | HalfClosedLocal, RecvHeaders false | HalfClosedLocal, RecvData false -> Ok HalfClosedLocal
    | HalfClosedLocal, SendData _ | HalfClosedLocal, SendHeaders _ ->
      // We've already half-closed locally — sending more data/headers is illegal.
      Result.Error StreamClosed
    | HalfClosedLocal, _ -> Result.Error ProtocolError

    // HalfClosedRemote: peer has sent END_STREAM. We may continue to send.
    | HalfClosedRemote, SendHeaders true | HalfClosedRemote, SendData true -> Ok StreamClosedState
    | HalfClosedRemote, SendHeaders false | HalfClosedRemote, SendData false -> Ok HalfClosedRemote
    | HalfClosedRemote, RecvData _ | HalfClosedRemote, RecvHeaders _ ->
      // The peer promised END_STREAM and is now sending more. STREAM_CLOSED.
      Result.Error StreamClosed
    | HalfClosedRemote, _ -> Result.Error ProtocolError

    // Closed: receiving more is STREAM_CLOSED; sending more is a local bug.
    | StreamClosedState, RecvHeaders _ | StreamClosedState, RecvData _ ->
      Result.Error StreamClosed
    | StreamClosedState, _ ->
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
    if delta < 0 then
      Result.Error ProtocolError
    elif delta = 0 then
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

  type Message<'a> = Request of 'a | Stop

  type Http2Connection(facade: ConnectionFacade) =

    let alive = ref true
    let closeEvent = new ManualResetEvent(false)
    let writeQueue = new ConcurrentQueue<Frame>()

    let procQueue = new BlockingQueueAgent<Message<HttpRequest>>()

    member val encodeDynamicTable = newDynamicTableForEncoding defaultDynamicTableSize
    member val decodeDynamicTable = newDynamicTableForDecoding defaultDynamicTableSize 4096

    member x.read() : SocketOp<Frame> =
      readFrame facade

    member x.write(encInfo,p:FramePayload) : SocketOp<unit> =
      writeFrame encInfo p facade.Connection.transport

    member x.writeResponseToFrame (response: HttpResult) = async {
        let headersFrame = encodeHeader { algorithm = CompressionAlgorithm.Naive; useHuffman = true} 4096 x.encodeDynamicTable  ((":status",response.status.code.ToString())::response.headers)
        let encInfo = { flags = BitOperations.set 0uy 2; streamIdentifier = 11;  padding = None}
        let! _ = Async.AwaitTask ((x.write (encInfo, Headers(None, headersFrame))).AsTask())
        match response.content with
        | Bytes bs ->
          let encInfo = { flags = BitOperations.set 0uy 0; streamIdentifier = 11;  padding = None}
          let! _ = Async.AwaitTask ((x.write (encInfo, Data(bs,true))).AsTask())
          ()
        | _ -> ()
        return ()
        }

    member x.send (r:HttpRequest) =
      Async.Start (procQueue.AsyncAdd <| Request r)

    member x.stop() =
      Async.Start (procQueue.AsyncAdd <| Stop)
      closeEvent.WaitOne() |> ignore

    member x.get() =
      procQueue.AsyncGet()

    member x.writeLoop (ctxOuter : HttpContext) (webPart: WebPart) =
     async {
       // send an empty SETTINGS frame
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
