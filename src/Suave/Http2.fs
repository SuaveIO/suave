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

  type FrameHeader = { 
    // the length field allows payloads of up to 224224 bytes (~16MB) per frame
    length : uint32;
    ``type`` : byte;
    flags : byte;
    streamIdentifier : uint32 }

  type Settings = 
    { headerTableSize : uint32
    ; enablePush : bool
    ; maxConcurrentStreams : uint32 option
    ; initialWindowSize : uint32
    ; maxFrameSize : uint32
    ; maxHeaderBlockSize : uint32 option
    }

  let defaultSetting =
    { headerTableSize = 4096u
    ; enablePush = true
    ; maxConcurrentStreams = None
    ; initialWindowSize = 4096u
    ; maxFrameSize = 16384u
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
  open Suave.Sockets
  open Suave.Sockets.Control.SocketMonad

  let readBytes (transport : ITransport) (n : int) =
    let arr = Array.zeroCreate n
    let rec loop i = socket {
      if i = n then
        return arr
      else
        let! read = transport.read <| ArraySegment(arr,i,n - i)
        return! loop (i+read)
      }
    loop 0

  let ensureBigEndian (b : byte []) =
    if (BitConverter.IsLittleEndian) then
      Array.Reverse (b)
    b

  let get31Bit (data: byte []) =
    data.[3] <- data.[3] &&& 128uy
    BitConverter.ToUInt32 (ensureBigEndian data, 0)

  let parseFrameHeader (bytes : byte[]) =
    assert(bytes.Length = 9)
    let flen = Array.zeroCreate<byte> 4
    flen.[0] <- 0uy
    flen.[1] <- bytes.[0]
    flen.[2] <- bytes.[1]
    flen.[3] <- bytes.[2]

    let frameLength = BitConverter.ToUInt32 (ensureBigEndian flen, 0)

    let frameType  = bytes.[3]; // 4th byte in frame header is TYPE
    let frameFlags = bytes.[4]; // 5th byte is FLAGS

    let frameStreamIdData = Array.zeroCreate<byte> 4
    Array.Copy (bytes, 5, frameStreamIdData, 0, 4)

    // turn of most significant bit
    let streamIdentifier = get31Bit frameStreamIdData 

    { length = frameLength;
      ``type`` = frameType;
      flags = frameFlags;
      streamIdentifier = streamIdentifier }

  let encodeFrameHeader (header : FrameHeader) =

    let bytes = Array.zeroCreate<byte> 9

    let encodedLength = BitConverter.GetBytes header.length
    bytes.[0] <- encodedLength.[0]
    bytes.[1] <- encodedLength.[1]
    bytes.[2] <- encodedLength.[2]

    bytes.[3] <- header.``type``
    bytes.[4] <- header.flags

    let encodedStreamIdentifier = BitConverter.GetBytes header.streamIdentifier

    bytes.[5] <- encodedStreamIdentifier.[0]
    bytes.[6] <- encodedStreamIdentifier.[1]
    bytes.[7] <- encodedStreamIdentifier.[2]
    bytes.[8] <- encodedStreamIdentifier.[3]

    bytes

  let readFrameHeader transport = socket{
    let! bytes = readBytes transport 9
    return parseFrameHeader bytes
    }

  type Priority = 
    { exclusive : bool
    ; streamIdentifier : uint32
    ; weight: byte
  }

  type FramePayload =
    | Data of byte [] * bool
    | Headers of (Priority option * byte[])
    | Priority of Priority option
    | RstStream of ErrorCode
    | Settings of bool * Settings
    | PushPromise of uint32 * byte []
    | Ping of bool * byte []
    | GoAway of uint32 * ErrorCode * byte[]
    | WindowUpdate of uint32
    | Continuation of byte[]
    | Unknown of byte * byte[]

  type PayloadDecoder = FrameHeader -> byte [] -> FramePayload

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
      let dependency = get31Bit dependecyData 
      let weight = payload.[4]

      Some ({ exclusive = true; streamIdentifier = dependency;weight = weight})
    else
      None

  let parseHeaders (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 1uy)
    let data = removePadding header payload

    match priority header data with
    | Some p ->
      let headerBlockFragment = Array.zeroCreate (data.Length - 5)
      Array.Copy(data,headerBlockFragment,data.Length - 5)
      Headers (Some p, headerBlockFragment)
    | None ->
      Headers (None, data)

  let parsePriority (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 2uy)
    Priority (priority header payload)

  let parseRstStream (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 3uy)
    RstStream (toErrorCode (int payload.[0]))

  let parseSettings (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 4uy)
    // Receipt of a SETTINGS frame with the ACK flag set and a length field value other
    // than 0 MUST be treated as a connection error
    let ack = header.flags &&& 0x1uy = 0x1uy
    let settings = ref defaultSetting
    for i in 0 .. 6 .. payload.Length do
      let settingIdentifier = BitConverter.ToUInt16 (payload, i)
      let value = (BitConverter.ToUInt32 (payload, i + 2))
      settings :=
        match settingIdentifier with
        | 1us -> setHeaderTableSize !settings value
        | 2us -> setEnablePush !settings (value=1u)
        | 3us -> setMaxConcurrentStreams !settings (Some value)
        | 4us -> setInitialWindowSize !settings value
        | 5us -> setMaxFrameSize !settings value
        | 6us -> setMaxHeaderBlockSize !settings (Some value)
        | _ -> failwith "invalid setting identifier"

    Settings (ack, !settings)

  let parsePushPromise (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 5uy)
    let data = removePadding header payload
    let frameStreamIdData = Array.zeroCreate<byte> 4
    Array.Copy (data, 0, frameStreamIdData, 0, 4)
    // turn of most significant bit
    let streamIdentifier = get31Bit frameStreamIdData
    let headerBlockFragment = Array.zeroCreate (data.Length - 5)
    Array.Copy(data,headerBlockFragment,data.Length - 5)
    PushPromise (streamIdentifier,headerBlockFragment)

  let parsePing (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 6uy)
    assert(header.length = 8u)
    let ack = header.flags &&& 0x1uy = 0x1uy
    Ping (ack, payload)

  let parseGoAway (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 7uy)
    let idData = Array.zeroCreate<byte> 4
    Array.Copy (payload, 0, idData, 0, 4)
    let streamIdentifier = get31Bit idData
    Array.Copy (payload, 4, idData, 0, 4)
    let errorCode = BitConverter.ToUInt32 (ensureBigEndian idData, 0)
    GoAway (streamIdentifier,toErrorCode (int errorCode), Array.sub payload 8 (payload.Length - 8))

  let parseWindowUpdate (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 8uy)
    let idData = Array.zeroCreate<byte> 4
    Array.Copy (payload, 0, idData, 0, 4)
    let windowSizeIncrement = get31Bit idData
    WindowUpdate windowSizeIncrement

  let parseContinuation (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 8uy)
    Continuation payload

  let payloadDecoders : PayloadDecoder [] = 
    [| parseData; parseHeaders; parsePriority; parseRstStream; parseSettings; parsePushPromise; parsePing;
       parseGoAway; parseWindowUpdate; parseContinuation |]

  let readFrame transport = socket {
    let! header = readFrameHeader transport
    let! payload = readBytes transport (int header.length)
    let payload = payloadDecoders.[int header.``type``] header payload
    return header, payload
    }