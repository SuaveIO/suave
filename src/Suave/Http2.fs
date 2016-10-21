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

  let checkEndianness (b : byte []) =
    if (BitConverter.IsLittleEndian) then
      Array.Reverse b
    b

  let get31Bit (data: byte []) =
    data.[3] <- data.[3] &&& 128uy
    BitConverter.ToUInt32(data, 0)

  let get24BitBytes (value : UInt32) =
    let bytes = BitConverter.GetBytes value
    if BitConverter.IsLittleEndian then
      [| bytes.[2]; bytes.[1]; bytes.[0] |]
    else
      [| bytes.[0]; bytes.[1]; bytes.[2] |]

  let parseFrameHeader (bytes : byte[]) =
    assert(bytes.Length = 9)
    let flen = Array.zeroCreate<byte> 4
    flen.[0] <- 0uy
    flen.[1] <- bytes.[0]
    flen.[2] <- bytes.[1]
    flen.[3] <- bytes.[2]

    let frameLength = BitConverter.ToUInt32 (checkEndianness flen, 0)

    let frameType  = bytes.[3]; // 4th byte in frame header is TYPE
    let frameFlags = bytes.[4]; // 5th byte is FLAGS

    let frameStreamIdData = Array.zeroCreate<byte> 4
    Array.Copy (bytes, 5, frameStreamIdData, 0, 4)

    // turn of most significant bit
    let streamIdentifier = get31Bit (checkEndianness frameStreamIdData)

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

    let encodedStreamIdentifier = BitConverter.GetBytes header.streamIdentifier

    encodedStreamIdentifier.[3] <- encodedStreamIdentifier.[3] &&& 128uy

    let encodedStreamIdentifier = checkEndianness encodedStreamIdentifier

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
      let dependency = get31Bit (checkEndianness dependecyData)
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
    Console.WriteLine payload.Length
    if payload.Length > 0 then
      for i in 0 .. 6 .. payload.Length do
        Console.WriteLine i
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
    let streamIdentifier = get31Bit (checkEndianness frameStreamIdData)
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
    let streamIdentifier = get31Bit (checkEndianness idData)
    Array.Copy (payload, 4, idData, 0, 4)
    let errorCode = BitConverter.ToUInt32 (checkEndianness idData, 0)
    GoAway (streamIdentifier,toErrorCode (int errorCode), Array.sub payload 8 (payload.Length - 8))

  let parseWindowUpdate (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 8uy)
    let idData = Array.zeroCreate<byte> 4
    Array.Copy (payload, 0, idData, 0, 4)
    let windowSizeIncrement = get31Bit (checkEndianness idData)
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

  let writeFrameHeader (h: FrameHeader) (t:ITransport) = socket {
    let bytes = encodeFrameHeader h
    do! t.write(ByteSegment(bytes,0,bytes.Length))
    }

  let poke16 (arr : byte array) i w =
    arr.[i]     <- byte w
    arr.[i + 1] <- byte w >>> 8

  let poke32 (arr : byte array) i w =
    arr.[i]     <- byte w
    arr.[i + 1] <- byte w >>> 8
    arr.[i + 2] <- byte w >>> 0x10
    arr.[i + 3] <- byte w >>> 0x18

  let encodePriority priority =
    let arr = Array.zeroCreate<byte> 5
    poke32 arr 0 priority.streamIdentifier
    arr.[4] <- priority.weight - 1uy
    arr

  let encodeFramePayload = function
    | Data (bytes, flag) -> [ bytes ]
    | Headers(None, bytes) -> [ bytes ]
    | Headers(Some priority, bytes) -> 
      [ encodePriority priority ; bytes ]
    | Priority None -> [[||]]
    | Priority (Some priority) -> [encodePriority priority]
    | RstStream errorCode ->
      let b4 = Array.zeroCreate 4
      poke32 b4 0 (uint32 (fromErrorCode errorCode))
      [ b4 ]
    | Settings (flag,settings) ->
      [ 
        for settingIdentifier in [ 1 .. 6 ] do
          let arr = Array.zeroCreate<byte> 6
          poke16 arr 0 settingIdentifier
          match settingIdentifier  with
          | 1 ->
            poke32 arr 2 settings.headerTableSize
          | 2 ->
            if settings.enablePush then
              poke32 arr 2 1u
          | 3 ->
            match settings.maxConcurrentStreams with
            | Some v ->
              poke32 arr 2 v
            | None ->
              ()
          | 4 ->
            poke32 arr 2 settings.initialWindowSize
          | 5 ->
            poke32 arr 2 settings.maxFrameSize
          | 6 ->
            match settings.maxHeaderBlockSize with
            | Some v ->
              poke32 arr 2 v
            | None ->
              ()
          | _ ->
            failwith "invalid setting identifier"
          yield arr
      ]
    | PushPromise (i,bytes) -> 
      let b4 = Array.zeroCreate 4
      poke32 b4 0 i
      [ b4 ; bytes ]
    | Ping (flag,bytes) -> [ bytes] 
    | GoAway (i,errorCode,bytes) -> 
      let b8 = Array.zeroCreate 8
      poke32 b8 0 i
      poke32 b8 4 (uint32 (fromErrorCode errorCode))
      [ b8; bytes]
    | WindowUpdate w -> 
      let b4 = Array.zeroCreate 4
      poke32 b4 0 w
      [ b4 ]
    | Continuation bytes -> [bytes]

  let writeFrame (header,payload:FramePayload) (t:ITransport) = socket {
    do! writeFrameHeader header t
    for bytes in encodeFramePayload payload do
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

  let newDynamicRevIndex = 
    Array.map (fun _ -> Map.empty<HeaderValue, HIndex>) [| minTokenIx .. maxStaticTokenIndex |]

  let newOtherRevIndex = Map.empty

  let newrevIndex = RevIndex (newDynamicRevIndex, newOtherRevIndex)

  let newDynamicTableForEncoding (maxSize:int) =
    let info = EncodeInfo(newrevIndex, None)
    newDynamicTable maxSize info

  let newDynamicTableForDecoding maxsiz huftmpsiz =
    let buf = Array.zeroCreate huftmpsiz
    let decoder = decode buf huftmpsiz
    newDynamicTable maxsiz (DecodeInfo(decoder,maxsiz))


  type Http2Connection(transport: ITransport) =

    let writeQueue = new ConcurrentQueue<Frame>()

    member val encodeDynamicTable = newDynamicTableForEncoding defaultDynamicTableSize
    member val decodeDynamicTable = newDynamicTableForDecoding defaultDynamicTableSize 4096

    member x.read() : SocketOp<Frame> = 
      readFrame transport

    member x.write(f:Frame) : SocketOp<unit> = 
      writeFrame f transport
