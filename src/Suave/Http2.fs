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

  type Settings = {
    headerTableSize : int
    ; enablePush : bool
    ; maxConcurrentStreams : int option
    ; initialWindowSize : int
    ; maxFrameSize : int
    ; maxHeaderBlockSize : int option
    }

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

  type FrameHeader = { 
    // the length field allows payloads of up to 224224 bytes (~16MB) per frame
    length : uint32;
    ``type`` : byte;
    flags : byte;
    streamIdentifier : uint32 }

  let get31Bit (data: byte []) =
    data.[3] <- data.[3] &&& 128uy
    BitConverter.ToUInt32 (ensureBigEndian data, 0)

  let readFrameHeader transport = socket{

      let! bytes = readBytes transport 9

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

      return { length = frameLength;
                ``type`` = frameType;
                flags = frameFlags;
                streamIdentifier = streamIdentifier}
    }

  type FramePayload =
    | Data of byte [] * bool
    | Headers of (uint32*byte) option * byte[]

  type PayloadDecoder = FrameHeader -> byte [] -> FramePayload

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

  let parseHeaders (header: FrameHeader) (payload: byte[]) =
    assert(header.``type`` = 1uy)
    let data = removePadding header payload
    let priority = header.flags &&& 0x20uy = 0x20uy

    if priority then
      let dependecyData = Array.zeroCreate 4
      Array.Copy(data, 0, dependecyData, 0, 4)
      let dependency = get31Bit dependecyData 
      let weight = data.[4]
      let headerBlockFragment = Array.zeroCreate (data.Length - 5)
      Headers (Some (dependency, weight), headerBlockFragment)
    else
      Headers (None, data)

  let payloadDecoders : PayloadDecoder [] = 
    [| parseData; parseHeaders |]

  let readFrame transport = socket {
    let! header = readFrameHeader transport
    let! payload = readBytes transport (int header.length)
    let payload = payloadDecoders.[int header.``type``] header payload
    return header, payload
    }