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

  type FrameHeader = { length : uint32;
                ``type`` : byte;
                flags : byte;
                streamIdentifier : uint32 }

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

      // we need to turn the stream id into a uint
      let frameStreamIdData = Array.zeroCreate<byte> 4
      Array.Copy (bytes, 5, frameStreamIdData, 0, 4)

      // turn of most significant bit
      frameStreamIdData.[3] <- frameStreamIdData.[3] &&& 128uy
      let streamIdentifier = BitConverter.ToUInt32 (ensureBigEndian frameStreamIdData, 0)

      return { length = frameLength;
                ``type`` = frameType;
                flags = frameFlags;
                streamIdentifier = streamIdentifier}
    }

  let readFrame transport = socket {
    let! header = readFrameHeader transport
    let! payload = readBytes transport (int header.length)
    return header, payload
    }