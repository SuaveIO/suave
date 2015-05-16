namespace Suave

module WebSocket =

  open Suave.Sockets
  open Suave.Types
  open Suave.Http
  open Suave.Utils
  open Suave.Web

  open System
  open System.Security.Cryptography
  open System.Text

  let magicGUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

  let internal sha1 (x : string) =
    let crpto = new SHA1Managed()
    let bytes = Encoding.ASCII.GetBytes x
    let hash = crpto.ComputeHash bytes
    hash

  let handShakeResponse (handShakeToken : string) =
    Writers.setHeader "Upgrade" "websocket"
    >>= Writers.setHeader "Connection" "Upgrade"
    >>= Writers.setHeader "Sec-WebSocket-Accept" handShakeToken
    >>= Response.response HTTP_101 [||] // HTTP 1xx responses  MUST NOT contain a body

  type Opcode = Continuation | Text | Binary | Reserved | Close | Ping | Pong

  let toOpcode = function
    | 0uy -> Continuation
    | 1uy -> Text
    | 2uy -> Binary
    | 3uy | 4uy | 5uy | 6uy | 7uy -> Reserved
    | 8uy -> Close
    | 9uy -> Ping
    | 10uy -> Pong
    | 11uy | 12uy | 13uy | 14uy | 15uy -> Reserved
    | _ -> failwith "Invalid opcode."

  type CloseCode =
    | CLOSE_NORMAL
    | CLOSE_GOING_AWAY
    | CLOSE_PROTOCO_ERROR
    | CLOSE_UNSUPPORTED
    | CLOSE_NO_STATUS
    | CLOSE_ABNORMAL
    | CLOSE_TOO_LARGE
    member x.code = 
      match x with
      | CLOSE_NORMAL -> 1000
      | CLOSE_GOING_AWAY -> 1001
      | CLOSE_PROTOCO_ERROR -> 1002
      | CLOSE_UNSUPPORTED -> 1003
      | CLOSE_NO_STATUS -> 1005
      | CLOSE_ABNORMAL -> 1006
      | CLOSE_TOO_LARGE -> 1009

  type FrameHeader =
    { fin     : bool
      rsv1    : byte
      rsv2    : byte
      rsv3    : byte
      opcode  : Opcode
      hasMask : bool
      length  : int }

  let internal exctractHeader (arr: byte array) =
    let bytes x = arr.[x]
    let firstByte = bytes 0
    let secondByte = bytes 1
    let length = int <| if secondByte >= 128uy then secondByte - 128uy else secondByte
    let header =
      { fin  = firstByte &&& 128uy <> 0uy
        rsv1 = firstByte &&& 64uy
        rsv2 = firstByte &&& 32uy
        rsv3 = firstByte &&& 16uy
        opcode = firstByte &&& 15uy |> toOpcode
        hasMask = secondByte &&& 128uy <> 0uy
        length = length
      }
    header

  let internal calcOffset (header : FrameHeader) =
    let mutable offset = 2
    if header.hasMask then  offset <- offset + 4
    if header.length > 65535 then offset <- offset + 8
    elif header.length > 125 then offset <- offset + 2
    offset

  let internal frame (data : byte[]) (opcode : Opcode) (fin : bool) =

    let firstByte = 
      match opcode with
      | Continuation -> 0uy
      | Text -> 1uy
      | Binary -> 2uy
      | Close -> 8uy
      | Ping -> 9uy
      | Pong -> 10uy
      | _ -> failwith "Invalid opcode usage."

    let firstByte = if fin then firstByte ||| 128uy else firstByte

    let strLen = data.Length

    let secondByte =
      if strLen < 126 then byte strLen
      elif strLen < 65536 then 126uy
      else 127uy

    [| yield firstByte; yield secondByte; yield! data |]

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

  /// This class represents a WebSocket connection, it provides an interface to read and write to a WebSocket.
  type WebSocket(connection : Connection) =

    let readExtendedLength header = socket {
      if header.length = 126 then
          let! bytes = readBytes connection.transport 2
          return uint64(bytes.[0]) * 256UL + uint64(bytes.[1])
        elif header.length = 127 then
          let! bytes = readBytes connection.transport 8
          return uint64(bytes.[0]) * 65536UL * 65536UL * 65536UL * 256UL +
          uint64(bytes.[1]) * 65536UL * 65536UL * 65536UL +
          uint64(bytes.[2]) * 65536UL * 65536UL * 256UL +
          uint64(bytes.[3]) * 65536UL * 65536UL +
          uint64(bytes.[4]) * 65536UL * 256UL +
          uint64(bytes.[5]) * 65536UL +
          uint64(bytes.[6]) * 256UL +
          uint64(bytes.[7])
        else
          return uint64(header.length)
      }

    let sendFrame bs opcode fin = socket{
      let frame = frame opcode bs fin
      let! _ = connection.transport.write <| ArraySegment (frame,0,frame.Length)
      return ()
      }

    let readFrame () = socket {
      assert (List.length connection.segments = 0)
      let! arr = readBytes connection.transport 2
      let header = exctractHeader arr
      let! extendedLenght = readExtendedLength header

      assert(header.hasMask)
      let! mask = readBytes connection.transport 4

      if extendedLenght > uint64 Int32.MaxValue then
        let reason = sprintf "Frame size of %d bytes exceeds maximun accepted frame size (2 GB)" extendedLenght
        let data = 
          [| yield! BitConverter.GetBytes (CloseCode.CLOSE_TOO_LARGE.code)
           ; yield! UTF8.bytes reason |]
        do! sendFrame Close data true
        return! abort (OtherError reason)
      else
        let! frame = readBytes connection.transport (int extendedLenght)
        // Messages from the client MUST be masked
        let data = if header.hasMask then frame |> Array.mapi (fun i x -> x ^^^ mask.[i % 4]) else frame
        return (header.opcode, data, header.fin)
      }

    member this.read () = async{
      let! res = readFrame ()
      match res with
      | Choice1Of2 ((a,b,c) as x) -> return x
      | _ ->
        return failwith "WebSocket: read failed."
      }
    member this.send bs opcode fin = async{
      let frame = frame opcode bs fin
      let! res = connection.transport.write <| ArraySegment (frame,0,frame.Length)
      match res with
      | Choice1Of2 x -> return ()
      | _ ->
        return failwith "WebSocket: write failed."
      }

  let internal handShakeAux webSocketKey continuation (ctx : HttpContext) =
    async{
      let webSocketHash = sha1 <| webSocketKey + magicGUID
      let handShakeToken = Convert.ToBase64String webSocketHash
      let! something = ParsingAndControl.run ctx <| handShakeResponse handShakeToken
      match something with
      | Choice1Of2 _ ->
        let webSocket = new WebSocket(ctx.connection)
        return! continuation webSocket ctx
      | _ ->
        return failwith "handShakeAux: disconnected."
    }

  /// The handShake combinator captures a WebSocket and pass it to the provided `continuation`
  let handShake continuation (ctx : HttpContext) = async{
    let r = ctx.request
    if r.``method`` <> HttpMethod.GET then
      return! RequestErrors.METHOD_NOT_ALLOWED "Method not allowed" ctx
    elif r.header "upgrade"  <> Some "websocket" then 
      return! RequestErrors.BAD_REQUEST "Bad Request" ctx
    else
      match r.header "connection" with
      // rfc 6455 - Section 4.1.6 : The request MUST contain a |Connection| header field whose value
      // MUST include the "Upgrade" token.
      | Some str when str.Contains "Upgrade" -> 
        match r.header "sec-websocket-key" with
        | Some webSocketKey ->
          return! handShakeAux webSocketKey continuation ctx
        | _ ->
          return! RequestErrors.BAD_REQUEST "Missing 'sec-websocket-key' header" ctx
      | _ ->
        return! RequestErrors.BAD_REQUEST "Bad Request" ctx
      
    }