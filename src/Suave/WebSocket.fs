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

  type FrameHeader =
    { fin     : bool
      rsv1    : byte
      rsv2    : byte
      rsv3    : byte
      opcode  : Opcode
      hasMask : bool
      length  : int
      mask    : byte[] }

  let internal exctractHeader (buffer: ByteSegment) n =
    let bytes x = buffer.Array.[buffer.Offset + x]
    let firstByte = bytes 0
    let secondByte = bytes 1
    let length = int <| if secondByte >= 129uy then secondByte - 128uy else secondByte
    let lenght, mask =
      if length = 126 then 
        int(bytes 2) * 256 + int(bytes 3), [| bytes 4; bytes 5; bytes 6; bytes 7|]
      elif length = 127 then
        int(bytes 2) * 65536 * 65536 * 65536 * 256 +
        int(bytes 3) * 65536 * 65536 * 65536 +
        int(bytes 4) * 65536 * 65536 * 256 +
        int(bytes 5) * 65536 * 65536 +
        int(bytes 6) * 65536 * 256 +
        int(bytes 7) * 65536 +
        int(bytes 8) * 256 +
        int(bytes 9), [| bytes 10; bytes 11; bytes 12; bytes 13|]
      else
        length, [| bytes 2; bytes 3; bytes 4; bytes 5|]
    let header =
      { fin  = firstByte &&& 128uy <> 0uy
        rsv1 = firstByte &&& 64uy
        rsv2 = firstByte &&& 32uy
        rsv3 = firstByte &&& 16uy
        opcode = firstByte &&& 15uy |> toOpcode
        hasMask = secondByte &&& 128uy <> 0uy
        length = length
        mask = mask
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

  /// This class represents a WebSocket connection, it provides an interface to read and write to a WebSocket.
  type WebSocket(connection : Connection) =
    member this.read () = async {
      assert (List.length connection.segments = 0)
      let bs = connection.bufferManager.PopBuffer()
      let! res = connection.transport.read bs
      match res with
      | Choice1Of2 x ->
        // TODO: we are asuming here the buffer can hold the entire frame
        let header = exctractHeader bs x
        let headerSize = calcOffset header
        let frameOffset = bs.Offset + headerSize
        let frame = [| for i = frameOffset to frameOffset + header.length - 1 do yield bs.Array.[i]|]
        // Messages from the client MUST be masked
        let data = if header.hasMask then frame |> Array.mapi (fun i x -> x ^^^ header.mask.[i % 4]) else frame
        connection.bufferManager.FreeBuffer bs
        return (header.opcode, data, header.fin)
      | _ ->
        connection.bufferManager.FreeBuffer bs
        return failwith "WebSocket: read failed."
      }
    member this.send bs opcode fin = async{
      let frame = frame opcode bs fin
      let! res = connection.transport.write <| ArraySegment (frame,0,frame.Length)
      match res with
      | Choice1Of2 x -> ()
      | _ ->
        failwith "WebSocket: write failed."
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
      | Some str when str.Contains "Upgrade" -> 
        match r.header "sec-websocket-key" with
        | Some webSocketKey ->
          return! handShakeAux webSocketKey continuation ctx
        | _ ->
          return! RequestErrors.BAD_REQUEST "Missing 'sec-websocket-key' header" ctx
      | _ ->
        return! RequestErrors.BAD_REQUEST "Bad Request" ctx
      
    }