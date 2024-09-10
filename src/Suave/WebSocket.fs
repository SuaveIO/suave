namespace Suave

module WebSocket =

  open Suave.Sockets
  open Suave.Sockets.Control
  open Suave.Operators
  open Suave.Utils
  open Suave.Utils.AsyncExtensions

  open System
  open System.Security.Cryptography
  open System.Text
  open System.Threading

  let magicGUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

  let internal sha1 (x : string) =
    let crpto = SHA1.Create()
    let bytes = Encoding.ASCII.GetBytes x
    let hash = crpto.ComputeHash bytes
    hash

  let handShakeResponse (handShakeToken : string) =
    Writers.setHeader "Upgrade" "websocket"
    >=> Writers.setHeader "Connection" "Upgrade"
    >=> Writers.setHeader "Sec-WebSocket-Accept" handShakeToken
    >=> Response.response HTTP_101 [||] // HTTP 1xx responses  MUST NOT contain a body

  let handShakeWithSubprotocolResponse (subprotocol : string) (handShakeToken : string) =
    Writers.setHeader "Sec-WebSocket-Protocol" subprotocol
    >=> handShakeResponse handShakeToken

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

  let fromOpcode = function
    | Continuation -> 0uy
    | Text -> 1uy
    | Binary -> 2uy
    | Close -> 8uy
    | Ping -> 9uy
    | Pong -> 10uy
    | _ -> failwith "Invalid opcode usage."

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

  type Frame = {
    OpcodeByte: byte
    EncodedLength: byte[]
    Data: ByteSegment
    }

  type ByteSegmentCapacityException(requiredCapacity: int, actualCapacity: int) =
    inherit Exception(
      "Byte segment provided to read websocket message does not have enough capacity [Required Capacity: " + requiredCapacity.ToString() + ", Actual Capacity: " + actualCapacity.ToString() + "]")

  let internal exctractHeader (arr: byte []) =
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

  let internal bytesToNetworkOrder (bytes : byte[]) =
    if BitConverter.IsLittleEndian then bytes |> Array.rev else bytes

  let writeFrame (connection: Connection) (f: Frame) = task {
      let! _ = connection.transport.write (Memory([| f.OpcodeByte |], 0, 1))
      let! _ = connection.transport.write (Memory(f.EncodedLength, 0, f.EncodedLength.Length))
      let! _ = connection.transport.write f.Data
      return Ok()
      }

  let internal frame (opcode : Opcode) (data : ByteSegment)  (fin : bool) =

    let firstByte = fromOpcode opcode

    let firstByte = if fin then firstByte ||| 128uy else firstByte

    let encodedLength =
        match data.Length with
        | l when l >= 65536 ->
          [| yield 127uy
             yield! BitConverter.GetBytes(uint64 data.Length) |> bytesToNetworkOrder |]
        | l when l >= 126 ->
          [| yield 126uy
             yield! BitConverter.GetBytes(uint16 data.Length) |> bytesToNetworkOrder |]
        | _ -> [| byte (data.Length) |]

    { Frame.OpcodeByte = firstByte; EncodedLength = encodedLength; Data = data }

  let readBytes (connection : Connection) (n : int) =
    task {
      let arr = Array.zeroCreate n
      let offset = ref 0
      let reader = connection.reader
      do! reader.readPostData n (fun a count ->
          let source = a.Span.Slice(0,count)
          let target = new Span<byte>(arr,!offset,count)
          source.CopyTo(target)
          offset := !offset + count)
      return Ok(arr)
    }

  let readBytesIntoByteSegment retrieveByteSegment (connection : Connection) (n : int) =
    let byteSegmentRetrieved: ByteSegment = retrieveByteSegment n

    let byteSegment =
        match byteSegmentRetrieved.Length with
        | count when count = n -> byteSegmentRetrieved
        | count when count < n -> raise (ByteSegmentCapacityException(n, byteSegmentRetrieved.Length))
        | _ -> byteSegmentRetrieved.Slice(0,n)

    let offset = ref 0
    let reader = connection.reader
    task{
      do! reader.readPostData n (fun a count ->
        let source = a.Span.Slice(0,count)
        let target = byteSegment.Span.Slice(!offset,count)
        source.CopyTo(target)
        offset := !offset + count)
      return Ok(byteSegment)
    }

  type internal Cont<'t> = 't -> unit

  let fork f = ThreadPool.QueueUserWorkItem(WaitCallback(f)) |> ignore

  /// This class represents a WebSocket connection, it provides an interface to read and write to a WebSocket.
  type WebSocket(connection : Connection, subprotocol: string option) =

    let readExtendedLength header = socket {
      if header.length = 126 then
          let! bytes = readBytes connection 2
          return uint64(bytes.[0]) * 256UL + uint64(bytes.[1])
        elif header.length = 127 then
          let! bytes = readBytes connection 8
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

    let writeSemaphore = new SemaphoreSlim(1, 1)
    let readSemaphore  = new SemaphoreSlim(1, 1)

    let releaseSemaphoreDisposable (semaphore: SemaphoreSlim) = { new IDisposable with member __.Dispose() = semaphore.Release() |> ignore }

    let runAsyncWithSemaphore semaphore asyncAction = task {
      use releaseSemaphoreDisposable = releaseSemaphoreDisposable semaphore
      do! semaphore.WaitAsync()
      return! asyncAction
      }

    let sendFrame opcode bs fin =
      let frame = frame opcode bs fin
      runAsyncWithSemaphore writeSemaphore (writeFrame connection frame)

    let readFrameIntoSegment (byteSegmentForLengthFunc: int -> ByteSegment) = socket {
      //assert (Seq.length connection.segments = 0)
      let! arr = readBytes connection 2
      let header = exctractHeader arr
      let! extendedLength = readExtendedLength header

      assert(header.hasMask)
      let! mask = readBytes connection 4

      if extendedLength > uint64 Int32.MaxValue then
        let reason = "Frame size of " + extendedLength.ToString() + " bytes exceeds maximum accepted frame size (2 GB)"
        let data =
            [| yield! BitConverter.GetBytes (CloseCode.CLOSE_TOO_LARGE.code) |> bytesToNetworkOrder
               yield! Encoding.UTF8.GetBytes reason |]
        do! sendFrame Close (Memory(data)) true
        return! SocketOp.abort (InputDataError(None, reason))
      else
        let! frame = readBytesIntoByteSegment byteSegmentForLengthFunc connection (int extendedLength)

        // Messages from the client MUST be masked
        if header.hasMask
        then
          for i = 0 to (int extendedLength) - 1 do
            frame.Span.[i] <- frame.Span.[i] ^^^ mask.[i % 4]

        return header.opcode, frame, header.fin
      }

    member this.read () = socket {
      return! runAsyncWithSemaphore readSemaphore (readFrameIntoSegment (Array.zeroCreate >> Memory))
      }

    /// Reads from the websocket and puts the data into a ByteSegment selected by the byteSegmentForLengthFunc parameter
    /// <param name="byteSegmentForLengthFunc">A function that takes in the message length in bytes required to hold the next websocket message and returns an appropriately sized ArraySegment of bytes</param>
    member this.readIntoByteSegment byteSegmentForLengthFunc = runAsyncWithSemaphore readSemaphore (readFrameIntoSegment byteSegmentForLengthFunc)

    member this.send opcode bs fin = sendFrame opcode bs fin

    member this.subprotocol = subprotocol

  let internal handShakeAux webSocketProtocol webSocketKey continuation (ctx : HttpContext) =
    socket {
      let webSocketHash = sha1 <| webSocketKey + magicGUID
      let handShakeToken = Convert.ToBase64String webSocketHash
      match webSocketProtocol with
      | Some subprotocol ->
        let! a = (new HttpOutput(ctx.connection,ctx.runtime)).run ctx.request (handShakeWithSubprotocolResponse subprotocol handShakeToken)
        ()
      | None ->
        let! a = (new HttpOutput(ctx.connection,ctx.runtime)).run ctx.request (handShakeResponse handShakeToken)
        ()
      let webSocket = new WebSocket(ctx.connection, webSocketProtocol)
      do! continuation webSocket ctx
    }

  let chooseSubprotocol (subprotocol : string) (requestSubprotocols : string []) (ctx : HttpContext) = async {
    if Array.exists ((=) subprotocol) requestSubprotocols then
      return Some subprotocol
    else
      return None
  }

  let validateConnectionHeader (header:Choice<string,string>) =
    match header with
    | Choice1Of2 s ->
      let parts =
        s.ToLower().Split([|','|],StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun (s:string) -> s.Trim())
      Array.contains "upgrade" parts
    | _ ->
      false

  let validateHandShake (ctx : HttpContext) =
    let r = ctx.request
    if r.``method`` <> HttpMethod.GET then
      Choice2Of2 (RequestErrors.METHOD_NOT_ALLOWED "Method not allowed" ctx)
    elif r.header "upgrade" |> Choice.map (fun s -> s.ToLower()) <> Choice1Of2 "websocket" then
      Choice2Of2 (RequestErrors.BAD_REQUEST "Bad Request" ctx)
    else
      if validateConnectionHeader (r.header "connection") then
        match r.header "sec-websocket-key" with
        | Choice1Of2 webSocketKey -> Choice1Of2 webSocketKey
        | _ -> Choice2Of2 (RequestErrors.BAD_REQUEST "Missing 'sec-websocket-key' header" ctx)
      else
        Choice2Of2 (RequestErrors.BAD_REQUEST "Bad Request" ctx)

  /// The handShakeWithSubprotocol combinator captures a WebSocket and pass it to the provided `continuation`
  let handShakeWithSubprotocol (choose : string [] -> HttpContext -> Async<string option>) (continuation : WebSocket -> HttpContext -> SocketOp<unit>) (ctx : HttpContext) =
    async {
      match validateHandShake ctx with
      | Choice1Of2 webSocketKey ->
        let! subprotocol =
          match ctx.request.header "sec-websocket-protocol" with
          | Choice1Of2 webSocketProtocol -> choose (webSocketProtocol.Split [|','|]) ctx
          | _ -> async.Return None

        let! a = handShakeAux subprotocol webSocketKey continuation ctx
        match a with
        | Ok _ ->
          do ()
        | Result.Error err ->
          do ()
        return! Control.CLOSE ctx
      | Choice2Of2 response -> return! response
    }

  /// The handShake combinator captures a WebSocket and pass it to the provided `continuation`
  let handShakeTask (continuation : WebSocket -> HttpContext -> SocketOp<unit>) (ctx : HttpContext) = task {
    match validateHandShake ctx with
    | Choice1Of2 webSocketKey ->
      let! a = handShakeAux None webSocketKey continuation ctx
      match a with
      | Ok _ ->
        do ()
      | Result.Error err ->
        Console.WriteLine($"WebSocket disconnected {err}",err)
      return! Control.CLOSE ctx
    | Choice2Of2 response ->
      return! response
  }

  let handShake (continuation : WebSocket -> HttpContext -> SocketOp<unit>) (ctx : HttpContext) = async {
    do! handShakeTask continuation ctx
    return Some ctx
  }
