namespace Suave

[<AutoOpen>]
module Http =

  open System
  open System.IO
  open System.Collections.Generic
  open System.Net.Sockets
  open System.Net
  open System.Text
  open Suave.Utils
  open Suave.Utils.Aether
  open Suave.Utils.Aether.Operators
  open Suave.Utils.Collections
  open Suave.Sockets
  open Suave.Tcp
  open Suave.Utils
  open Suave.Log
  open Suave.Logging

  open Microsoft.FSharp.Reflection

  [<RequireQualifiedAccess>]
  type HttpMethod =
    | GET
    | POST
    | DELETE
    | PUT
    | HEAD
    | CONNECT
    | PATCH
    | TRACE
    | OPTIONS
    | OTHER of string

    override x.ToString() =
      match x with
      | GET     -> "GET"
      | POST    -> "POST"
      | DELETE  -> "DELETE"
      | PUT     -> "PUT"
      | HEAD    -> "HEAD"
      | CONNECT -> "CONNECT"
      | PATCH   -> "PATCH"
      | TRACE   -> "TRACE"
      | OPTIONS -> "OPTIONS"
      | OTHER s -> s

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpMethod =

    let parse (methodString:string) =
      match methodString.ToUpperInvariant() with
        | "GET"     -> HttpMethod.GET
        | "POST"    -> HttpMethod.POST
        | "DELETE"  -> HttpMethod.DELETE
        | "PUT"     -> HttpMethod.PUT
        | "HEAD"    -> HttpMethod.HEAD
        | "CONNECT" -> HttpMethod.CONNECT
        | "PATCH"   -> HttpMethod.PATCH
        | "TRACE"   -> HttpMethod.TRACE
        | "OPTIONS" -> HttpMethod.OPTIONS
        | s         -> HttpMethod.OTHER s

  type HttpStatus = 
    { code   : int
      reason : string
    }
    static member code_ = (fun x -> x.code), fun v x -> { x with code = v }
    static member reason_ = (fun x -> x.reason), fun v x -> { x with reason = v }

  type HttpCode =
    | HTTP_100 | HTTP_101
    | HTTP_200 | HTTP_201 | HTTP_202 | HTTP_203 | HTTP_204 | HTTP_205 | HTTP_206
    | HTTP_300 | HTTP_301 | HTTP_302 | HTTP_303 | HTTP_304 | HTTP_305 | HTTP_307
    | HTTP_400 | HTTP_401 | HTTP_402 | HTTP_403 | HTTP_404 | HTTP_405 | HTTP_406
    | HTTP_407 | HTTP_408 | HTTP_409 | HTTP_410 | HTTP_411 | HTTP_412 | HTTP_413
    | HTTP_422 | HTTP_428 | HTTP_429 | HTTP_414 | HTTP_415 | HTTP_416 | HTTP_417
    | HTTP_451 | HTTP_500 | HTTP_501 | HTTP_502 | HTTP_503 | HTTP_504 | HTTP_505

    member x.code = 
      match x with 
      | HTTP_100 -> 100 | HTTP_101 -> 101 | HTTP_200 -> 200 | HTTP_201 -> 201
      | HTTP_202 -> 202 | HTTP_203 -> 203 | HTTP_204 -> 204 | HTTP_205 -> 205
      | HTTP_206 -> 206 | HTTP_300 -> 300 | HTTP_301 -> 301 | HTTP_302 -> 302
      | HTTP_303 -> 303 | HTTP_304 -> 304 | HTTP_305 -> 305 | HTTP_307 -> 307
      | HTTP_400 -> 400 | HTTP_401 -> 401 | HTTP_402 -> 402 | HTTP_403 -> 403
      | HTTP_404 -> 404 | HTTP_405 -> 405 | HTTP_406 -> 406 | HTTP_407 -> 407
      | HTTP_408 -> 408 | HTTP_409 -> 409 | HTTP_410 -> 410 | HTTP_411 -> 411
      | HTTP_412 -> 412 | HTTP_413 -> 413 | HTTP_414 -> 414 | HTTP_415 -> 415
      | HTTP_416 -> 416 | HTTP_417 -> 417 | HTTP_422 -> 422 | HTTP_428 -> 428
      | HTTP_429 -> 429 | HTTP_451 -> 451 | HTTP_500 -> 500 | HTTP_501 -> 501
      | HTTP_502 -> 502 | HTTP_503 -> 503 | HTTP_504 -> 504 | HTTP_505 -> 505

    member x.reason = 
      match x with
      | HTTP_100 -> "Continue"
      | HTTP_101 -> "Switching Protocols"
      | HTTP_200 -> "OK"
      | HTTP_201 -> "Created"
      | HTTP_202 -> "Accepted"
      | HTTP_203 -> "Non-Authoritative Information"
      | HTTP_204 -> "No Content"
      | HTTP_205 -> "Reset Content"
      | HTTP_206 -> "Partial Content"
      | HTTP_300 -> "Multiple Choices"
      | HTTP_301 -> "Moved Permanently"
      | HTTP_302 -> "Found"
      | HTTP_303 -> "See Other"
      | HTTP_304 -> "Not Modified"
      | HTTP_305 -> "Use Proxy"
      | HTTP_307 -> "Temporary Redirect"
      | HTTP_400 -> "Bad Request"
      | HTTP_401 -> "Unauthorized"
      | HTTP_402 -> "Payment Required"
      | HTTP_403 -> "Forbidden"
      | HTTP_404 -> "Not Found"
      | HTTP_405 -> "Method Not Allowed"
      | HTTP_406 -> "Not Acceptable"
      | HTTP_407 -> "Proxy Authentication Required"
      | HTTP_408 -> "Request Timeout"
      | HTTP_409 -> "Conflict"
      | HTTP_410 -> "Gone"
      | HTTP_411 -> "Length Required"
      | HTTP_412 -> "Precondition Failed"
      | HTTP_413 -> "Request Entity Too Large"
      | HTTP_414 -> "Request-URI Too Long"
      | HTTP_415 -> "Unsupported Media Type"
      | HTTP_416 -> "Requested Range Not Satisfiable"
      | HTTP_417 -> "Expectation Failed"
      | HTTP_422 -> "Unprocessable Entity"
      | HTTP_428 -> "Precondition Required"
      | HTTP_429 -> "Too Many Requests"
      | HTTP_451 -> "Unavailable For Legal Reasons"
      | HTTP_500 -> "Internal Server Error"
      | HTTP_501 -> "Not Implemented"
      | HTTP_502 -> "Bad Gateway"
      | HTTP_503 -> "Service Unavailable"
      | HTTP_504 -> "Gateway Timeout"
      | HTTP_505 -> "HTTP Version Not Supported"

    member x.message = 
      match x with 
      | HTTP_100 -> "Request received, please continue"
      | HTTP_101 -> "Switching to new protocol; obey Upgrade header"
      | HTTP_200 -> "Request fulfilled, document follows"
      | HTTP_201 -> "Document created, URL follows"
      | HTTP_202 -> "Request accepted, processing continues off-line"
      | HTTP_203 -> "Request fulfilled from cache"
      | HTTP_204 -> "Request fulfilled, nothing follows"
      | HTTP_205 -> "Clear input form for further input."
      | HTTP_206 -> "Partial content follows."
      | HTTP_300 -> "Object has several resources -- see URI list"
      | HTTP_301 -> "Object moved permanently -- see URI list"
      | HTTP_302 -> "Object moved temporarily -- see URI list"
      | HTTP_303 -> "Object moved -- see Method and URL list"
      | HTTP_304 -> "Document has not changed since given time"
      | HTTP_305 -> "You must use proxy specified in Location to access this resource."
      | HTTP_307 -> "Object moved temporarily -- see URI list"
      | HTTP_400 -> "Bad request syntax or unsupported method"
      | HTTP_401 -> "No permission -- see authorization schemes"
      | HTTP_402 -> "No payment -- see charging schemes"
      | HTTP_403 -> "Request forbidden -- authorization will not help"
      | HTTP_404 -> "Nothing matches the given URI"
      | HTTP_405 -> "Specified method is invalid for this resource."
      | HTTP_406 -> "URI not available in preferred format."
      | HTTP_407 -> "You must authenticate with this proxy before proceeding."
      | HTTP_408 -> "Request timed out; try again later."
      | HTTP_409 -> "Request conflict."
      | HTTP_410 -> "URI no longer exists and has been permanently removed."
      | HTTP_411 -> "Client must specify Content-Length."
      | HTTP_412 -> "Precondition in headers is false."
      | HTTP_413 -> "Entity is too large."
      | HTTP_414 -> "URI is too long."
      | HTTP_415 -> "Entity body in unsupported format."
      | HTTP_416 -> "Cannot satisfy request range."
      | HTTP_417 -> "Expect condition could not be satisfied."
      | HTTP_422 -> "The entity sent to the server was invalid."
      | HTTP_428 -> "You should verify the server accepts the request before sending it."
      | HTTP_429 -> "Request rate too high, chill out please."
      | HTTP_451 -> "The server is subject to legal restrictions which prevent it servicing the request"
      | HTTP_500 -> "Server got itself in trouble"
      | HTTP_501 -> "Server does not support this operation"
      | HTTP_502 -> "Invalid responses from another server/proxy."
      | HTTP_503 -> "The server cannot process the request due to a high load"
      | HTTP_504 -> "The gateway server did not receive a timely response"
      | HTTP_505 -> "Cannot fulfill request."

    member x.describe () =
      sprintf "%d %s: %s" x.code x.reason x.message

    member x.status = { code = x.code; reason = x.reason }

    static member tryParse (code : int) =
      let found =
        HttpCodeStatics.mapCases.Force()
        |> Map.tryFind ("HTTP_" + string code)

      match found with
      | Some x ->
        Choice1Of2 x

      | None ->
        Choice2Of2 (sprintf "Couldn't convert %i to HttpCode. Please send a PR to https://github.com/suaveio/suave if you want it" code)

  and private HttpCodeStatics() =
    static member val mapCases : Lazy<Map<string,HttpCode>> =
      lazy
        FSharpType.GetUnionCases(typeof<HttpCode>)
        |> Array.map (fun case -> case.Name, FSharpValue.MakeUnion(case, [||]) :?> HttpCode)
        |> Map.ofArray

  type HttpCookie =
    { name     : string
      value    : string
      expires  : DateTimeOffset option
      path     : string option
      domain   : string option
      secure   : bool
      httpOnly : bool }

    static member name_     = (fun x -> x.name),    fun v x -> { x with name = v }
    static member value_    = (fun x -> x.value), fun v x -> { x with value = v }
    static member expires_  = (fun x -> x.expires), fun v x -> { x with expires = v }
    static member path_     = (fun x -> x.path), fun v (x : HttpCookie) -> { x with path = v }
    static member domain_   = (fun x -> x.domain), fun v x -> { x with domain = v }
    static member secure_   = (fun x -> x.secure), fun v x -> { x with secure = v }
    static member httpOnly_ = (fun x -> x.httpOnly), fun v x -> { x with httpOnly = v }

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpCookie =

    let mk name value expires path domain secure httpOnly =
      { name      = name
        value     = value
        expires   = expires
        path      = path
        domain    = domain
        secure    = secure
        httpOnly = httpOnly }


    let mkKV name value =
      { name      = name
        value     = value
        expires   = None
        path      = Some "/"
        domain    = None
        secure    = false
        httpOnly = true }

    let empty = mkKV "" ""

    let toHeader (x : HttpCookie) =
      let app (sb : StringBuilder) (value : string) = sb.Append value |> ignore
      let sb = new StringBuilder(String.Concat [ x.name; "="; x.value ])
      let app value = app sb (String.Concat [";"; value])
      let appkv k fMap v = v |> Option.iter (fun v -> app (String.Concat [ k; "="; fMap v ]))
      x.domain  |> appkv "Domain" id
      x.path    |> appkv "Path" id
      x.expires |> appkv "Expires" (fun (i : DateTimeOffset) -> i.ToString("R"))
      if x.httpOnly then app "HttpOnly"
      if x.secure    then app "Secure"
      sb.ToString ()

  type MimeType =
    { name        : string
      compression : bool }

    static member name_ = Property (fun x -> x.name) (fun v (x : MimeType) -> { x with name = v })
    static member compression_ = Property (fun x -> x.compression) (fun v (x : MimeType) -> { x with compression = v })

  type MimeTypesMap = string -> MimeType option

  type HttpUpload =
    { fieldName    : string
      fileName     : string
      mimeType     : string
      tempFilePath : string }

    static member fieldName_ = Property<HttpUpload,_> (fun x -> x.fieldName) (fun v x -> { x with fieldName = v })
    static member fileName_ = Property<HttpUpload,_> (fun x -> x.fileName) (fun v x -> { x with fileName = v })
    static member mimeType_ = Property<HttpUpload,_> (fun x -> x.mimeType) (fun v x -> { x with mimeType = v })
    static member tempFilePath_ = Property<HttpUpload,_> (fun x -> x.tempFilePath) (fun v x -> { x with tempFilePath = v })

  [<AllowNullLiteral>]
  type TlsProvider =
    abstract member wrap : Connection * obj -> SocketOp<Connection>

  type Protocol = 
    | HTTP
    | HTTPS of obj

    member x.secure = 
      match x with
      | HTTP    -> false
      | HTTPS _ -> true

    override x.ToString() = 
      match x with
      | HTTP    -> "http"
      | HTTPS _ -> "https"

  type Host = string

  type HttpRequest =
    { httpVersion     : string
      url             : Uri
      host            : Host
      ``method``      : HttpMethod
      headers         : (string * string) list
      rawForm         : byte []
      rawQuery        : string
      files           : HttpUpload list
      multiPartFields : (string * string) list
      trace           : TraceHeader }
    static member httpVersion_     = Property<HttpRequest,_> (fun x -> x.httpVersion) (fun v (x : HttpRequest) -> { x with httpVersion = v })
    static member url_             = Property<HttpRequest,_> (fun x -> x.url) (fun v x -> { x with url = v })
    static member host_            = Property<HttpRequest,_> (fun x -> x.host) (fun v x -> { x with host = v })
    static member method_          = Property<HttpRequest,_> (fun x -> x.``method``) (fun v x -> { x with ``method`` = v })
    static member headers_         = Property<HttpRequest,_> (fun x -> x.headers) (fun v x -> { x with headers = v })
    static member rawForm_         = Property<HttpRequest,_> (fun x -> x.rawForm) (fun v x -> { x with rawForm = v })
    static member rawQuery_        = Property<HttpRequest,_> (fun x -> x.rawQuery) (fun v x -> { x with rawQuery = v })
    static member files_           = Property<HttpRequest,_> (fun x -> x.files) (fun v x -> { x with files = v })
    static member multipartFields_ = Property<HttpRequest,_> (fun x -> x.multiPartFields) (fun v x -> { x with multiPartFields = v })
    static member trace_           = Property<HttpRequest,_> (fun x -> x.trace) (fun v x -> { x with trace = v })

    member x.query =
      Parsing.parseData x.rawQuery

    member x.queryParam (key : string) =
      getFirstOpt x.query key

    member x.header key =
      // Field names are case-insensitive (RFC 2616 section 4.2)
      getFirstCaseInsensitve x.headers key

    member x.form =
      Parsing.parseData (ASCII.toString x.rawForm)

    member x.formData (key : string) =
      getFirstOpt x.form key

    member x.fieldData (k : string) =
      getFirst x.multiPartFields k

    member this.Item
      with get(x) =

        let inline (>>=) f1 f2 x =
          match f1 x with
          | Some x' -> Some x'
          | None -> f2 x

        let params' =
          (tryGetChoice1 this.queryParam)
            >>= (tryGetChoice1 this.formData) 
            >>= (tryGetChoice1 <| getFirst this.multiPartFields)

        params' x

    member x.clientHost trustProxy sources : string =
      if trustProxy then
        sources
        |> List.fold (fun state source ->
          state |> Choice.bindSnd (fun _ -> x.header source))
          (Choice2Of2 "")
        |> Choice.orDefault x.host
      else
        x.host

    member x.clientHostTrustProxy =
      x.clientHost true [ "x-forwarded-host" ]

    member x.path =
      System.Net.WebUtility.UrlDecode x.url.AbsolutePath

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpRequest =

    open Suave.Utils

    let empty =
      { httpVersion     = "HTTP/1.1"
        url             = Uri "http://localhost/"
        host            = "localhost"
        ``method``      = HttpMethod.OTHER ""
        headers         = []
        rawForm         = Array.empty
        rawQuery        = ""
        files           = []
        multiPartFields = []
        trace           = TraceHeader.empty }

  type HttpBinding =
    { scheme        : Protocol
      socketBinding : SocketBinding }

    member x.uri (path : string) query =
      let path' = if path.StartsWith "/" then path else "/" + path
      String.Concat [
        x.scheme.ToString(); "://"; x.socketBinding.ToString()
        path'
        (match query with | "" -> "" | qs -> "?" + qs)
      ]
      |> fun x -> Uri x

    override x.ToString() =
      String.Concat [ x.scheme.ToString(); "://"; x.socketBinding.ToString() ]

    static member scheme_ = Property<HttpBinding,_> (fun x -> x.scheme) (fun v x -> { x with scheme = v })
    static member socketBinding_ = Property<HttpBinding,_> (fun x -> x.socketBinding) (fun v x -> { x with socketBinding=v })

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpBinding =

    let DefaultBindingPort = 8083us

    let defaults =
      { scheme        = HTTP
        socketBinding = SocketBinding.mk IPAddress.Loopback DefaultBindingPort }

    let mk scheme (ip : IPAddress) (port : Port) = 
      { scheme        = scheme
        socketBinding = SocketBinding.mk ip port }

    let mkSimple scheme ip (port : int) = 
      { scheme        = scheme
        socketBinding = SocketBinding.mk (IPAddress.Parse ip) (uint16 port) } 

  type HttpContent =
    | NullContent
    | Bytes of byte []
    | SocketTask of (Connection * HttpResult -> SocketOp<Connection>)

    static member NullContent__ =
      (function | NullContent -> Some ()
                | _ -> None),
      fun _ -> NullContent

    static member Bytes__ =
      (function | Bytes bs -> Some bs
                | _ -> None),
      Bytes

    static member SocketTask__ =
      (function | SocketTask cb -> Some cb
                | _ -> None),
      (fun cb -> SocketTask cb)

    static member NullContent_ : PLens<HttpContent, unit> =
      Aether.idLens <-?> HttpContent.NullContent__

    static member Bytes_ : PLens<HttpContent, byte[]> =
      Aether.idLens <-?> HttpContent.Bytes__

    static member SocketTask_ : PLens<HttpContent, Connection * HttpResult -> SocketOp<Connection>> =
      Aether.idLens <-?> HttpContent.SocketTask__

  and HttpResult =
    { status        : HttpStatus
      headers       : (string * string) list
      content       : HttpContent
      writePreamble : bool }

    static member status_ = Property<HttpResult,_> (fun x -> x.status) (fun v x -> { x with status = v })
    static member headers_ = Property<HttpResult,_> (fun x -> x.headers) (fun v x -> { x with headers = v })
    static member content_ = Property<HttpResult,_> (fun x -> x.content) (fun v x -> { x with content = v })
    static member writePreamble_ = Property<HttpResult,_> (fun x -> x.writePreamble) (fun v x -> { x with writePreamble = v })

  type ServerKey = byte []

  type IPAddress with
    static member tryParseC str =
      match IPAddress.TryParse str with
      | false, _ -> Choice2Of2 ()
      | _, ip    -> Choice1Of2 ip

  type HttpRuntime =
    { serverKey         : ServerKey
      errorHandler      : ErrorHandler
      mimeTypesMap      : MimeTypesMap
      homeDirectory     : string
      compressionFolder : string
      logger            : Logger
      matchedBinding    : HttpBinding
      parsePostData     : bool
      cookieSerialiser  : CookieSerialiser
      tlsProvider       : TlsProvider
      hideHeader        : bool }

    static member serverKey_ = Property (fun x -> x.serverKey) (fun v x -> { x with serverKey = v })
    static member errorHandler_ = Property (fun x -> x.errorHandler) (fun v x -> { x with errorHandler = v })
    static member mimeTypesMap_ = Property (fun x -> x.mimeTypesMap) (fun v x -> { x with mimeTypesMap = v })
    static member homeDirectory_ = Property (fun x -> x.homeDirectory) (fun v x -> { x with homeDirectory = v })
    static member compressionFolder_ = Property (fun x -> x.compressionFolder) (fun v x -> { x with compressionFolder = v })
    static member logger_ = Property (fun x -> x.logger) (fun v x -> { x with logger = v })
    static member matchedBinding_ = Property (fun x -> x.matchedBinding) (fun v x -> { x with matchedBinding = v })
    static member parsePostData_ = Property (fun x -> x.parsePostData) (fun v x -> { x with parsePostData = v })
    static member cookieSerialiser_ = Property (fun x -> x.cookieSerialiser) (fun v x -> { x with cookieSerialiser = v })
    static member tlsProvider_ = Property (fun x -> x.tlsProvider) (fun v x -> { x with tlsProvider = v })
    static member hideHeader_ = Property (fun x -> x.hideHeader) (fun v x -> { x with hideHeader = v })

  and HttpContext =
    { request    : HttpRequest
      runtime    : HttpRuntime
      connection : Connection
      userState  : Map<string, obj>
      response   : HttpResult }

    member x.clientIp trustProxy sources =
      if trustProxy then
        sources
        |> List.fold (fun state source ->
          state |> Choice.bindSnd (fun _ ->
            x.request.header source |> Choice.bindUnit IPAddress.tryParseC))
          (Choice2Of2 ())
        |> Choice.orDefault x.connection.ipAddr
      else
        x.connection.ipAddr

    member x.clientIpTrustProxy =
      x.clientIp true [ "x-real-ip" ]

    member x.isLocal =
      IPAddress.IsLoopback (x.clientIp false [])

    member x.clientPort trustProxy sources : Port =
      if trustProxy then
        sources
        |> List.fold (fun state source ->
          state |> Choice.bindSnd (fun _ ->
            x.request.header source
            |> Choice.bind (
              Choice.parser UInt16.TryParse "failed to parse X-Forwarded-Port")))
          (Choice2Of2 "")
        |> Choice.orDefault x.connection.port
      else
        x.connection.port

    member x.clientPortTrustProxy =
      x.clientPort true [ "x-forwarded-port" ]

    member x.clientProto trustProxy sources : string =
      if trustProxy then
        sources
        |> List.fold (fun state source ->
          state |> Choice.bindSnd (fun _ ->
            x.request.header source))
          (Choice2Of2 "")
        |> Choice.orDefault (x.runtime.matchedBinding.scheme.ToString())
      else
        x.runtime.matchedBinding.scheme.ToString()

    member x.clientProtoTrustProxy =
      x.clientProto true [ "x-forwarded-proto" ]

    static member request_     = Property (fun x -> x.request) (fun v x -> { x with request = v })
    static member runtime_     = Property (fun x -> x.runtime) (fun v x -> { x with runtime = v })
    static member connection_  = Property (fun x -> x.connection) (fun v x -> x)
    static member userState_   = Property (fun x -> x.userState) (fun v x -> { x with userState = v })
    static member response_    = Property (fun x -> x.response) (fun v x -> { x with response = v })

    // read-only
    static member clientIp_    = Property (fun (x : HttpContext) -> x.clientIpTrustProxy) (fun v x -> x)
    static member isLocal_     = Property (fun (x : HttpContext) -> x.isLocal) (fun v x -> x)
    static member clientPort_  = Property (fun (x : HttpContext) -> x.clientPortTrustProxy) (fun v x -> x)
    static member clientProto_ = Property (fun (x : HttpContext) -> x.clientProtoTrustProxy) (fun v x -> x)

  and ErrorHandler = Exception -> String -> WebPart<HttpContext>

  type WebPart = WebPart<HttpContext>

  /// a module that gives you the `empty` result
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpResult =

    /// The empty HttpResult, with a 404 and a HttpContent.NullContent content
    let empty =
      { status        = HTTP_404.status
        headers       = []
        content       = HttpContent.NullContent
        writePreamble = true }

  /// a module that gives you the `empty` (beware) and `mk` functions for creating
  /// a HttpRuntime
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpRuntime =

    let ServerKeyLength : uint16 = Crypto.KeyLength

    let empty =
      { serverKey         = Crypto.generateKey ServerKeyLength
        errorHandler      = fun _ _ -> fun _ -> async.Return None
        mimeTypesMap      = fun _ -> None
        homeDirectory     = "."
        compressionFolder = "."
        logger            = Loggers.saneDefaultsFor LogLevel.Debug
        matchedBinding    = HttpBinding.defaults
        parsePostData     = false
        #if NETSTANDARD1_5
        cookieSerialiser  = new JsonFormatterSerialiser()
        #else
        cookieSerialiser  = new BinaryFormatterSerialiser()
        #endif
        tlsProvider       = null
        hideHeader        = false }

    let mk serverKey errorHandler mimeTypes homeDirectory compressionFolder
           logger parsePostData cookieSerialiser tlsProvider hideHeader binding =
      { serverKey         = serverKey
        errorHandler      = errorHandler
        mimeTypesMap      = mimeTypes
        homeDirectory     = homeDirectory
        compressionFolder = compressionFolder
        logger            = logger
        matchedBinding    = binding
        parsePostData     = parsePostData
        cookieSerialiser  = cookieSerialiser
        tlsProvider       = tlsProvider
        hideHeader        = hideHeader }

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpContext =
    let empty =
      { request    = HttpRequest.empty
        userState  = Map.empty
        runtime    = HttpRuntime.empty
        connection = Connection.empty
        response   = HttpResult.empty }

    let mk request runtime connection writePreamble =
      { request    = request
        userState  = Map.empty
        runtime    = runtime
        connection = connection
        response   = { status = HTTP_404.status
                       headers = []
                       content = NullContent
                       writePreamble = writePreamble } }

    let request x = x.request
    let userState x = x.userState
    let runtime x = x.runtime
    let response x = x.response

  let request apply (a : HttpContext) = apply a.request a
  let context apply (a : HttpContext) = apply a a
