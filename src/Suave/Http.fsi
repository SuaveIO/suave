namespace Suave

[<AutoOpen>]
module Http =

  open System
  open System.Net
  open Suave.Utils
  open Suave.Logging
  open Suave.Sockets

  /// These are the known HTTP methods. See http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html
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
    /// This represents a method string that isn't one of the standard methods.
    | OTHER of string

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpMethod =

    /// Parse a string into a HttpMethod
    val parse : string -> HttpMethod

  /// A HTTP status code and reason message
  type HttpStatus =
    { code   : int
      reason : string
    }
    static member code_ : Property<HttpStatus, int>
    static member reason_ : Property<HttpStatus, string>

  /// The standard HTTP response codes
  type HttpCode =
    | HTTP_100 | HTTP_101
    | HTTP_200 | HTTP_201 | HTTP_202 | HTTP_203 | HTTP_204 | HTTP_205 | HTTP_206
    | HTTP_300 | HTTP_301 | HTTP_302 | HTTP_303 | HTTP_304 | HTTP_305 | HTTP_306
    | HTTP_307 | HTTP_400 | HTTP_401 | HTTP_402 | HTTP_403 | HTTP_404 | HTTP_405
    | HTTP_406 | HTTP_407 | HTTP_408 | HTTP_409 | HTTP_410 | HTTP_411 | HTTP_412
    | HTTP_413 | HTTP_422 | HTTP_426 | HTTP_428 | HTTP_429 | HTTP_414 | HTTP_415
    | HTTP_416 | HTTP_417 | HTTP_451 | HTTP_500 | HTTP_501 | HTTP_502 | HTTP_503
    | HTTP_504 | HTTP_505

    member code : int

    member reason : string

    member message : string

    member describe : unit -> string

    member status : HttpStatus

    static member tryParse : code:int -> Choice<HttpCode, string>

  type SameSite =
    | Strict
    | Lax

  /// HTTP cookie
  type HttpCookie =
    { name     : string
      value    : string
      expires  : DateTimeOffset option
      path     : string option
      /// This cookies is only valid for the given domain
      domain   : string option
      /// This cookie is not forwarded over plaintext transports
      secure   : bool
      /// This cookie is not readable from JavaScript
      httpOnly : bool
      sameSite : SameSite option }

    static member name_ : Property<HttpCookie, string>
    static member value_ : Property<HttpCookie, string>
    static member expires_ : Property<HttpCookie, DateTimeOffset option>
    static member path_ : Property<HttpCookie, string option>
    static member domain_ : Property<HttpCookie, string option>
    static member secure_ : Property<HttpCookie, bool>
    static member httpOnly_ : Property<HttpCookie, bool>
    static member sameSite_ : Property<HttpCookie, SameSite option>

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpCookie =

    /// Create a new HttpCookie with all the given values.
    val create : name:string -> value:string -> expires:DateTimeOffset option
               -> path:string option -> domain:string option -> secure:bool
               -> httpOnly:bool
               -> sameSite:SameSite option
               -> HttpCookie

    /// Create a new cookie with the given name, value, and defaults:
    ///
    /// - no explicit expiry time
    /// - path at "/", so that it's global to the domain that it's created under.
    /// - no specific domain (defaults to the current domain plus its subdomains)
    /// - secure = false (you can set it over plain text HTTP - change to true in SSL terminator)
    /// - http_only = true - the cookie can be read from JS - change this to
    ///   false if you want to only be able to read the cookie from JS, but
    ///   Good default if you're implementing session handling.
    /// - version: an optional version field
    ///
    /// More reading:
    /// - http://www.nczonline.net/blog/2009/05/05/http-cookies-explained/
    /// - https://developer.mozilla.org/en-US/docs/Web/API/document.cookie
    ///
    val createKV : name:string -> value:string -> HttpCookie

    /// An empty cookie value
    val empty : HttpCookie

    /// Assumes only valid characters go in, see http://tools.ietf.org/html/rfc6265#section-4.1.1
    val toHeader : cookie:HttpCookie -> string

  /// A file's mime type and if compression is enabled or not
  type MimeType =
    { name        : string
      compression : bool }
    /// MimeType name lens
    static member name_ : Property<MimeType, string>
    /// MimeType compression lens
    static member compression_ : Property<MimeType, bool>

  type MimeTypesMap = string -> MimeType option

  /// A holder for uploaded file meta-data
  type HttpUpload =
    { fieldName    : string
      fileName     : string
      mimeType     : string
      tempFilePath : string }

    static member fieldName_ : Property<HttpUpload, string>
    static member fileName_ : Property<HttpUpload, string>
    static member mimeType_ : Property<HttpUpload, string>
    static member tempFilePath_ : Property<HttpUpload, string>

  [<AllowNullLiteral>]
  type TlsProvider =
    abstract wrap : Connection * obj -> SocketOp<Connection>

  /// Gets the supported protocols, HTTP and HTTPS with a certificate
  type Protocol =
    /// The HTTP protocol is the core protocol
    | HTTP
    /// The HTTP protocol tunneled in a TLS tunnel
    | HTTPS of obj

    member secure : bool

  /// Type alias for string. This is the host as seen from the server; not
  /// necessarily as seen from the client.
  type Host = string

  /// A HTTP binding is a protocol is the product of HTTP or HTTP, a DNS or IP
  /// binding and a port number.
  type HttpBinding =
    { scheme        : Protocol
      socketBinding : SocketBinding }

    member uri : path:string -> query:string -> Uri

    /// Overrides the default ToString() method to provide an implementation that
    /// is assignable to a BaseUri for a RestClient/HttpClient.
    override ToString : unit -> string

    static member scheme_ : Property<HttpBinding,Protocol>
    static member socketBinding_ : Property<HttpBinding, SocketBinding>

  /// A holder for the data extracted from the request.
  type HttpRequest =
    { httpVersion     : string
      binding         : HttpBinding
      rawPath         : string
      rawHost         : string
      rawMethod       : string
      headers         : (string * string) list
      rawForm         : byte []
      rawQuery        : string
      files           : HttpUpload list
      multiPartFields : (string * string) list
      trace           : TraceHeader }

    static member httpVersion_ : Property<HttpRequest, string>
    static member absolutePath_ : Property<HttpRequest, string>
    static member binding_ : Property<HttpRequest, HttpBinding>
    static member rawHost_ : Property<HttpRequest, string>
    static member rawMethod_ : Property<HttpRequest, string>
    static member headers_ : Property<HttpRequest, (string * string) list>
    static member rawForm_ : Property<HttpRequest, byte[]>
    static member rawQuery_ : Property<HttpRequest, string>
    static member files_ : Property<HttpRequest, HttpUpload list>
    static member multipartFields_ : Property<HttpRequest, (string * string) list>
    static member trace_ : Property<HttpRequest, TraceHeader>

    /// Gets the query string from the HttpRequest. Use queryParam to try to fetch
    /// data for individual items.
    member query : (string * string option) list

    /// Finds the key k from the query string in the HttpRequest. To access form
    /// data, use either `formData` to access normal form data, or `fieldData` to
    /// access the multipart-fields.
    member queryParam : key:string -> Choice<string, string>

    /// Try to find the query parameter named `key`. Returns None if none was
    /// found, otherwise Some.
    member queryParamOpt : key:string -> (string * string option) option

    /// Check the query string for a `flag`:
    ///
    /// - `?flag` => `true`
    /// - `?flag=false` => `false`
    /// - `?flag=apa` => `false`
    /// - `?flag=true` => `true`
    /// - `?` => `false`
    member queryFlag : flag:string -> bool

    /// Gets the header for the given key in the HttpRequest
    member header : key:string -> Choice<string, string>

    /// Gets the form as a ((string * string option) list) from the HttpRequest.
    /// Use formData to get the data for a particular key or use the indexed
    /// property in the HttpRequest.
    member form : (string * string option) list

    /// Finds the key k from the form of the HttpRequest. To access query string
    /// parameters, use `queryParam` or to access multipart form fields, use
    /// `fieldData`.
    member formData : key:string -> Choice<string, string>

    /// Finds the key k from the multipart-form of the HttpRequest. To access
    /// query string parameters, use `queryParam` or to access regular form data,
    /// use `formData`.
    member fieldData : key:string -> Choice<string, string>

    /// Syntactic Sugar to retrieve query string, form or multi-field values
    /// from HttpRequest
    member Item : key:string -> string option with get

    /// Get the client's view of what host is being called. If you trust your
    /// proxy the value will be fetched from X-Forwarded-Host, then the Host
    /// headers. If you don't explicitly overwrite these headers in the proxy
    /// you may be open to clients spoofing the headers. Hence the explicit
    /// interfaces which force you as a developer to think abou the problem.
    member clientHost : trustProxy:bool -> sources:string list -> Host

    /// See docs on clientHost
    member clientHostTrustProxy : Host

    /// path is equal to UrlDecode(rawPath)
    member path : string

    /// Returns Uri object representing the url associated with this request
    member url : Uri

    /// The Host that the web server responds to; not necessarily the host called
    /// by the client, as the request may have traversed proxies. As Suave
    /// binds to an IP rather than IP+Hostname, this can be anything the client
    /// has passed as the Host header. If you're behind a proxy, it may be the
    /// DNS name of the node that the reverse proxy forwards to, or if you're
    /// exposing Suave publically, it should match the public DNS name of the
    /// node.
    ///
    /// To ensure the correct host-name is being called, you can use `Http.host`
    /// in your web app.
    member host : Host

    /// Returns a HttpMethod 
    member method : HttpMethod

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpRequest =
    val empty : HttpRequest

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpBinding =

    val DefaultBindingPort : Port

    /// This is the value of the default HttpBinding.
    val defaults : HttpBinding

    /// Create a HttpBinding for the given protocol, an IP address to bind to and
    /// a port to listen on – this is the strongly typed overload.
    val create : scheme:Protocol -> ip:IPAddress -> port:Port -> HttpBinding

    /// Create a HttpBinding for the given protocol, an IP address to bind to and
    /// a port to listen on – this is the "stringly typed" overload.
    val createSimple : scheme:Protocol -> ip:string -> port:int -> HttpBinding

  type HttpContent =
    /// This is the default HttpContent. If you place this is a HttpResult the web
    /// server won't be that happy. It's assumed by Suave that you place a proper
    /// Bytes or SocketTask content as the value – all built-in Http applicates
    /// do this properly.
    | NullContent
    /// This tells Suave to respond with an array of bytes. Since most responses
    /// are small enough to fit into memory, this is the most used HttpContent
    /// used as results. If you want a streaming result, use SocketTask instead;
    /// useful when you're serving large files through Suave.
    | Bytes of byte []
    /// This task, especially with the `writePreamble`-flag lets your WebPart
    /// control the flow of bytes by using a SocketOp. Contrasting with Bytes,
    /// setting the HttpContent as this discriminated union type lets you stream
    /// data back to the client through Suave.
    | SocketTask of (Connection * HttpResult -> SocketOp<Connection>)

    static member NullContent__  : (HttpContent -> unit option) * (unit -> HttpContent)
    static member Bytes__       : (HttpContent -> byte [] option) * (byte [] -> HttpContent)
    static member SocketTask__  : (HttpContent -> (Connection * HttpResult -> SocketOp<Connection>) option)
                                * ((Connection * HttpResult -> SocketOp<Connection>) -> HttpContent)

    static member NullContent_ : (HttpContent -> unit option)
                               * (unit -> HttpContent -> HttpContent)
    static member Bytes_ : (HttpContent -> byte [] option)
                         * (byte [] -> HttpContent -> HttpContent)
    static member SocketTask_ : (HttpContent -> (Connection * HttpResult -> SocketOp<Connection>) option)
                              * ((Connection * HttpResult -> SocketOp<Connection>) -> HttpContent -> HttpContent)

  /// The HttpResult is the structure that you work with to tell Suave how to
  /// send the response. Have a look at the docs for HttpContent for further
  /// details on what is possible.
  and HttpResult =
    { status        : HttpStatus
      headers       : (string * string) list
      content       : HttpContent
      writePreamble : bool }

    static member status_ : Property<HttpResult,HttpStatus>
    static member headers_ : Property<HttpResult,(string * string) list>
    static member content_ : Property<HttpResult, HttpContent>
    static member writePreamble_ : Property<HttpResult, bool>

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpResult =

    /// The empty HttpResult, with a 404 and a HttpContent.NullContent content
    val empty : HttpResult

  /// A server-key is a 256 bit key with high entropy
  type ServerKey = byte []

  /// Utilities to ensure server keys are well-formed
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module ServerKey =
    
    /// Ensure that a server key is the proper length
    val validate : ServerKey -> ServerKey

    /// Create a key from a base-64 encoded string
    val fromBase64 : (string -> ServerKey)

  type IPAddress with
    /// Try parse the IP address from a string, returning a choice.
    static member tryParseC : ip:string -> Choice<IPAddress, unit>

  /// The HttpRuntime is created from the SuaveConfig structure when the web
  /// server starts. You can also use the `HttpRuntime` module to create a new
  /// value yourself, or use the `empty` one.
  type HttpRuntime =
    { serverKey         : ServerKey
      errorHandler      : ErrorHandler
      mimeTypesMap      : MimeTypesMap
      homeDirectory     : string
      compressionFolder : string
      logger            : Logger
      matchedBinding    : HttpBinding
      cookieSerialiser  : CookieSerialiser
      tlsProvider       : TlsProvider
      hideHeader        : bool
      maxContentLength  : int }

    static member serverKey_ : Property<HttpRuntime, ServerKey>
    static member errorHandler_ : Property<HttpRuntime, ErrorHandler>
    static member mimeTypesMap_ : Property<HttpRuntime, MimeTypesMap>
    static member homeDirectory_ : Property<HttpRuntime, string>
    static member compressionFolder_ : Property<HttpRuntime, string>
    static member logger_ : Property<HttpRuntime, Logger>
    static member matchedBinding_ : Property<HttpRuntime, HttpBinding>
    static member cookieSerialiser_ : Property<HttpRuntime, CookieSerialiser>
    static member tlsProvider_ : Property<HttpRuntime, TlsProvider>
    static member hideHeader_ : Property<HttpRuntime, bool>
    static member maxContentLength_ : Property<HttpRuntime, int>

  /// The HttpContext is the container of the request, runtime, user-state and
  /// response.
  and HttpContext =
    { /// The HTTP request being processed
      request    : HttpRequest

      /// The HttpRuntime for the request being processed
      runtime    : HttpRuntime

      /// The connection for the request being processed
      connection : Connection

      /// The user state for the request being processed
      userState  : Map<string, obj>

      /// The response for the request being processed
      response   : HttpResult }

    /// Get the IP of the client from the HttpContext.
    member clientIp : trustProxy:bool -> sources:string list -> IPAddress

    /// Warning; if you don't write these headers in your rev.proxy, the client will
    /// be able to spoof them. Related headers:
    /// - client-ip
    /// - x-forwarded-for: the "X-Forwarded-For" client request header field with
    ///   the $remote_addr variable appended to it, separated by a comma. If the
    ///   "X-Forwarded-For" field is not present in the client request header, the
    ///   $proxy_add_x_forwarded_for variable is equal to the $remote_addr variable.
    ///   from http://nginx.org/en/docs/http/ngx_http_proxy_module.html
    ///
    /// Related blog entry, with suggestion on nginx module to use to recursively
    /// tell all upstream proxies to overwrite X-Real-IP:
    /// http://distinctplace.com/infrastructure/2014/04/23/story-behind-x-forwarded-for-and-x-real-ip-headers/
    member clientIpTrustProxy : IPAddress

    member isLocal : bool

    member clientPort : trustProxy:bool -> sources:string list -> Port

    member clientPortTrustProxy : Port

    member clientProto : trustProxy:bool -> sources:string list -> string

    member clientProtoTrustProxy : string

    static member request_     : Property<HttpContext, HttpRequest>
    static member runtime_     : Property<HttpContext, HttpRuntime>

    /// read-only
    static member connection_  : Property<HttpContext, Connection>
    static member userState_   : Property<HttpContext, Map<string, obj>>
    static member response_    : Property<HttpContext, HttpResult>

    /// read-only
    static member clientIp_    : Property<HttpContext, IPAddress>

    /// read-only
    static member isLocal_     : Property<HttpContext, bool>

    /// read-only
    static member clientPort_  : Property<HttpContext, Port>

    /// read-only
    static member clientProto_ : Property<HttpContext, string>

  /// A WebPart is an asynchronous function that transforms the HttpContext.  An asynchronous return
  /// value of None indicates 'did not handle'.

  /// An error handler takes the exception, a programmer-provided message, a
  /// request (that failed) and returns an asynchronous workflow for the handling
  /// of the error.
  and ErrorHandler = Exception -> String -> WebPart<HttpContext>

  type WebPart = WebPart<HttpContext>

  /// a module that gives you the `empty` (beware) and `create` functions for creating
  /// a HttpRuntime
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpRuntime =

    /// The key length in bytes, references Crypto.KeyLength which is appropriate
    /// for the underlying AES-256 bit symmetric crypto in use.
    val ServerKeyLength : uint16

    /// warn: this is not to be played around with; prefer using the config
    /// defaults instead, from Web.fs, as they contain the logic for printing to
    /// the output stream correctly.
    val empty : HttpRuntime

    /// make a new HttpRuntime from the given parameters
    val create : serverKey:ServerKey -> errorHandler:ErrorHandler
               -> mimeTypes:MimeTypesMap -> homeDirectory:string
               -> compressionFolder:string -> logger:Logger
               -> cookieSerialiser:CookieSerialiser
               -> tlsProvider:TlsProvider -> hideHeader:bool -> maxContentLength:int 
               -> binding:HttpBinding
               -> HttpRuntime

  /// A module that provides functions to create a new HttpContext.
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpContext =
    /// The empty HttpContext is fairly useless for doing real work; you'd be well
    /// adviced to write some of the properties. However, it can be quite useful
    /// in unit tests.
    val empty : HttpContext

    val create : request:HttpRequest -> runtime:HttpRuntime -> connection:Connection
               -> writePreamble:bool
               -> HttpContext

  val request : apply:(HttpRequest -> HttpContext -> 'a) -> context:HttpContext -> 'a
  val context : apply:(HttpContext -> HttpContext -> 'a) -> context:HttpContext -> 'a
