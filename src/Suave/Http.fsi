namespace Suave

[<AutoOpen>]
module Http =

  open System
  open System.Collections.Generic
  open System.Net
  open Suave.Logging
  open Suave.Sockets
  open Suave

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

    /// Parse a string into a `HttpMethod`
    val parse : string -> HttpMethod

  /// A HTTP status code and reason message
  type HttpStatus =
    { code   : int
      reason : string
    }

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

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpCookie =

    /// Create a new `HttpCookie` with all the given values.
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
    /// - `secure = false` (you can set it over plain text HTTP - change to true in SSL terminator)
    /// - `httpOnly = true` - the cookie can be read from JS - change this to
    ///   false if you want to only be able to read the cookie from JS, but
    ///   Good default if you're implementing session handling.
    /// - `version`: an optional version field
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

  /// A holder for the data extracted from the request.
  type [<Struct>] HttpRequest =
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

    /// Gets the query string from the `HttpRequest` as a list of `(key, value option)` tuples.
    /// If a key is present without value, the entry will be `(key, None)`. It will be `(key, Some value)` otherwise.
    /// If a key is present multiple times in the URL, this list will contain multiple entries.
    ///
    /// Use `queryParam` to try to fetch data for individual items.
    member query : (string * string option) list

    /// Gets the value of the first matching query parameter with `key` in the `HttpRequest` as `Choice1Of2`.
    /// The `key` comparison is case-sensitive.
    /// If `key` is not present, or no value is present for the first occurrence of `key`,
    /// an error is returned as `Choice2Of2`.
    ///
    /// To access form data, use either `formData` to access normal form data, or `fieldData` to
    /// access the multipart-fields.
    member queryParam : key:string -> Choice<string, string>

    /// Finds the first matching query parameter with `key` in the `HttpRequest` and returns
    ///
    /// - `None` if no matching parameter was found,
    /// - `(key, None)` if a matching parameter was found, but without value, or
    /// - `(key, Some value)` if a matching parameter was found.
    member queryParamOpt : key:string -> (string * string option) option

    /// Check the query string for a `flag`:
    ///
    /// - `?flag` => `true`
    /// - `?flag=false` => `false`
    /// - `?flag=apa` => `false`
    /// - `?flag=true` => `true`
    /// - `?` => `false`
    member queryFlag : flag:string -> bool

    /// Gets the header for the given key in the `HttpRequest`
    member header : key:string -> Choice<string, string>

    /// Gets the form as a `((string * string option) list)` from the HttpRequest.
    /// Use `formData` to get the data for a particular key or use the indexed
    /// property in the `HttpRequest`.
    member form : (string * string option) list

    /// Finds the key k from the form of the `HttpRequest`. To access query string
    /// parameters, use `queryParam` or to access multipart form fields, use
    /// `fieldData`.
    member formData : key:string -> Choice<string, string>

    /// Finds the key k from the multipart-form of the `HttpRequest`. To access
    /// query string parameters, use `queryParam` or to access regular form data,
    /// use `formData`.
    member fieldData : key:string -> Choice<string, string>

    /// Syntactic Sugar to retrieve query string, form or multi-field values
    /// from `HttpRequest`
    member Item : key:string -> string option with get

    /// Get the client's view of what host is being called. If you trust your
    /// proxy the value will be fetched from `X-Forwarded-Host`, then the `Host`
    /// headers. If you don't explicitly overwrite these headers in the proxy
    /// you may be open to clients spoofing the headers. Hence the explicit
    /// interfaces which force you as a developer to think about the problem.
    member clientHost : trustProxy:bool -> sources:string list -> Host

    /// See docs on `clientHost`
    member clientHostTrustProxy : Host

    /// path is equal to `UrlDecode(rawPath)`
    member path : string

    /// Returns `Uri` object representing the url associated with this request
    member url : Uri

    /// The `Host` that the web server responds to; not necessarily the host called
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

    /// Returns a `HttpMethod`
    member method : HttpMethod

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpRequest =
    val empty : HttpRequest



  type HttpContent =
    /// This is the default `HttpContent`. If you place this in a `HttpResult` the web
    /// server won't be that happy. It's assumed by Suave that you place a proper
    /// Bytes or SocketTask content as the value – all built-in Http applicates
    /// do this properly.
    | NullContent
    /// This tells Suave to respond with an array of bytes. Since most responses
    /// are small enough to fit into memory, this is the most used `HttpContent`
    /// used as results. If you want a streaming result, use `SocketTask` instead;
    /// useful when you're serving large files through Suave.
    | Bytes of byte []
    /// This task, especially with the `writePreamble`-flag lets your `WebPart`
    /// control the flow of bytes by using a `SocketOp`. Contrasting with `Bytes`,
    /// setting the `HttpContent` as this discriminated union type lets you stream
    /// data back to the client through Suave.
    | SocketTask of (Connection * HttpResult -> SocketOp<unit>)


  /// The `HttpResult` is the structure that you work with to tell Suave how to
  /// send the response. Have a look at the docs for `HttpContent` for further
  /// details on what is possible.
  and [<Struct>] HttpResult =
    { status        : HttpStatus
      headers       : (string * string) list
      content       : HttpContent
      writePreamble : bool }

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpResult =

    /// The empty `HttpResult`, with a 404 and a `HttpContent.NullContent` content
    val empty : HttpResult

  /// The `HttpRuntime` is created from the `SuaveConfig` structure when the web
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
      hideHeader        : bool
      maxContentLength  : int }

  /// The `HttpContext` is the container of the request, runtime, user-state and
  /// response.
  and [<Struct>] HttpContext =
    { /// The HTTP request being processed
      mutable request    : HttpRequest

      /// The `HttpRuntime` for the request being processed
      runtime    : HttpRuntime

      /// The connection for the request being processed
      connection : Connection

      /// The user state for the request being processed
      userState  : Dictionary<string, obj>

      /// The response for the request being processed
      response   : HttpResult }

    /// Get the IP of the client from the `HttpContext`.
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

    member isLocalTrustProxy : bool

    member clientPort : trustProxy:bool -> sources:string list -> Port

    member clientPortTrustProxy : Port

    member clientProto : trustProxy:bool -> sources:string list -> string

    member clientProtoTrustProxy : string

  /// A `WebPart` is an asynchronous function that transforms the `HttpContext`.  An asynchronous return
  /// value of `None` indicates 'did not handle'.

  /// An error handler takes the exception, a programmer-provided message, a
  /// request (that failed) and returns an asynchronous workflow for the handling
  /// of the error.
  and ErrorHandler = Exception -> String -> WebPart<HttpContext>

  type WebPart = WebPart<HttpContext>

  /// a module that gives you the `empty` (beware) and `create` functions for creating
  /// a `HttpRuntime`
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpRuntime =

    /// The key length in bytes, references `Crypto.KeyLength` which is appropriate
    /// for the underlying AES-256 bit symmetric crypto in use.
    val ServerKeyLength : uint16

    /// warn: this is not to be played around with; prefer using the config
    /// defaults instead, from Web.fs, as they contain the logic for printing to
    /// the output stream correctly.
    val empty : HttpRuntime

    /// make a new `HttpRuntime` from the given parameters
    val create : serverKey:ServerKey -> errorHandler:ErrorHandler
               -> mimeTypes:MimeTypesMap -> homeDirectory:string
               -> compressionFolder:string -> logger:Logger
               -> cookieSerialiser:CookieSerialiser
               -> hideHeader:bool -> maxContentLength:int
               -> binding:HttpBinding
               -> HttpRuntime

  /// A module that provides functions to create a new `HttpContext`.
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpContext =
    /// The empty `HttpContext` is fairly useless for doing real work; you'd be well
    /// adviced to write some of the properties. However, it can be quite useful
    /// in unit tests.
    val empty : HttpContext

    val create : request:HttpRequest -> runtime:HttpRuntime -> connection:Connection
               -> writePreamble:bool
               -> HttpContext

    val addKeepAliveHeader : ctx: HttpContext -> HttpContext

  val request : apply:(HttpRequest -> HttpContext -> 'a) -> context:HttpContext -> 'a
  val context : apply:(HttpContext -> HttpContext -> 'a) -> context:HttpContext -> 'a
