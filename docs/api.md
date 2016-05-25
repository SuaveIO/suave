---
layout: default
---

API
===

HttpContext
-----------

{% highlight fsharp %}
type ErrorHandler = Exception -> String -> HttpContext -> HttpContext

and HttpRuntime =
  { protocol          : Protocol
    errorHandler      : ErrorHandler
    mimeTypesMap      : MimeTypesMap
    homeDirectory     : string
    compressionFolder : string
    logger            : Log.Logger
    sessionProvider   : ISessionProvider }

and HttpContext =
  { request   : HttpRequest
    runtime   : HttpRuntime
    userState : Map<string, obj>
    response  : HttpResult }

and ISessionProvider =
  abstract member Generate : TimeSpan * HttpContext -> string
  abstract member Validate : string * HttpContext -> bool
  abstract member Session<'a>  : string -> SessionStore<'a>
{% endhighlight %}

Default-supported HTTP Verbs
----------------------------

See [RFC 2616](http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html).

These applicatives match on HTTP verbs.

{% highlight fsharp %}
let GET     (x : HttpRequest) = ``method`` "GET" x
let POST    (x : HttpRequest) = ``method`` "POST" x
let DELETE  (x : HttpRequest) = ``method`` "DELETE" x
let PUT     (x : HttpRequest) = ``method`` "PUT" x
let HEAD    (x : HttpRequest) = ``method`` "HEAD" x
let CONNECT (x : HttpRequest) = ``method`` "CONNECT" x
let PATCH   (x : HttpRequest) = ``method`` "PATCH" x
let TRACE   (x : HttpRequest) = ``method`` "TRACE" x
let OPTIONS (x : HttpRequest) = ``method`` "OPTIONS" x
{% endhighlight %}

Server configuration
--------------------

The first argument to `startWebServer` is a configuration record with the following signature.

{% highlight fsharp %}
/// The core configuration of suave. See also Suave.Web.defaultConfig which
/// you can use to bootstrap the configuration:
/// <code>{ defaultConfig with bindings = [ ... ] }</code>
type SuaveConfig =
  { /// The bindings for the web server to launch with
    bindings          : HttpBinding list

    /// The server key used for creating cookies
    serverKey         : byte [] // should be 64 bytes (256 bits)

    /// An error handler to use for handling exceptions that are
    /// are thrown from the web parts
    errorHandler      : ErrorHandler

    /// Timeout to wait for the socket bind to finish
    listenTimeout     : TimeSpan

    /// A cancellation token for the web server. Signalling this token
    /// means that the web server shuts down
    cancellationToken : CancellationToken

    /// buffer size for socket operations
    bufferSize        : int

    /// max number of concurrent socket operations
    maxOps            : int

    /// MIME types
    mimeTypesMap      : MimeTypesMap

    /// Home or root directory
    homeFolder        : string option

    /// Folder for temporary compressed files
    compressedFilesFolder : string option

    /// A logger to log with
    logger           : Log.Logger

    /// A http session provider
    sessionProvider  : ISessionProvider }
{% endhighlight %}

With `Protocol` , `HttpBinding` and `MimeType` defined like follows:

{% highlight fsharp %}
type ITlsProvider =
  abstract member Wrap  : Connection -> SocketOp<Connection>

/// Gets the supported protocols, HTTP and HTTPS with a certificate
type Protocol =
  /// The HTTP protocol is the core protocol
  | HTTP
  /// The HTTP protocol tunneled in a TLS tunnel
  | HTTPS of ITlsProvider

/// A HTTP binding is a protocol is the product of HTTP or HTTP, a DNS or IP binding
/// and a port number
type HttpBinding =
  /// The scheme in use
  { scheme : Protocol
    /// The host or IP address to bind to. This will be interpreted by the operating system
    ip     : IPAddress
    /// The port for the binding
    port   : uint16 }

type MimeType =
  /// The name of the mime type, i.e "text/plain"
  { name         : string
    /// If the server will compress the file when clients ask for gzip or
    /// deflate in the `Accept-Encoding` header
    compression  : bool }
{% endhighlight %}

## Overview

A request life-cycle begins with the `HttpProcessor` that takes an `HttpRequest`
and the request as bytes and starts parsing it. It returns an `HttpRequest
option` that, if Some, gets run against the WebParts passed.

### The WebPart

A web part is a thing that acts on a HttpContext, the web part could fail by
returning `None` or succeed and produce a new HttpContext. Each web part can
execute asynchronously, and it's not until it is evaluated that the async is
evaluated. It will be evaluated on the same fibre (asynchronous execution
context) that is consuming from the browser's TCP socket.

{% highlight fsharp %}
type SuaveTask<'a> = Async<'a option>
type WebPart = HttpContext -> SuaveTask<HttpContext>
// hence: WebPart = HttpContext -> Async<HttpContext option>
{% endhighlight %}

### The ErrorHandler

An error handler takes the exception, a programmer-provided message, a request
(that failed) and returns a web part for the handling of the
error.

{% highlight fsharp %}
/// An error handler takes the exception, a programmer-provided message, a
/// request (that failed) and returns
/// an asynchronous workflow for the handling of the error.
type ErrorHandler = Exception -> String -> WebPart
{% endhighlight %}

