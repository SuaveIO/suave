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

The first argument to `startWebServer` is a configuration record with the
following signature. (See [below](#changing-the-default-configuration) for tips
on customizing this.)

{% highlight fsharp %}
/// The core configuration of suave. See also Suave.Web.defaultConfig which
/// you can use to bootstrap the configuration:
/// <code>{ defaultConfig with bindings = [ ... ] }</code>
type SuaveConfig =
  { /// The bindings for the web server to launch with
    bindings                : HttpBinding list

    /// A server-key to use for cryptographic operations. When generated it
    /// should be completely random; you can share this key between load-balanced
    /// servers if you want to have them cryptographically verify similarly.
    serverKey              : byte []

    /// An error handler to use for handling exceptions that are
    /// are thrown from the web parts
    errorHandler           : ErrorHandler

    /// Timeout to wait for the socket bind to finish
    listenTimeout          : TimeSpan

    /// A cancellation token for the web server. Signalling this token
    /// means that the web server shuts down
    cancellationToken      : Threading.CancellationToken

    /// buffer size for socket operations
    bufferSize             : int

    /// Buffer manager auto grow
    autoGrow               : bool

    /// max number of concurrent socket operations
    maxOps                 : int

    /// MIME types
    mimeTypesMap          : MimeTypesMap

    /// Home or root directory
    homeFolder             : string option

    /// Folder for temporary compressed files
    compressedFilesFolder  : string option

    /// Suave's logger. You can override the default instance if you wish to
    /// ship your logs, e.g. using https://www.nuget.org/packages/Logary.Adapters.Suave/
    /// Also, this logger will be configured by default for Suave unless you
    /// explicitly use `Suave.Logging.Global.initialise` before starting the
    /// web server (the first time – the second time, the static will already
    /// have been initialised).
    logger                : Logger

    /// Pluggable TCP async sockets implementation. You can choose betwee libuv
    /// and CLR's Async Socket Event Args. Currently defaults to the managed-only
    /// implementation.
    tcpServerFactory      : TcpServerFactory

    /// The cookie serialiser to use for converting the data you save in cookies
    /// from your application into a byte array.
    cookieSerialiser      : CookieSerialiser

    /// A TLS provider implementation.
    tlsProvider           : TlsProvider

    /// Make this true, if you want Suave not to display its server header in
    /// every response. Defaults to false.
    hideHeader            : bool

    /// Maximun upload size in bytes
    maxContentLength      : int }
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

### Changing the Default Configuration

`defaultConfig` (defined in `Suave.Web`) has sane defaults, and for many users,
these will be fine. However, since `SuaveConfig` is a record type, it is easy
to swap out one or more of the default settings, tweaking the configuration to
your precise needs. While we will discuss the default values below, you can
review the defaults at the bottom of
[Web.fs](https://github.com/SuaveIO/suave/blob/master/src/Suave/Web.fs).

If you're looking to get started quickly, you can jump straight to sections
detailing how to
[change the IP addres or port](#changing-the-servers-ip-address-or-port),
[specify a home directory](#changing-the-home-folder), and
[set the server's cryptography key](#changing-the-servers-cryptography-key). For
those whose descriptions are prefixed with "_(advanced)_", there is the
potential to degrade Suave's performance; you should take great care when
changing these from their default values.

#### Changing the server's IP address or port

The default binding for Suave is http://127.0.0.1:8080. It is rather simple to
change that, though.

{% highlight fsharp %}
let myCfg =
  { defaultConfig with
      bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" 8082 ]
    }
{% endhighlight %}

As `bindings` is a list, you can also configure Suave to listen on any
combination of IP addresses and ports.

#### Changing the server's cryptography key

Suave encrypts state stored in [sessions](sessions.html), and the key used for
that is the `serverKey` configuration setting. This key is required to be a
specific length (256 bits as of this writing), so there is a `ServerKey` module
that helps ensure that the key is the proper length.  The importance of this
key, how to generate one, and how to plug it into your Suave config can be found
under the [Server Keys](/sessions.html#server-keys) heading on that page.

While the examples all demonstrate using a base64-encoded string, if you have
256 bits already in a byte array that you want to use as a server key, you can
use `ServerKey.validate` instead of `ServerKey.fromBase64`; it will ensure that
the key is the proper length.

#### Changing the error handler

Suave's default error handler logs the error, then returns an HTTP 500
response. For local requests, it returns the error and stack trace in the body
of the response; for others, it returns "Internal Server Error". The best way
to customize it would be to start with `defaultErrorHandler`
[near the top of Web.fs](https://github.com/SuaveIO/suave/blob/master/src/Suave/Web.fs),
and tweak it to your liking. Then...

{% highlight fsharp %}
let myCfg =
  { defaultConfig with
      errorHandler = myErrorHandler
    }
{% endhighlight %}

#### Changing the listen timeout

_(advanced)_ This is the `TimeSpan` Suave will wait, on startup, for its request
to bind to a specific TCP port to be successful. The default value is for
`listenTimeout` is 2 seconds.

#### Changing the cancellation token

As with any asynchronous process, Suave can be controlled by the cancellation
token used to start the process. By default, it uses the default cancellation
token; however, giving `cancellationToken` a specific value here will allow for
scenarios such as stopping and restarting Suave programmatically.

#### Changing the buffer size

_(advanced)_ The `bufferSize` that is used for socket operations (low-level
communications). Its default value is 8192 bytes (8KB).

#### Changing whether the buffer can automatically grow

_(advanced)_ This boolean specifies whether the buffer manager is allowed to
grow itself; `autoGrow` defaults to `true`.

#### Changing the maximum concurrent operations

_(advanced)_ This is the maximum number of concurrent socket operations that
Suave will attempt to serve. The default value for `maxOps` is 100.

#### Changing the MIME type map

The `Writers` module has a default MIME type map, and that is the default map
in the configuration.  Suave will not serve a file for which it cannot determine
a MIME type, so if you are serving files that are not
[current in the MIME type map](https://github.com/SuaveIO/suave/blob/master/src/Suave/Combinators.fs#L90),
you will need to add this type.

The above paragraph uses the word "map" several times, but it's technically a
mapping function; to modify it, you need a function of your own.  As of this
writing, .iso files are not in the default mapping.  Here's how we could add it.

{% highlight fsharp %}
let myMimeTypesMap ext =
  match Writers.defaultMimeTypesMap ext with
  | Some mime -> mime
  | _ ->
      match ext with
      | ".iso" -> createMimeType "application/octet-stream" false
      | _ -> None

// and then

let myCfg =
  { defaultConfig with
      mimeTypesMap = myMimeTypesMap
    }
{% endhighlight %}

#### Changing the home folder

Suave does not have a default home folder.  If you want to serve files from
a folder, just specify it as a `Some` string.  If an absolute path is not given,
it is interpreted from the current working directory when Suave was started.
For example, if we follow the .NET Core convention of putting our
publicly-available files under `wwwroot`, this example sets it as the home
folder.

{% highlight fsharp %}
let myCfg =
  { defaultConfig with
      homeFolder = Some "./wwwroot"
    }
{% endhighlight %}

#### Changing the compressed files folder

Suave writes temporary files to disk when performing compression; the default
directory where these files are placed is the current working directory when
Suave was started.  However, if you want these files to go somewhere specific,
you can specify them the exact same way we did above for the home folder; just
set the `compressedFilesFolder` field instead.  (Suave deletes these files once
they are served, so their lifetime is usually less than a second.)

#### Changing logging options

A good overview can be found [on the logging page](logs.html).

#### Changing the default TCP server

Suave's default TCP server uses the .NET CLR's socket implementation to bind
and listen. Suave also comes with a TCP server implementation based on
[LibUV](https://github.com/libuv/libuv); to use it...

{% highlight fsharp %}
open Suave.LibUv

let myCfg =
  { defaultConfig with
      tcpServerFactory = new LibUvServerFactory()
    }
{% endhighlight %}

#### Changing the default cookie serializer

By default, Suave uses a binary formatter to serialize a `Map<string, obj>`
into a string that is encrypted and used as a cookie payload. On the "State and
Sessions" page, there is an
[example of swapping out the default cookie serializer for one based on JSON.NET](/sessions.html#cookie-serializationbrof-particular-interest-to-net-core--netstandard20).

#### Changing the default TLS provider

_(advanced)_ The TLS provider supports encrypted communications (HTTPS). The
default is an instance of the `DefaultTlsProvider` class; customizing it would
require implementing the `TlsProvider` interface, and providing an instance of
that class in your configuration's `tlsProvider` field.

#### Changing whether the header is shown

By default, Suave adds a `Server` header to each response, identifying itself
as the software that handled the request. If this behavior is not desired, you
can set `hideHeader` to `true`.

#### Changing the maximum file upload size

The `maxContentLength` field controls the maximum allowed upload size, in bytes;
its default is 10000000 (10 MiB).
