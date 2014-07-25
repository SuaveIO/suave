Introduction
============

Suave is inspired in the simplicity of Happstack and born out of the necessity
of embedding web server capabilities in my own applications.  Still in its early
stages Suave supports HTTPS, multiple TCP/IP bindings, Basic Access
Authentication, Keep-Alive and HTTP compression.

Suave also takes advantage of F# asynchronous workflows to perform non-blocking
IO. In fact, Suave is written in a **completely non-blocking** fashion
throughout. Suave **runs on Linux**, OS X and Windows flawlessly.

What follows is a tutorial on how to create Suave applications. Scroll past the
tutorial to see detailed function documentation.

NuGet
-----

To install Suave, run the following command in the
[Package Manager Console](http://docs.nuget.org/docs/start-here/using-the-package-manager-console)

{% highlight dosbatch %}
PM> Install-Package Suave
{% endhighlight %}

Tutorial: Hello World!
----------------------

The simplest Suave application is a simple HTTP server that greets all visitors
with the string `"Hello World!"`

{% highlight fsharp %}
open Suave.Http
open Suave.Web

web_server default_config (OK "Hello World!")
{% endhighlight %}

The above statement will start a web server on default port 8083 over HTTP.
`web_server` takes a configuration record and the webpart `(OK "Hello World")` 

Webparts are functions with the following type:
`HttpContext -> HttpContext option`.

For every request `web_server` will evaluate the WebPart, if the evaluation
succeeds it will send the calculated response back to the http client. 

`OK` is a combinator that always succeed and writes its argument to the
underlying response stream.

Something that may trick beginners up is that your web parts are "values" in the
sense that they evaluate once, e.g. when constructing `choose [ OK "hi" ]`, `OK
"hi"` is evaluated once, not every request. You need to wrap your web part in a
closure if you want to re-evaluated every request, with e.g. `warbler`.

Tutorial: Composing bigger programs
-----------------------------------

Logic is expressed with the help of different combinators built around the
`HttpContext option` type. We build webparts out of functions of type
`HttpContext -> HttpContext option` and the operator `>>=` in the following way.

{% highlight fsharp %}
let simple_app _ = url "/hello" >>= OK "Hello World" ;
{% endhighlight %}

To select between different routes or options we use the function choose; for
example:

{% highlight fsharp %}
let complex_app _ = 
  choose
    [ Console.OpenStandardOutput() |> log >>= never
    ; url "/hello" >>= never >>= OK "Never executes"
    ; url "/hello" >>= OK "Hello World" ]
{% endhighlight %}

The function `choose` accepts a list of webparts and execute each webpart in the
list until one returns success. Since choose itself returns a webpart we can
nest them for more complex logic.

{% highlight fsharp %}
let nested_logic _ =
  choose
    [ GET >>= choose 
        [ url "/hello" >>= OK "Hello GET"
        ; url "/goodbye" >>= OK "Good bye GET" ]
    ; POST >>= choose 
        [ url "/hello" >>= OK "Hello POST"
        ; url "/goodbye" >>= OK "Good bye POST" ] ]
{% endhighlight %}

To gain access to the underlying `HttpRequest` and read query and http form data
we can use the `request` combinator.

{% highlight fsharp %}
let http_form _ = 
  choose
    [ GET  >>= url "/query" >>= request(fun x -> OK ("Hello " + (opt <| x.query ? name)))
    ; POST >>= url "/query" >>= request(fun x -> OK ("Hello " + (opt <| x.form  ? name)))
    ; NOT_FOUND "Found no handlers" ]
{% endhighlight %}

You can similarly use `warbler` to gain access to the full `HttpContext` and
connection.

To protect a route with HTTP Basic Authentication the combinator
`authenticate_basic` is used like in the following example.

{% highlight fsharp %}
let requires_authentication _ = 
  choose
    [ GET >>= url "/public" >>= OK "Hello anonymous"
    //access to handlers after this one will require authentication
    ; authenticate_basic (fun x -> x.Username.Equals("foo") && x.Password.Equals("bar"))
    ; GET >>= url "/protected" >>= request(fun x -> OK ("Hello " + x.Username)) ]
{% endhighlight %}

Typed routes
------------

{% highlight fsharp %}
let testapp : WebPart = 
  choose
    [ url_scan "/add/%d/%d" (fun (a,b) -> OK((a + b).ToString()))
    ; NOT_FOUND "Found no handlers" ]
{% endhighlight %}

Multiple bindings and SSL support
---------------------------------

Suave supports binding the application to multiple TCP/IP addresses and ports
combinations. It also supports HTTPS via the interface `ITlsProvider`.

There is an OpenSSL implementation at [https://github.com/SuaveIO/suave/tree/master/suave.OpenSsl](https://github.com/SuaveIO/suave/tree/master/suave.OpenSsl)

{% highlight fsharp %}

open Suave.OpenSsl.Provider

let sslCert = new X509Certificate("suave.pfx","easy");
let cfg = 
  { default_config with
      bindings =
        [ { scheme = HTTP
          ; ip     = IPAddress.Parse "127.0.0.1"
          ; port   = 80us }
        ; { scheme = HTTPS <| open_ssl sslCert
          ; ip     = IPAddress.Parse "192.168.13.138"
          ; port   = 443us } ]
    ; timeout = TimeSpan.FromMilliseconds 3000. }
choose 
  [ log default_config.logger log_format >>= never // log to the default logger
    url "/hello" >>= OK "Hello World"
    NOT_FOUND "Found no handlers" ]
|> web_server cfg
{% endhighlight %}

The OpenSSL implementation comes with conditional bindings in
App.config for the three operating systems: linux, OS X and Windows.

**Note** -- currently the compiled versions are "gott och blandat" as we say in
Swedish. It means some are compiled for x86 (Windows) and some x64 (Linux, OS
X); check out https://github.com/SuaveIO/suave/issues/42 to see more about this
issue.

API
===

HttpContext
-----------

{% highlight fsharp %}
type ErrorHandler = Exception -> String -> HttpContext -> HttpContext

and HttpRuntime =
  { protocol           : Protocol
  ; error_handler      : ErrorHandler
  ; mime_types_map     : MimeTypesMap
  ; home_directory     : string
  ; compression_folder : string
  ; logger             : Log.Logger
  ; session_provider   : ISessionProvider }

and HttpContext =
  { request    : HttpRequest
  ; runtime    : HttpRuntime
  ; user_state : Map<string, obj>
  ; response   : HttpResult }

and ISessionProvider =
  abstract member Generate : TimeSpan * HttpContext -> string
  abstract member Validate : string * HttpContext -> bool
  abstract member Session<'a>  : string -> SessionStore<'a>
{% endhighlight %}

Default-supported HTTP Verbs
----------------------------

See "RFC 2616":http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html.

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

The first argument to `web_server` is a configuration record with the following signature.

{% highlight fsharp %}
/// The core configuration of suave. See also Suave.Web.default_config which
/// you can use to bootstrap the configuration:
/// <code>{ default_config with bindings = [ ... ] }</code>
type SuaveConfig =
  /// The bindings for the web server to launch with
  { bindings       : HttpBinding list

  /// An error handler to use for handling exceptions that are
  /// are thrown from the web parts
  ; error_handler  : ErrorHandler

  /// Timeout for responses to be generated
  ; timeout        : TimeSpan

  /// Timeout to wait for the socket bind to finish
  ; listen_timeout : TimeSpan

  /// A cancellation token for the web server. Signalling this token
  /// means that the web server shuts down
  ; ct             : CancellationToken

  /// Buffer size used for socket IO, default value is 8192 (8 Kilobytes)
  ; buffer_size    : int

  /// Maximun number of accept, receive and send operations the server can handle simultaneously
  /// default value is 100
  ; max_ops        : int

  /// MIME types, registry of file extensions Suave can serve and if they can be compressed
  ; mime_types_map : string -> MimeType option

  /// Home or root directory, the directory where content will be served from
  /// if no directory is specified the directory where suave.dll lives is assumed the home directory
  ; home_folder    : string option

  /// Folder for temporary compressed files, directory where to cache compressed static files
  /// if no directory is specified the directory where suave.dll lives is assumed
  ; compressed_files_folder : string option

  /// Http session provider
  ; session_provider : ISessionProvider }
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
  ; ip     : IPAddress
  /// The port for the binding
  ; port   : uint16 }

type MimeType = 
  /// The name of the mime type, i.e "text/plain"
  { name         : string
  /// If the server will compress the file when clients ask for gzip or 
  /// deflate in the `Accept-Encoding` header
  ; compression  : bool }
{% endhighlight %}

## Serving static files, HTTP Compression and MIME types

Suave supports **gzip** and **deflate** http compression encodings.  Http compression is configured via the MIME types map in the server configuration record. By default Suave does not serve files with extensions not registered in the mime types map.

The default mime types map `default_mime_types_map` looks like this.

{% highlight fsharp %}
let default_mime_types_map = function
    | ".css" -> mk_mime_type "text/css" true
    | ".gif" -> mk_mime_type "image/gif" false
    | ".png" -> mk_mime_type "image/png" false
    | ".htm"
    | ".html" -> mk_mime_type "text/html" true
    | ".jpe"
    | ".jpeg"
    | ".jpg" -> mk_mime_type "image/jpeg" false
    | ".js"  -> mk_mime_type "application/x-javascript" true
    | _      -> None
{% endhighlight %}

You can register additional MIME extensions by creating a new mime map in the following fashion.

{% highlight fsharp %}
// Adds a new mime type to the default map
let mime_types x =
  default_mime_types_map 
    >=> (function | ".avi" -> mk_mime_type "video/avi" false | _ -> None)
{% endhighlight %}

## Overview

A request life-cycle begins with the `HttpProcessor` that takes an `HttpRequest`
and the request as bytes and starts parsing it. It returns an `HttpRequest
option` that, if Some, gets run against the WebParts passed.

### The WebPart

A web part is a thing that acts on a HttpContext, the web part could fail by returning `None` or succeed and produce a new HttpContext

{% highlight fsharp %}
type WebPart = HttpContext -> HttpContext option
{% endhighlight %}

### The ErrorHandler

An error handler takes the exception, a programmer-provided message, a request
(that failed) and returns a web part for the handling of the
error.

{% highlight fsharp %}
type ErrorHandler = Exception -> String -> HttpContext -> HttpContext
{% endhighlight %}


Replacing Suave's logging
-------------------------

When you are using suave you will probably want to funnel all logs from the
output to your own log sink. We provide the interface `Logger` to do that; just
set the propery `logger` in the configuration to an instance of your thread-safe
logger. An example:

{% highlight fsharp %}
type MyHackLogger(min_level) =
  interface Logger with
    member x.Log level f_line =
      if level >= min_level then
        // don't do this for real ;)
        System.Windows.Forms.MessageBox.Show((f_line ()).message)
{% endhighlight %}

You can use Logary for integrated logging:

{% highlight dosbatch %}
PM> Install-Package Intelliplan.Logary.Suave
{% endhighlight %}

Use the `SuaveAdapter` type to set the Logger in Suave's configuration.
