Introduction
============

Suave is a lightweight, non-blocking web server. The non-blocking I/O model is efficient and suitable for building fast, scalable network applications. In fact, Suave is written in a **completely non-blocking** fashion throughout. Suave **runs on Linux**, OS X and Windows flawlessly.

Suave is inspired in the simplicity of Happstack and born out of the necessity
of embedding web server capabilities in my own applications.  Still in its early
stages Suave supports HTTPS, multiple TCP/IP bindings, Basic Access
Authentication, Keep-Alive and HTTP compression.

NuGet
-----

To install Suave, run the following command in the
[Package Manager Console](http://docs.nuget.org/docs/start-here/using-the-package-manager-console)

{% highlight dosbatch %}
PM> Install-Package Suave
{% endhighlight %}

Or with [Paket](https://github.com/fsprojects/Paket) in paket.dependencies:

{% highlight dosbatch %}
source https://nuget.org/api/v2
nuget Suave 0.16.0
{% endhighlight %}

The simplest possible application: Hello World!
-----------------------------------------------

The simplest Suave application is a simple HTTP server that greets all visitors
with the string `"Hello World!"`

{% highlight fsharp %}
open Suave                 // always open suave
open Suave.Http.Successful // for OK-result
open Suave.Web             // for config

startWebServer defaultConfig (OK "Hello World!")
{% endhighlight %}

The above statement will start a web server on default port 8083 over HTTP.
`startWebServer` takes a configuration record and the WebPart `(OK "Hello World")`
It's worth noting that with the above, your application will block on the
function call, until you cancel the `Async.DefaultCancellationToken`. If you
want to handle disposal of the async yourself, have a look at
`startWebServerAsync`.

In suave, we have opted to write a lot of documentation inside the code; so just
hover the function in your IDE or use an assembly browser to bring out the XML
docs. 

Suave + Paket = ♥
-----------------
Working fully self-contained getting-started example for Suave Web Server
scripting

Note you don't need to have _anything_ installed before starting with this
script. Nothing but F# Interactive and this script.

This script fetches the Paket.exe component which is referenced later in the
script.  Initially the #r "paket.exe" reference is shown as unresolved. Once it
has been downloaded by the user (by executing the first part of the script) the
reference shows as resolved and can be used.

Paket is then used to fetch a set of F# packages, which are then used later inn
the script.

{% highlight fsharp %}
// Step 0. Boilerplate to get the paket.exe tool
 
open System
open System.IO
 
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
 
if not (File.Exists "paket.exe") then
let url = "https://github.com/fsprojects/Paket/releases/download/0.31.5/paket.exe"
use wc = new Net.WebClient() in let tmp = Path.GetTempFileName() in wc.DownloadFile(url, tmp); File.Move(tmp,Path.GetFileName url)
 
// Step 1. Resolve and install the packages
 
#r "paket.exe"
 
Paket.Dependencies.Install """
source https://nuget.org/api/v2
nuget Suave
nuget FSharp.Data
nuget FSharp.Charting
""";;
 
// Step 2. Use the packages
 
#r "packages/Suave/lib/net40/Suave.dll"
#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "packages/FSharp.Charting/lib/net40/FSharp.Charting.dll"
 
let ctxt = FSharp.Data.WorldBankData.GetDataContext()
 
let data = ctxt.Countries.Algeria.Indicators.``GDP (current US$)``
 
open Suave // always open suave
open Suave.Http.Successful // for OK-result
open Suave.Web // for config
 
startWebServer defaultConfig (OK (sprintf "Hello World! In 2010 Algeria earned %f " data.[2010]))
{% endhighlight %}

A slightly more complex example: routing HTTP requests
----------------------------------------------------------

Most .NET web frameworks are based on the object-oriented paradigm. This means that you have a class called something like CheesesController or CheesesModule. The methods in that class are then associated with url routes, with the most common routing methods being convention based, attribute-based (e.g. adding attributes like `[Route("cheeses/{cheeseId}/taste")]` and `[HttpGet]`) or some sort of global routing table.

Suave is designed to fit in with F#'s functional programming paradigm, so routing does not use any of these routing methods. Instead routing takes place using a single function, itself composed from many smaller functions. This function has the signature `WebPart`:

{% highlight fsharp %}
type SuaveTask<'a> = Async<'a option>
type WebPart = HttpContext -> SuaveTask<HttpContext>
// hence: WebPart = HttpContext -> Async<HttpContext option>
{% endhighlight %}

This function has a single parameter of type `HttpContext`. This is an F# record type that includes the HTTP request, the HTTP response, and a few other things. This should be pretty familiar to anyone who has done any web programming before.

A WebPart function returns an asynchronous workflow which itself ultimately returns an `HttpContext option`. Asynchronous workflows are hopefully already somewhat familiar. The `HttpContext option` returned by the asynchronous workflow is either `Some HttpContext` record or `None`, and it is the option that is used to determine routing. Here is a web server with some simple routing logic. You can ignore the `>>=` operator and the exact workings of `choose` for now (both are explained in more detail later), just focus on the tree-like structure of the code:

{% highlight fsharp %}        
open Suave
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Successful
open Suave.Web

let app =
  choose
    [ GET >>= choose
        [ path "/hello" >>= OK "Hello GET"
          path "/goodbye" >>= OK "Good bye GET" ]
      POST >>= choose
        [ path "/hello" >>= OK "Hello POST"
          path "/goodbye" >>= OK "Good bye POST" ] ]

startWebServer defaultConfig app
{% endhighlight %}

By using `choose` we execute different logic depending on whether the request was a GET or a POST, and depending on whether the url was /hello or /goodbye. If a request matches a given path in the decision tree, `choose` will return `Some HttpContext`, if it doesn't, `choose` will return `None`. The end result is that when someone makes a request the server will walk down this tree looking for the first part that returns `Some HttpContext`, and then return it to the client. If no part of the tree returns `Some HttpContext` then the result is an exception. You can can also add a default route which returns a 404 page.

The server won't evalute the entire data structure for every request, only the actual decisions, so there is no need to be concerned about performance.

Async computation expressions and >>=
-------------------------------------

Suave WebParts use F# async computation expressions. For example you could write:

{% highlight fsharp %}
let sleep milliseconds message: WebPart =
  fun (x : HttpContext) ->
    async {
      do! Async.Sleep milliseconds
      return! OK message x
    }
{% endhighlight %}

Async computation expressions are built into the F# language as a way of chaining asynchronous functions together. As well as `do!` and `return!` you can use `let!` to wait for another async function to return and then assign the result to a variable. For instance, `let! result = anotherAsyncWorkflow`. Behind the scenes "let!" is just syntactic sugar for calling the method `builder.Bind` on a computation builder.

F# lets you define your own custom computation expressions with their own implementation of `Bind`, but instead of defining a custom computation builder with its own implementation of `Bind` Suave chooses to define the `>>=` operator. This is the standard operator for the `bind` operation in some other languages. So `>>=` chains asynchronous expressions in almost the same way as `let!`, except the expressions are not of type `Async<'b>` and `Async<'c>`, but rather `Async<'b option>` and `Async<'c option>`. That is to say, `>>=`
chains together Async options rather than just vanilla Async computations. If the result of the first of the two chained workflows is `None`, then the computation is short-circuited and the second computation is never run. If the first computation returns `Some x`, then `>>=` behaves in much the same way as `let!`: the result of running the expression on the left is passed into the expression on the right.

If you hear someone use the "M" word, they are referring to code that uses the `>>=` operator.

There is a good tutorial on using the `>>=` operator to short circuit a series of operations here: [Railway oriented programming](http://fsharpforfunandprofit.com/posts/recipe-part2/)

There is also a good tutorial on computation expressions by the same author: [Computation expressions: Introduction](http://fsharpforfunandprofit.com/posts/computation-expressions-intro/)

Composing bigger programs: combinators
--------------------------------------------

Defining the entire logic of your program in a single giant function called app would clearly be impossible. Functional programming is all about composing functions from several smaller functions, and both F# and Suave offer various tools to make this easy.

In functional programming parlance, a "combinator" either combines several things of the same type into another thing of the same type, or otherwise takes a value and returns a new, modified version of that value. In mathematics it has a slightly different meaning, but we need not worry about this. In the case of Suave, there are two types of combinator:

- Combinators which combine multiple `WebPart` into a single `WebPart`.
- Combinators that produce `WebPart` from more primitive values. Recall that `WebPart` has the type `HttpContext -> Async<HttpContext option>`. These combinators therefore always take a single HttpContext and produce a new HttpContext, wrapped inside an async option workflow.

Together these are used to create web parts, combine them to produce new webparts, and ultimately combine them all into a single webpart passed as an argument used to initialise the web server.

We have already seen several examples of combinators. The `choose` function seen above takes a list of `WebPart`, and combines them all into a single new `WebPart`:

{% highlight fsharp %}
val choose : (options : WebPart list) -> WebPart
{% endhighlight %}

The `choose` combinator is implemented such that it will execute each webpart in the list until one returns success.

`>>=` is also a combinator, one that combines exactly two web parts. It runs the first, waits for it to finish, and then either passes the result into the second part, or short circuits if the first part returns `None`.

`OK` is a combinator of the second type. It always succeeds and writes its argument to the underlying response stream. It has type `string -> WebPart`.

To gain access to the underlying `HttpRequest` and read query and http form data we can use the `request` combinator (the `^^` custom operator is shorthand for searching a list of key-value option pairs and returning the value (or None if not found)):

{% highlight fsharp %}
let greetings q =
  defaultArg (q ^^ "name") "World" |> sprintf "Hello %s"

let sample : WebPart = 
    url "/hello" >>= choose [
      GET  >>= request(fun r -> OK <| greetings (query r))
      POST >>= request(fun r -> OK <| greetings (form r))
      NOT_FOUND "Found no handlers" ]
{% endhighlight %}

You can similarly use `context` to gain access to the full `HttpContext` and connection.

To protect a route with HTTP Basic Authentication the combinator `authenticateBasic` is used like in the following example.

{% highlight fsharp %}
let requiresAuthentication _ =
  choose
    [ GET >>= url "/public" >>= OK "Hello anonymous"
      // access to handlers after this one will require authentication
      authenticateBasic (fun (user, pass) -> user = "foo" && pass = "bar")
      GET >>= url "/protected" >>= context (fun x -> OK ("Hello " + x.userState.["userName"])) ]
{% endhighlight %}

Your web parts are "values" in the sense that they evaluate
once, e.g. when constructing `choose [ OK "hi" ]`, `OK "hi"` is evaluated once,
not every request. You need to wrap your web part in a closure if you want to
re-evaluated every request, with `Suave.Http.warbler`, `Suave.Types.context` or
`Suave.Types.request`.

`warbler : (f : 'a -> 'a -> 'b) -> 'a -> 'b` - a piece of the applicatives
puzzle, which allows you to act on the `'a` argument and return a function that
'is the same' as after your acting on it. Using this is very useful for control
flow, because you can then inspect `HttpContext` and choose what applicative
function to return.

`context`: basically the same as warbler.

`request`: basically the same as context, but only looks at the request - allows
you to cut down on the pattern matching of HttpContext a bit: but you have to
return an applicative that is a WebPart (i.e. something that isn't from
HttpRequest to something else, but from HttpContext to async http context
option).

More Applicatives and HTTP combinators
---------------------------------

The documentation for applicates is written in
[Https.fsi](https://github.com/SuaveIO/suave/blob/master/Suave/Http.fsi) - so
it's recommended you explore the module using the code-completion of your IDE,
or by looking at the linked file.

All-in-all, the Http module consists of these sub-modules:

<dl>
<dt>Codes</dt>
<dd>simply hosts the HttpCode type.</dd>
<dt>Internals</dt>
<dd>constants and version of suave library.</dd>
<dt>Compression</dt>
<dd>Functions for compressing responses.</dd>
<dt>Response</dt>
<dd>response and response_f functions.</dd>
<dt>Writers</dt>
<dd>ways to modify the response.</dd>
<dt>Intermediate - </dt>
<dd>100 and 101 response codes.</dd>
<dt>Successful</dt>
<dd>2xx response codes.</dd>
<dt>Redirection</dt>
<dd>3xx response codes.</dd>
<dt>RequestErrors - </dt>
<dd>4xx response codes.</dd>
<dt>ServerErrors</dt>
<dd>5xx response codes.</dd>
<dt>Applicatives</dt>
<dd>use to filter down the request to something you</dd>
<dt>Files</dt>
<dd>send files to the client</dd>
<dt>Authentication</dt>
<dd>Methods for authenticating http requests</dd>
</dl>

If you can get the FSFormatting project to work, we would appreciate a PR with
generated documentation from the Http module as we've literally transcribed the
RFC that documents all HTTP result codes, into this file.

The numeric/discriminated-union HTTP codes can be found in `Suave.Types.Codes`.

Programming with fish (custom operator reference)
----------------------------------------

Functional programming tends to involve custom operators. An excessive number of custom operators makes for cryptic, illegible code, but a few well chosen ones allow logic to be more succint and readable. We have already seen `>>=`, and happily suave does not use either ><<*> or <*)))>{

The other custom operators it declares are:

| Operator | Description |
| ---------|-------------|
|>=>       | Left-to-right Kleisli composition of monads, see Http.fsi
|<&#124;>  | Left-to-right Kleisli composition of web parts, see Http.fsi
|?         | Try find a value by key in a dictionary
|%%        | Search a list of key-value pairs and return the value (or None if not found)
|^^        | Search a list of key-value option pairs and return the value (or None if not found)
|?<-       | Assign a value to the key in the dictionary

Typed routes
------------

{% highlight fsharp %}
let testapp : WebPart =
  choose
    [ urlScan "/add/%d/%d" (fun (a,b) -> OK((a + b).ToString()))
      NOT_FOUND "Found no handlers" ]
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
  { defaultConfig with
      bindings =
        [ HttpBinding.mk HTTP IPAddress.Loopback 80us
          HttpBinding.mk (HTTPS (openSsl sslCert)) (IPAddress.Parse "0.0.0.0") 443us
        ]
      timeout = TimeSpan.FromMilliseconds 3000. }
choose
  [ url "/hello" >>= OK "Hello World"
    NOT_FOUND "Found no handlers" ]
|> startWebServer cfg
{% endhighlight %}

The OpenSSL implementation comes with conditional bindings in
App.config for the three operating systems: linux, OS X and Windows.

**Note** -- currently the compiled versions are "gott och blandat" as we say in
Swedish. It means some are compiled for x86 (Windows) and some x64 (Linux, OS
X); check out [issue 42](https://github.com/SuaveIO/suave/issues/42) to see more
about this issue.

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

## Serving static files, HTTP Compression and MIME types

Suave supports **gzip** and **deflate** http compression encodings.  Http
compression is configured via the MIME types map in the server configuration
record. By default Suave does not serve files with extensions not registered in
the mime types map.

The default mime types map `defaultMimeTypesMap` looks like this.

{% highlight fsharp %}
let defaultMimeTypesMap = function
  | ".css" -> mkMimeType "text/css" true
  | ".gif" -> mkMimeType "image/gif" false
  | ".png" -> mkMimeType "image/png" false
  | ".htm"
  | ".html" -> mkMimeType "text/html" true
  | ".jpe"
  | ".jpeg"
  | ".jpg" -> mkMimeType "image/jpeg" false
  | ".js"  -> mkMimeType "application/x-javascript" true
  | _      -> None
{% endhighlight %}

You can register additional MIME extensions by creating a new mime map in the following fashion.

{% highlight fsharp %}
// Adds a new mime type to the default map
let mimeTypes =
  defaultMimeTypesMap
    >=> (function | ".avi" -> mkMimeType "video/avi" false | _ -> None)

let webConfig = { defaultConfig with mimeTypesMap = mimeTypes }
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


Getting Hold of Suave's Logs
----------------------------

When you are using suave you will probably want to funnel all logs from the
output to your own log sink. We provide the interface `Logger` to do that; just
set the propery `logger` in the configuration to an instance of your thread-safe
logger. An example:

{% highlight fsharp %}
type MyHackLogger(minLevel) =
  interface Logger with
    member x.Log level fLine =
      if level >= minLevel then
        // don't do this for real ;)
        System.Windows.Forms.MessageBox.Show((fLine ()).message)
{% endhighlight %}

You can use Logary for integrated logging:

{% highlight dosbatch %}
Install-Package Logary.Adapters.Suave
{% endhighlight %}

Use the `SuaveAdapter` type to set the Logger in Suave's configuration:

{% highlight fsharp %}
open Suave.Logging

use logary =
  withLogary' "logibit.web" (
    withTargets [
      Console.create Console.empty "console"
      Debugger.create Debugger.empty "debugger"
    ] >>
    withMetrics (Duration.FromMilliseconds 5000L) [
      WinPerfCounters.create (WinPerfCounters.Common.cpuTimeConf) "wperf"
(Duration.FromMilliseconds 300L)
      ] >>
      withRules [
        Rule.createForTarget "console"
        Rule.createForTarget "debugger"
      ]
    )

let webConfig =
  { defaultConfig with
      logger   = SuaveAdapter(logary.GetLogger "suave")
  }
{% endhighlight %}

Deploying Suave to Heroku
----------------------------

Suave web sites can be as simple as a single F# script which starts a web server, or a full project.  

Your application needs to be either a single script ``app.fsx`` (plus an heroku ``Procfile`` and ``dummy.sln`` file) OR 
a directory with a ``.sln`` solution  (plus an heroku ``Procfile``)
	
Optionally, you can have a ``paket.dependencies`` OR ``packages.config`` files

Either way, your application  must start a web server that binds to 0.0.0.0:$PORT.
   
Your ``Procfile`` must specify how the application starts.

If you don't have an app.fsx already that implements your website, then clone an example, putting it in a new directory (replace myproj by a unique project name)


1. [Install the Heroku Toolbelt](https://toolbelt.heroku.com/) and login to Heroku using the command-line tools:

       heroku login

2. Clone the sample:

       git clone https://github.com/SuaveIO/heroku-getting-started.git myproj
       cd myproj

3. Create a new heroku web app and register "heroku" as a remote you can push to:

       heroku create myproj --buildpack https://github.com/SuaveIO/mono-script-buildpack.git 

4. Push!

        git push heroku master  

When pushing, use an empty user name. You may need to use ``git auth:token`` to get a app token to use as a password here.

You can change the buildpack being used at a later date (e.g. to update to a later version of Mono) using 

    heroku buildpack:set https://github.com/your-build-pack-repo

If using github or bitbucket, find your app on Heroku and enable automatic deploy so you don't need to push explciitly.

You can look at logs from your web server script using ``heroku logs``.

