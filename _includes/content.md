Introduction.
=============

Suave is a simple web development F# library providing a lightweight web server and a set of combinators to manipulate route flow and task composition. Suave is inspired in the simplicity of Happstack and born out of the necessity of embedding web server capabilities in my own applications. 
Still in its early stages Suave supports HTTPS, multiple TCP/IP bindings, Basic Access Authentication, Keep-Alive. Suave also takes advantage of F# asynchronous workflows to perform non-blocking IO. In fact, Suave is written in a completely non-blocking fashion throughout.

What follows is a tutorial on how to create applications. Scroll past the tutorial to see detailed function documentation.

Tutorial: Hello World!
----------------------

The simplest Suave application is a simple HTTP server that greets all visitors with the string `"Hello World!"`

{% highlight fsharp %}
web_server default_config (OK "Hello World!")
{% endhighlight %}

The above statement will start a web server on default port 8083. `web_server` takes a configuration record and the webpart `(OK "Hello World")` 

Webparts are functions with the following type: `HttpRequest -> Option<Async<unit>>`. For every request `web_server` will evaluate the webpart, if the evaluation succeeds it will execute the resulting asynchronous computation. `OK` is a combinator that always succeed and writes its argument to the underlying response stream.

Tutorial: Composing bigger programs.
------------------------------------

Logic is expressed with the help of different combinators built around the `option<HttpRequest>` type. We build webparts out of functions of type `HttpRequest -> HttpRequest option` and the operator `>>=` in the following way.

{% highlight fsharp %}
let simple_app _ = url "/hello" >>= OK "Hello World" ;
{% endhighlight %}

To select between different routes or options we use the function choose; for example:

{% highlight fsharp %}
let complex_app _ = 
  choose
    [ Console.OpenStandardOutput() |> log >>= never
    ; url "/hello" >>= never >>= OK "Never executes"
    ; url "/hello" >>= OK "Hello World" ]
{% endhighlight %}

The function `choose` accepts a list of webparts and execute each webpart in the list until one returns success. Since choose itself returns a webpart we can nest them for more complex logic.

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

To gain access to the underlying `HttpRequest` and read query and http form data we can use the warbler* combinator.

{% highlight fsharp %}
let http_form _ = 
  choose
    [ GET  >>= url "/query" >>= warbler(fun x -> OK ("Hello " + (x.Query) ? name))
    ; POST >>= url "/query" >>= warbler(fun x -> OK ("Hello " + (x.Form)  ? name))
    ; notfound "Found no handlers" ]
{% endhighlight %}

or alternatively with `>>== = warbler`

{% highlight fsharp %}
let http_form _ = 
  choose
    [ GET  >>= url "/query" >>== (fun x -> OK ("Hello " + (x.Query) ? name))
    ; POST >>= url "/query" >>== (fun x -> OK ("Hello " + (x.Form)  ? name))
    ; notfound "Found no handlers" ]
{% endhighlight %}

Here is how http authentication would work:

{% highlight fsharp %}
let requires_authentication _ = 
  choose
    [ GET >>= url "/public" >>= OK "Hello anonymous"
    //access to handlers bellow this one will require authentication
    ; authenticate_basic (fun x -> x.Username.Equals("foo") && x.Password.Equals("bar"))
    ; GET >>= url "/protected" >>== (fun x -> OK ("Hello " + x.Username)) ]
{% endhighlight %}

*warbler gets its name from the famous book "To Mock a Mockingbird" by Raymond Smullyan.

Typed routes
------------

{% highlight fsharp %}
let testapp : WebPart = 
  choose
    [ urlscan "/add/%d/%d" (fun (a,b) -> OK((a + b).ToString()))
    ; notfound "Found no handlers" ]
{% endhighlight %}

Multiple bindings and SSL support
---------------------------------

Suave supports binding the application to multiple TCP/IP addresses and ports combinations. It also supports HTTPS.

{% highlight fsharp %}
let sslCert = new X509Certificate("suave.pfx","easy");
let cfg = 
  { default_config with
      bindings =
        [ { scheme = HTTP
          ; ip     = IPAddress.Parse "127.0.0.1"
          ; port   = 80us }
        ; { scheme = HTTPS sslCert
          ; ip     = IPAddress.Parse "192.168.13.138"
          ; port   = 443us } ]
    ; timeout = TimeSpan.FromMilliseconds 3000. }
choose 
  [ Console.OpenStandardOutput() |> log >>= never // log to standard output
  ; url "/hello" >>= OK "Hello World"
  ; notfound "Found no handlers" ]
|> web_server cfg
{% endhighlight %}

API
===

Default-supported HTTP Verbs
----------------------------

See "RFC 2616":http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html.

{% highlight fsharp %}
/// Match on GET requests
let GET     (x : HttpRequest) = meth0d "GET" x
/// Match on POST requests
let POST    (x : HttpRequest) = meth0d "POST" x
/// Match on DELETE requests
let DELETE  (x : HttpRequest) = meth0d "DELETE" x
/// Match on PUT requests
let PUT     (x : HttpRequest) = meth0d "PUT" x
/// Match on HEAD requests
let HEAD    (x : HttpRequest) = meth0d "HEAD" x
/// Match on CONNECT requests
let CONNECT (x : HttpRequest) = meth0d "CONNECT" x
/// Match on PATCH requests
let PATCH   (x : HttpRequest) = meth0d "PATCH" x
/// Match on TRACE requests
let TRACE   (x : HttpRequest) = meth0d "TRACE" x
/// Match on OPTIONS requests
let OPTIONS (x : HttpRequest) = meth0d "OPTIONS" x
{% endhighlight %}

Server configuration
--------------------

The first argument to `web_server` is a configuration record with the following signature.

{% highlight fsharp %}
/// Gets the supported protocols, HTTP and HTTPS with a certificate
type Protocol =
  /// The HTTP protocol is the core protocol
  | HTTP
  /// The HTTP protocol tunneled in a TLS tunnel
  | HTTPS of X509Certificate
{% endhighlight %}

{% highlight fsharp %}
/// A HTTP binding is a protocol is the product of HTTP or HTTP, a DNS or IP binding and a port number
type HttpBinding =
  /// The scheme in use
  { scheme : Protocol
  /// The host or IP address to bind to. This will be interpreted by the operating system
  ; ip     : IPAddress
  /// The port for the binding
  ; port   : uint16 }
{% endhighlight %}

{% highlight fsharp %}
/// The core configuration of suave
type Config =
  /// The bindings for the web server to launch with
  { bindings      : HttpBinding list
  /// An error handler to use for handling exceptions that are
  /// are thrown from the web parts
  ; error_handler : ErrorHandler
  /// Timeout for responses to be generated
  ; timeout       : TimeSpan
  /// A cancellation token for the web server. Signalling this token
  /// means that the web server shuts down
  ; ct            : CancellationToken }
{% endhighlight %}

*bindings* array of bindings of the form _protocol, ip address, port_
*error_handler* a handler to deal with runtime errors
*timeout* maximun number of milliseconds before killing a request
