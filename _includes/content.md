Introduction.
=============

Suave is inspired in the simplicity of Happstack and born out of the necessity of embedding web server capabilities in my own applications. 
Still in its early stages Suave supports HTTPS, multiple TCP/IP bindings, Basic Access Authentication, Keep-Alive. Suave also takes advantage of F# asynchronous workflows to perform non-blocking IO.

Hello World!
------------

The simplest Suave application is a simple HTTP server that greets all visitors with the string `"Hello World!"`

{% highlight fsharp %}
web_server defaultConfig (OK "Hello World!")
{% endhighlight %}

The above statement will start a web server on default port 8083. `web_server` takes a configuration record and the webpart `(OK "Hello World")` 

Webparts are functions with the following type: `HttpRequest -> Option<Async<unit>>`. For every request web_server will evaluate the webpart, if the evaluation succeeds it will execute the resulting asynchronous computation. `OK` is a combinator that always succeed and writes its argument to the underlying response stream.

Composing bigger programs.
--------------------------

Logic is expressed with the help of different combinators built around the `option<HttpRequest>` type. We build webparts out of functions of type `HttpRequest -> HttpRequest option` and the operator `>>=` in the following way.

{% highlight fsharp %}
let simple_app _ = url "/hello" >>= OK "Hello World" ;
{% endhighlight %}

To select between different routes or options we use the function choose; for example:

{% highlight fsharp %}
let complex_app _ = 
    choose [
        Console.OpenStandardOutput() |> log >>= never; 
        url "/hello" >>= never >>= OK "Never executes";
        url "/hello" >>= OK "Hello World"  ;
    ]
{% endhighlight %}

The function `choose` accepts a list of webparts and execute each webpart in the list until one returns success. Since choose itself returns a webpart we can nest them for more complex logic.

{% highlight fsharp %}
let nested_logic _= 
    choose [
        GET >>= choose 
            [ url "/hello" >>= OK "Hello GET" ; url "/goodbye" >>= OK "Good bye GET" ];
        POST >>= choose 
            [ url "/hello" >>= OK "Hello POST" ; url "/goodbye" >>= OK "Good bye POST" ];
    ]
{% endhighlight %}

To gain access to the underlying `HttpRequest` and read query and http form data we can use the `warbler` combinator.

{% highlight fsharp %}
let http_form _ = 
    choose [
         GET  >>= url "/query" >>= warbler( fun x -> OK ("Hello " + (x.Query) ? name));
         POST >>= url "/query" >>= warbler( fun x -> OK ("Hello " + (x.Form)  ? name));
         notfound "Found no handlers"
    ]
{% endhighlight %}

Here is how http authentication would work:

{% highlight fsharp %}
let requires_authentication _ = 
    choose [
         GET >>= url "/public" >>= OK ("Hello anonymous");
         //access to handlers bellow this one will require authentication
         authenticate_basic ( fun x -> x.Username.Equals("foo") && x.Password.Equals("bar"));
         GET >>= url "/protected" >>= warbler( fun x -> OK ("Hello " + x.Username));
    ]
{% endhighlight %}

`warbler` gets its name from the famous book "To Mock a Mockingbird" by Raymond Smullyan.

Typed routes
------------

{% highlight fsharp %}
let testapp : WebPart = 
    choose [
        urlscan "/add/%d/%d" (fun (a,b) -> OK((a + b).ToString()))
        notfound "Found no handlers" 
    ]
{% endhighlight %}

Multiple bindings and SSL support
---------------------------------

Suave supports binding the application to multiple TCP/IP addresses and ports combinations. It also supports HTTPS.

{% highlight fsharp %}
let sslCert = new X509Certificate("suave.pfx","easy");
choose [
    Console.OpenStandardOutput() |> log >>= never ; // log to standard output
    url "/hello" >>= OK "Hello World" ; 
    notfound "Found no handlers"     
    ] 
    |> web_server { 
        defaultConfig with bindings = [| HTTP, "127.0.0.1",80; HTTPS(sslCert), "192.168.13.138", 443 |];
        timeout = 3000
    }
{% endhighlight %}

Server configuration
--------------------

The first argument to `web_server` is a configuration record with the following signature.

{% highlight fsharp %}
type Protocols = | HTTP | HTTPS of X509Certificate
type HttpBinding = Protocols * string * int 
type Config = { 
    bindings : HttpBinding array; 
    error_handler : Exception -> String -> HttpRequest -> Async<unit>;
    timeout : int
    }
{% endhighlight %}

*bindings* array of bindings of the form _protocol, ip address, port_
*error_handler* a handler to deal with runtime errors
*timeout* maximun number of milliseconds before killing a request

Coding Guidelines
=================

Style Guide
-----------

Two space indentation.

{% highlight fsharp %}
match x with // '|' characters at base of 'match'
| A     -> ()
| Bcdef -> "aligned arrows" // space after '|' character
{% endhighlight %}

Method formatting with no spaces after/before normal parenthesis

{% highlight fsharp %}
let my_method_name first_arg (second : WithType) = async { // and monad builder
  return! f first_arg second
} // at base of 'let' kw
{% endhighlight %}

You need to document your methods with '///' to create XML-doc. A XML
documentation file is generated together with the compilation and is distributed
with the NuGet so that others can read your code's intentions easily.

Testing
-------

Tests really required.
