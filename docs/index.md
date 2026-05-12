---
layout: default
---

Introduction
============

Suave is a lightweight, non-blocking web server. The non-blocking I/O model is efficient and suitable for building fast, scalable network applications. In fact, Suave is written in a **completely non-blocking** fashion throughout. Suave **runs on Linux**, OS X and Windows flawlessly.

Suave is inspired in the simplicity of Happstack and born out of the necessity
of embedding web server capabilities in my own applications. Still in its early
stages Suave supports HTTPS, multiple TCP/IP bindings, Basic Access
Authentication, Keep-Alive and HTTP compression.

NuGet
-----

To install Suave, add the following to your
[paket](https://github.com/fsprojects/Paket).dependencies:

{% highlight dosbatch %}
source https://nuget.org/api/v2
nuget Suave
{% endhighlight %}

Or you can use the legacy NuGet command line [Package Manager
Console](http://docs.nuget.org/docs/start-here/using-the-package-manager-console):

{% highlight dosbatch %}
PM> Install-Package Suave
{% endhighlight %}

The simplest possible application: Hello World!
-----------------------------------------------

The simplest Suave application is a simple HTTP server that greets all visitors
with the string `"Hello World!"`

{% highlight fsharp %}
open Suave

startWebServer defaultConfig (Successful.OK "Hello World!")
{% endhighlight %}

The above statement will start a web server on default port 8080 over HTTP.
`startWebServer` takes a configuration record and the WebPart `(OK "Hello
World")` It's worth noting that with the above, your application will block on
the function call, until you cancel the `Async.DefaultCancellationToken`. If you
want to handle disposal of the async yourself, have a look at
`startWebServerAsync`.

Ta-daa!

To test the above yourself, paste that code in `Hello.fsx` and then invoke it
with `fsharpi Hello.fsx` (or `fsi Hello.fsx` on Windows). Completely new? Here
are installation instructions on [OS X/macOS](http://fsharp.org/use/mac/),
[Windows](http://fsharp.org/use/windows/) and
[Linux](http://fsharp.org/use/linux/). If you have Visual Studio installed you
should be able to find the "F# Interactive" in your menus.

If you are running in an IDE, and not starting Suave via `Hello.fsx`, you'll
want to provide a way of stopping the server without having to restart the IDE;
the code below will run the server until a key is pressed in the console window,
then shuts down the server.

{% highlight fsharp %}
open System
open System.Threading
open Suave

[<EntryPoint>]
let main argv = 
  let cts = new CancellationTokenSource()
  let conf = { defaultConfig with cancellationToken = cts.Token }
  let listening, server = startWebServerAsync conf (Successful.OK "Hello World")
    
  Async.Start(server, cts.Token)
  printfn "Make requests now"
  Console.ReadKey true |> ignore
    
  cts.Cancel()

  0 // return an integer exit code
{% endhighlight %}

In suave, we have opted to write a lot of documentation inside the code; so just
hover the function in your IDE or use an assembly browser to bring out the XML
docs. You can also browse [our API reference](/Suave.html).
