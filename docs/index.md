---
layout: default
---

Introduction
============

Suave is a lightweight, non-blocking web server. The non-blocking I/O model is efficient and suitable for building fast, scalable network applications. In fact, Suave is written in a **completely non-blocking** fashion throughout. Suave **runs on Linux**, OS X, and Windows flawlessly.

Suave was inspired by the simplicity of [Happstack](https://www.happstack.com/) and born out of the necessity of embedding web server capabilities in my own applications. Suave supports HTTPS, multiple TCP/IP bindings, Basic Access Authentication, Keep-Alive, and HTTP compression.

Getting started
---------------

See [Suave's NuGet page](https://www.nuget.org/packages/Suave) for installation / usage instructions.

The simplest possible application: Hello World!
-----------------------------------------------

The simplest Suave application is an HTTP server that greets all visitors with the string `"Hello World!"`.

{% highlight fsharp %}
open Suave

//                 record               WebPart
startWebServer defaultConfig (Successful.OK "Hello World!")
{% endhighlight %}

The above statement will start a web server on default port 8080 over HTTP.
`startWebServer` takes a configuration record (in this case,[ `Web.defaultConfig`](https://github.com/SuaveIO/suave/blob/d9deb5f4f973fd21d15bdd7e85ac9c0bee05baab/src/Suave/Web.fs#L78-L93)) and a `WebPart` (in this case, [`Successful.OK`](https://github.com/SuaveIO/suave/blob/d9deb5f4f973fd21d15bdd7e85ac9c0bee05baab/src/Suave/Combinators.fs#L129-L150)` "Hello World"`).

Ta-daa!

> NOTE  
> The snippet above will block on the function call! That is, if you run it in an IDE, you'll probably have to restart it to stop Suave.
To avoid this, you have to cancel the [`Async.DefaultCancellationToken`](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-control-fsharpasync.html#DefaultCancellationToken), and if you want to handle disposal of the async yourself, have a look at [`startWebServerAsync`](https://github.com/SuaveIO/suave/blob/d9deb5f4f973fd21d15bdd7e85ac9c0bee05baab/src/Suave/Web.fs#L26-L67). 
>
> See next section for a cancellable example.

Cancellable "Hello World!" on the terminal
-------------------------------------------

Paste the code below into an [`.fsx`](https://learn.microsoft.com/en-us/dotnet/fsharp/tools/fsharp-interactive/#scripting-with-f) file (e.g., `Hello.fsx`):

{% highlight fsharp %}
#r "nuget: Suave, 2.6.2"

open System
open System.Threading
open Suave

let cts = new CancellationTokenSource()
let conf = { defaultConfig with cancellationToken = cts.Token }
let listening, server = startWebServerAsync conf (Successful.OK "Hello World")

Async.Start(server, cts.Token)

Console.ReadKey true |> ignore
cts.Cancel()
{% endhighlight %}

Now run it with `dotnet fsi Hello.fsx`. The code above will run the server until a key is pressed in the console window.

> INFO  
> Completely new? Here are installation instructions on [OS X/macOS](http://fsharp.org/use/mac/), [Windows](http://fsharp.org/use/windows/), and [Linux](http://fsharp.org/use/linux/). If you have Visual Studio installed you
should be able to find the "F# Interactive" in your menus.

Documentation
-------------

In Suave, we have opted to write a lot of documentation inside the code; so just
hover the function in your IDE or use an assembly browser to bring out the XML
docs. You can also browse [our API reference](/Suave.html).
