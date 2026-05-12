---
layout: default
---

Typed routes
------------

You can create typed routes by using pathScan() with standard format specifiers. pathScan() uses PrintfFormat and so your path is statically typed; the compiler will complain if the number of parameters in the path doesn't match the handler. 

The matching path segments are passed to your handler in a tuple. Here's an example:

{% highlight fsharp %}
let testapp : WebPart =
  choose
    [ pathScan "/add/%d/%d" (fun (a, b) -> OK((a + b).ToString()))
      NOT_FOUND "Found no handlers" ]
{% endhighlight %}

Note that there is no ">=>" between the path string and the handler.

If you need to access the request, just construct the WebPart your handler returns with context() or request(). Modifying the previous example:

{% highlight fsharp %}
let handler (a, b) =
  context (fun ctx ->
    // get something from ctx.request...
    OK((a + b).ToString())

let testapp : WebPart =
  choose
    [ pathScan "/add/%d/%d" handler
      NOT_FOUND "Found no handlers" ]
{% endhighlight %}
