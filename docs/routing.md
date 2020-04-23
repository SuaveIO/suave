---
layout: default
---

A slightly more complex example: routing HTTP requests
----------------------------------------------------------

Most .NET web frameworks are based on the object-oriented paradigm. This means that you have a class called something like CheesesController or CheesesModule. The methods in that class are then associated with url routes, with the most common routing methods being convention based, attribute-based (e.g. adding attributes like `[Route("cheeses/{cheeseId}/taste")]` and `[HttpGet]`) or some sort of global routing table.

Suave is designed to fit in with F#'s functional programming paradigm, so routing does not use any of these routing methods. Instead routing takes place using a single function, itself composed from many smaller functions. This function has the signature `WebPart`:

{% highlight fsharp %}

type WebPart = HttpContext -> Async<HttpContext option>

{% endhighlight %}

This function has a single parameter of type `HttpContext`. This is an F# record type that includes the HTTP request, the HTTP response, and a few other things. This should be pretty familiar to anyone who has done any web programming before.

A WebPart function returns an asynchronous workflow which itself ultimately returns an `HttpContext option`. Asynchronous workflows are hopefully already somewhat familiar. The `HttpContext option` returned by the asynchronous workflow is either `Some HttpContext` record or `None`, and it is the option that is used to determine routing. Here is a web server with some simple routing logic. You can ignore the `>=>` operator and the exact workings of `choose` for now (both are explained in more detail later), just focus on the tree-like structure of the code:

{% highlight fsharp %}        
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

let app =
  choose
    [ GET >=> choose
        [ path "/hello" >=> OK "Hello GET"
          path "/goodbye" >=> OK "Good bye GET" ]
      POST >=> choose
        [ path "/hello" >=> OK "Hello POST"
          path "/goodbye" >=> OK "Good bye POST" ] ]

startWebServer defaultConfig app
{% endhighlight %}

By using `choose` we execute different logic depending on whether the request was a GET or a POST, and depending on whether the url was /hello or /goodbye. If a request matches a given path in the decision tree, `choose` will return `Some HttpContext`, if it doesn't, `choose` will return `None`. The end result is that when someone makes a request the server will walk down this tree looking for the first part that returns `Some HttpContext`, and then return it to the client. If no part of the tree returns `Some HttpContext` then the result is an exception. You can also add a default route which returns a 404 page.

The server won't evaluate the entire data structure for every request, only the actual decisions, so there is no need to be concerned about performance.

## Handling Errors

How does the `userState` work when there's an error?

Even though the web part that created the state succeed earlier in the pipeline, there's no way to get at it later. Whenever there's an exception, Suave will terminate the currently running web part and jump into the exception handler. No, your best bet is to avoid throwing exceptions and working with types and values for control flow instead. BestPractice™
