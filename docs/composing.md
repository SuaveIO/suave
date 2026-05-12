---
layout: default
---

Composing bigger programs: combinators
--------------------------------------------

Defining the entire logic of your program in a single giant function called app would clearly be impossible. Functional programming is all about composing functions from several smaller functions, and both F# and Suave offer various tools to make this easy.

In functional programming parlance, a "combinator" either combines several things of the same type into another thing of the same type, or otherwise takes a value and returns a new, modified version of that value. In mathematics it has a slightly different meaning, but we need not worry about this. In the case of Suave, there are two types of combinator:

- Combinators which combine multiple `WebPart` into a single `WebPart`.
- Combinators that produce `WebPart` from more primitive values. Recall that `WebPart` has the type `HttpContext -> Async<HttpContext option>`. These combinators therefore always take a single HttpContext and produce a new HttpContext, wrapped inside an async option workflow.

Together these are used to create web parts, combine them to produce new webparts, and ultimately combine them all into a single webpart passed as an argument used to initialise the web server.

We have already seen several examples of combinators. The `choose` function seen below takes a list of `WebPart`, and combines them all into a single new `WebPart`:

{% highlight fsharp %}
val choose : (options : WebPart list) -> WebPart
{% endhighlight %}

The `choose` combinator is implemented such that it will execute each webpart in the list until one returns success.

`>=>` is also a combinator, one that combines exactly two web parts. It runs the first, waits for it to finish, and then either passes the result into the second part, or short circuits if the first part returns `None`.

`OK` is a combinator of the second type. It always succeeds and writes its argument to the underlying response stream. It has type `string -> WebPart`.

To gain access to the underlying `HttpRequest` and read query and http form data we can use the `request` combinator (the `^^` custom operator, you have to add `open Suave.Utils.Collections`, is shorthand for searching a list of key-value option pairs and returning the value (or None if not found)):

{% highlight fsharp %}
let greetings q =
  defaultArg (Option.ofChoice (q ^^ "name")) "World" |> sprintf "Hello %s"

let sample : WebPart = 
    path "/hello" >=> choose [
      GET  >=> request (fun r -> OK (greetings r.query))
      POST >=> request (fun r -> OK (greetings r.form))
      RequestErrors.NOT_FOUND "Found no handlers" ]
{% endhighlight %}

You can similarly use `context` to gain access to the full `HttpContext` and connection.

To protect a route with HTTP Basic Authentication the combinator `authenticateBasic` is used like in the following example.

{% highlight fsharp %}
let requiresAuthentication _ =
    choose
        [ GET >=> path "/public" >=> OK "Default GET"
          // Access to handlers after this one will require authentication
          Authentication.authenticateBasic 
            (fun (user,pwd) -> user = "foo" && pwd = "bar") 
            (choose [
                GET >=> path "/whereami" >=> OK (sprintf "Hello authenticated person ")
                GET >=> path "/" >=> dirHome
                GET >=> browseHome // Serves file if exists 
             ])]
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
