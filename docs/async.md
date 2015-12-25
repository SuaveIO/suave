---
layout: default
---

Async computation expressions and >=>
-------------------------------------

Suave WebParts use F# async computation expressions. For example you could write a time wasting WebPart like this:

{% highlight fsharp %}
let sleep milliseconds message: WebPart =
  fun (x : HttpContext) ->
    async {
      do! Async.Sleep milliseconds
      return! OK message x
    }
{% endhighlight %}

Async computation expressions are built into the F# language as a way of chaining asynchronous functions together. As well as `do!` and `return!` you can use `let!` to wait for another async function to return and then assign the result to a variable. For instance, `let! result = anotherAsyncWorkflow`. Behind the scenes "let!" is just syntactic sugar for calling the method `builder.Bind` on a computation builder.

In Suave we program by chaining functions of type WebPart.

{% highlight fsharp %}
type WebPart = HttpContext -> Async<HttpContext option>
{% endhighlight %}

Suave uses the `>=>` operator to chain WebParts together. In category theory this operation is known as a Kleisli composition. That is to say, `>=>`
chains together Async options rather than just vanilla Async computations. If the result of the first of the two chained workflows is `None`, then the computation is short-circuited and the second computation is never run. If the first computation returns `Some x`, then `>=>` behaves in much the same way as `let!`: the result of running the expression on the left is passed into the expression on the right.

There is a good tutorial on using the `>=>` operator to short circuit a series of operations here: [Railway oriented programming](http://fsharpforfunandprofit.com/posts/recipe-part2/)

There is also a good tutorial on computation expressions by the same author: [Computation expressions: Introduction](http://fsharpforfunandprofit.com/posts/computation-expressions-intro/)