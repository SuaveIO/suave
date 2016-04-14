---
layout: default
---

Suave.DotLiquid
===============

How to use liquid from Suave.

{% highlight liquid %}
open Suave
open Suave.DotLiquid
open DotLiquid

let app =
  let o = { title = "Hello World" }
  page<> "my_page.liquid" o

{% endhighlight %}


References
----------

 - [DotLiquid for Designers](https://github.com/dotliquid/dotliquid/wiki/DotLiquid-for-Designers)
 - [DotLiquid for Developers](https://github.com/dotliquid/dotliquid/wiki/DotLiquid-for-Developers)