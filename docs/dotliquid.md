---
layout: default
---

Suave.DotLiquid
===============

Installing DotLiquid
--------------------

{% highlight %}
paket add nuget DotLiquid
paket add nuget Suave.DotLiquid
{% endhighlight %}

How to use liquid from Suave.
-----------------------------

{% highlight fsharp %}
open Suave
open Suave.DotLiquid
open DotLiquid

type Model =
  { title : string }

setTemplatesDirectory "./templates"

let o = { title = "Hello World" }

let app =
  choose
    [ GET >=> choose
        [ path "/" >=> page "my_page.liquid" o ]]

{% endhighlight %}


References
----------

 - [DotLiquid for Designers](https://github.com/dotliquid/dotliquid/wiki/DotLiquid-for-Designers)
 - [DotLiquid for Developers](https://github.com/dotliquid/dotliquid/wiki/DotLiquid-for-Developers)
