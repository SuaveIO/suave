---
layout: default
---

Suave.DotLiquid
===============

Installing DotLiquid
--------------------

{% highlight text %}
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

Then, for your template:

{% highlight html %}
{% raw %}
<html>
  <head>
    <title>{{ model.title }}</title>
  </head>
  <body>
    <p>Hello from {{ model.title }}!</p>
  </body>
</html>
{% endraw %}
{% endhighlight %}

Naming conventions
--------------------

Suave.DotLiquid sets the DotLiquid naming convention to Ruby by default. This means that if, for example, your model is a record type with a member called 'Name', DotLiquid would expect the binding to be '{% raw %}{{model.name}}{% endraw %}'. You can change the naming convention to C#:

{% highlight fsharp %}
DotLiquid.setCSharpNamingConvention()
{% endhighlight %}

Working with Options
--------------------

DotLiquid can handle option types.

Example 1:

{% highlight fsharp %}
type UserModel = {
    UserName: string option
}

let model = { UserName = Some "Dave" }
// or
let model = { UserName = None }

let home = page "Index.html" model
{% endhighlight %}

{% highlight html %}
{% raw %}
<div>
    {% if model.UserName %}
        Hello {{model.UserName.Value}}
    {% else %}
        Dave is not here
    {% endif %}
</div>
{% endraw %}
{% endhighlight %}

Example 2:

{% highlight fsharp %}
let model = Some "Dave"
// or
let model : string option = None

let home = page "Index.html" model
{% endhighlight %}

{% highlight html %}
{% raw %}
<div>
    {% if model %}
        Hello {{model.Value}}
    {% else %}
        Dave is not here
    {% endif %}
</div>
{% endraw %}
{% endhighlight %}

References
----------

 - [DotLiquid for Designers](https://github.com/dotliquid/dotliquid/wiki/DotLiquid-for-Designers)
 - [DotLiquid for Developers](https://github.com/dotliquid/dotliquid/wiki/DotLiquid-for-Developers)
