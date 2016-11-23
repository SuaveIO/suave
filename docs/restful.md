---
layout: default
---

Building RESTful Web Services
-----------------------------

We can expose simple REST web services with the help of the combinator `mapJson`. The `mapJson` uses the default BCL JSON serializer `DataContractJsonSerializer`.

{% highlight fsharp %}
open Suave
open Suave.Json
open System.Runtime.Serialization

[<DataContract>]
type Foo =
  { [<field: DataMember(Name = "foo")>]
    foo : string }

[<DataContract>]
type Bar =
  { [<field: DataMember(Name = "bar")>]
    bar : string }

startWebServer defaultConfig (mapJson (fun (a:Foo) -> { bar = a.foo }))
{% endhighlight %}


{% highlight bash %}
ademar@nascio:~$ curl -X POST -d '{"foo":"xyz"}' http://localhost:8083/ -w "\n"
{"bar":"xyz"}
{% endhighlight %}

Or you can bring your own JSON serializer like Chiron:https://github.com/xyncro/chiron

{% highlight fsharp %}
type A = 
  { a : int }
  static member ToJson (x : A) =
    Json.writer "a" x.a
{% endhighlight %}

{% highlight bash %}
Json.format (Json.serialize { a = 42 })

val it = """{"a":42}"""
{% endhighlight %}

{% highlight fsharp %}
let app =
  GET >=> path "/test"
  >=> OK (UTF8.bytes it)
  >=> setMimeType "application/json; charset=utf-8"
{% endhighlight %}
