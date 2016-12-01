---
layout: default
---

Multiple bindings and SSL support
---------------------------------

Suave supports binding the application to multiple TCP/IP addresses and ports
combinations. It also supports HTTPS via the interface `ITlsProvider`.

There is an OpenSSL implementation at [https://github.com/SuaveIO/suave/tree/master/src/Suave.OpenSSL](https://github.com/SuaveIO/suave/tree/master/src/Suave.OpenSSL)

{% highlight fsharp %}
let cfg =
  { defaultConfig with
      bindings =
        [ HttpBinding.create HTTP IPAddress.Loopback 80us
          HttpBinding.createSimple HTTP "10.0.1.34" 9000 ]
      listenTimeout = TimeSpan.FromMilliseconds 3000. }
choose [
  path "/hello" >=> OK "Hello World"
  NOT_FOUND "Found no handlers"
]
|> startWebServer cfg
{% endhighlight %}
