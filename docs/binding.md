---
layout: default
---

Multiple bindings and SSL support
---------------------------------

Suave supports binding the application to multiple TCP/IP addresses and ports
combinations. It also supports HTTPS via the interface `ITlsProvider`.

There is an OpenSSL implementation at [https://github.com/SuaveIO/suave/tree/master/suave.OpenSsl](https://github.com/SuaveIO/suave/tree/master/suave.OpenSsl)

{% highlight fsharp %}

open Suave.OpenSsl.Provider

let sslCert = new X509Certificate("suave.pfx","easy");
let cfg =
  { defaultConfig with
      bindings =
        [ HttpBinding.mk HTTP IPAddress.Loopback 80us
          HttpBinding.mk (HTTPS (openSsl sslCert)) (IPAddress.Parse "0.0.0.0") 443us
        ]
      timeout = TimeSpan.FromMilliseconds 3000. }
choose
  [ path "/hello" >>= OK "Hello World"
    NOT_FOUND "Found no handlers" ]
|> startWebServer cfg
{% endhighlight %}

The OpenSSL implementation comes with conditional bindings in
App.config for the three operating systems: linux, OS X and Windows.

**Note** -- currently the compiled versions are "gott och blandat" as we say in
Swedish. It means some are compiled for x86 (Windows) and some x64 (Linux, OS
X); check out [issue 42](https://github.com/SuaveIO/suave/issues/42) to see more
about this issue.