---
layout: default
---

Getting Hold of Suave's Logs
----------------------------

When you are using suave you will probably want to funnel all logs from the
output to your own log sink. We provide the interface `Logger` to do that; just
set the propery `logger` in the configuration to an instance of your thread-safe
logger. An example:

{% highlight fsharp %}
type MyHackLogger(minLevel) =
  interface Logger with
    member x.Log level fLine =
      if level >= minLevel then
        // don't do this for real ;)
        System.Windows.Forms.MessageBox.Show((fLine ()).message)
{% endhighlight %}

You can use Logary for integrated logging:

{% highlight dosbatch %}
Install-Package Logary.Adapters.Suave
{% endhighlight %}

Use the `SuaveAdapter` type to set the Logger in Suave's configuration:

{% highlight fsharp %}
open Suave.Logging

use logary =
  withLogary' "logibit.web" (
    withTargets [
      Console.create Console.empty "console"
      Debugger.create Debugger.empty "debugger"
    ] >>
    withMetrics (Duration.FromMilliseconds 5000L) [
      WinPerfCounters.create (WinPerfCounters.Common.cpuTimeConf) "wperf"
(Duration.FromMilliseconds 300L)
      ] >>
      withRules [
        Rule.createForTarget "console"
        Rule.createForTarget "debugger"
      ]
    )

let webConfig =
  { defaultConfig with
      logger   = SuaveAdapter(logary.GetLogger "suave")
  }
{% endhighlight %}