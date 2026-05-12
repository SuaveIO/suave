---
layout: default
---

Getting Hold of Suave's Logs
----------------------------

Current documentation for configuring Suave is either
[in this sample](https://github.com/fable-compiler/fable-suave-scaffold/blob/master/src/Server/WebServer.fs#L17-#L22)
or [in the Logary readme](https://github.com/logary/logary#using-logary-in-a-library).

Here is an example of using Logary via Facade adapter 4.0 + with Suave ver. 2.0+

{% highlight fsharp %}
open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Adapters.Facade

//init logger
let logary = 
        withLogaryManager "ExampleLogger" (
            withTargets [
                LiterateConsole.create LiterateConsole.empty "console"
            ]
            >> withRules [ Rule.createForTarget "console"]
            )
        |> Hopac.Hopac.run
        
//get logger for API
let logger = Logging.getLoggerByName("Suave.API")

let webConfig =
  { defaultConfig with
      logger   = LoggerAdapter.createGeneric logger)
  }
{% endhighlight %}

The below details how to configure Suave v1.x with Logary v3.x:

When you are using suave you will probably want to funnel all logs from the
output to your own log sink. We provide the interface `Logger` to do that; just
set the property `logger` in the configuration to an instance of your thread-safe
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
