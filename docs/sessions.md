---
layout: default
---

Using Sessions in Suave
=======================

While stateless web applications are all the rage, there still may be times where session storage is needed. In these cases, Suave has you covered!

Cookie State Session Storage
----------------------------

Suave provides storage interfaces for cookies in the `Suave.State.CookieStateStore` module.  The `statefulForSession` WebPart can be composed to make a path session-aware.  From there, the `HttpContext.state` function extracts the state information, and the `get` and `set` functions on the resulting `StateStore` object can be used to manipulate the contents of the state tracked by the session.

{% highlight fsharp %}
open Suave
open Suave.State.CookieStateStore

/// string -> 'T -> WebPart
let setSessionValue key value = context (fun ctx ->
  match ctx |> HttpContext.state with
  | Some state -> state.set key value
  | _ -> never // fail)

/// HttpContext -> string -> 'T option
let getSessionValue ctx key =
  match ctx |> HttpContext.state with
  | Some state -> state.get key
  | _ -> None

/// HttpContext -> string -> string
let getStringSessionValue ctx key = 
  match getSessionValue ctx key with
  | Some value -> string value
  | _ -> ""

/// WebPart
let cookieYes = warbler (fun ctx -> OK (getStringSessionValue ctx "test"))

/// WebPart
let cookieNo = warbler (fun ctx -> OK (getStringSessionValue ctx "nope"))

/// WebPart
let app =
  statefulForSession
  >=> setSessionValue "test" "123"
  >=> choose [
        path "/yes" >=> cookieYes
        path "/no" >=> cookieNo
        RequestErrors.NOT_FOUND
        ]
{% endhighlight %}

Server Keys
-----------

The contents of the cookie are encrypted before the cookie is sent. Suave's default configuration generates a new server key each time the server is restarted. While this is not _wrong_, users would likely get quite annoyed if they lost their state because the server was restarted. Additionally, specifying a server key lets load-balanced servers access the same information.

Continuing our example from above... _(This should not be used on production servers; a key should be generated and provided via a configuration file.)_

{% highlight fsharp %}
let suaveCfg =
  { defaultConfig with
      serverKey = System.Text.Encoding.UTF8.GetBytes("12345678901234567890123456789012")
    }

[<EntryPoint>]
let main argv = 
  startWebServer suaveCfg app
  0 
{% endhighlight %}

Cookie Serialization (of particular interest to .NET Core < netstandard2.0)
---------------------------------------------------------------------------

Suave uses the .NET Framework type `BinaryFormatter` to serialize the `Map<string, obj>` containing the session state; this is the default. However, the `BinaryFormatter` was removed in the .NET Core API, and the `DataContractJsonSerializer` does not recognize the `Map<string, obj>` type. One option is to utilize JSON.NET to serialize this object. To use that, ensure you've added the `Newtonsoft.Json` NuGet package to your project, then put the following code somewhere before the `suaveCfg` definition in the example above.

{% highlight fsharp %}
/// alias
let utf8 = System.Text.Encoding.UTF8

type JsonNetCookieSerialiser() =
  interface CookieSerialiser with
    member x.serialise m =
      utf8.GetBytes (JsonConvert.SerializeObject m)
    member x.deserialise m =
      JsonConvert.DeserializeObject<Map<string, obj>> (utf8.GetString m)
{% endhighlight %}

Then, modify the configuration to use that serializer.

{% highlight fsharp %}
/// Again, do not make this your server key in production...
let suaveCfg =
  { defaultConfig with
      serverKey = System.Text.Encoding.UTF8.GetBytes("12345678901234567890123456789012")
      cookieSerialiser = new JsonNetCookieSerialiser()
    }
{% endhighlight %}