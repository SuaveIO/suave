---
layout: default
---

Suave + Paket = ♥
-----------------
Working fully self-contained getting-started example for Suave Web Server
scripting

Note you don't need to have _anything_ installed before starting with this
script. Nothing but F# Interactive and this script.

This script fetches the Paket.exe component which is referenced later in the
script. Initially the #r "paket.exe" reference is shown as unresolved. Once it
has been downloaded by the user (by executing the first part of the script) the
reference shows as resolved and can be used.

Paket is then used to fetch a set of F# packages, which are then used later in
the script.

{% highlight fsharp %}
// Step 0. Boilerplate to get the paket.exe tool
 
open System
open System.IO
 
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
 
if not (File.Exists "paket.exe") then
    let url = "https://github.com/fsprojects/Paket/releases/download/0.31.5/paket.exe"
    use wc = new Net.WebClient()
    let tmp = Path.GetTempFileName()
    wc.DownloadFile(url, tmp)
    File.Move(tmp,Path.GetFileName url)
 
// Step 1. Resolve and install the packages
 
#r "paket.exe"
 
Paket.Dependencies.Install """
source https://nuget.org/api/v2
nuget Suave
nuget FSharp.Data
nuget FSharp.Charting
""";;
 
// Step 2. Use the packages
 
#r "packages/Suave/lib/net40/Suave.dll"
#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "packages/FSharp.Charting/lib/net40/FSharp.Charting.dll"
 
let ctxt = FSharp.Data.WorldBankData.GetDataContext()
 
let data = ctxt.Countries.Algeria.Indicators.``GDP (current US$)``
 
open Suave // always open suave
open Suave.Http.Successful // for OK-result
open Suave.Web // for config
 
startWebServer defaultConfig (OK (sprintf "Hello World! In 2010 Algeria earned %f " data.[2010]))
{% endhighlight %}