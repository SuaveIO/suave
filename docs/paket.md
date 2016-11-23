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
    File.Move(tmp,Path.GetFileName url);;
 
// Step 1. Resolve and install the packages
 
#r "paket.exe"
 
Paket.Dependencies.Install """
frameworks: net46
source https://nuget.org/api/v2
nuget Suave
""";;
 
// Step 2. Use the packages
 
#r "packages/Suave/lib/net40/Suave.dll"
 
open Suave // always open suave
 
startWebServer defaultConfig (Successful.OK "Hello World!")
{% endhighlight %}
