open System
open System.Net

open Suave
open Suave.Web
open Suave.Http
open Suave.Types
open Suave.Http.Applicatives
open Suave.Http.Files
open Suave.Logging

let logger = Loggers.saneDefaultsFor LogLevel.Verbose

let config = 
  { defaultConfig with 
      bindings = [ HttpBinding.mk' HTTP "127.0.0.1" 8082 ]
      bufferSize = 2048
      maxOps = 10000
      logger = logger }

let listening, server = createWebServerAsync config (choose [ GET >>= browseHomeDirectory ])
Async.Start server

// wait for the server to start listening
listening |> Async.RunSynchronously |> ignore

async {
    while true do
      for _ in [1 .. 20] do
        use wc = new WebClient()
        wc.DownloadStringAsync(Uri("http://localhost:8082/test.html"))
      do! Async.Sleep 1000
} |> Async.Start

Console.Read() |> ignore