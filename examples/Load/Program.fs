open System
open System.Net

open Suave
open Suave.Operators
open Suave.Http
open Suave.Filters
open Suave.Files
open Suave.Logging

let logger = Targets.create Verbose [||]

let config =
  { defaultConfig with
      bindings   = [ HttpBinding.createSimple HTTP "127.0.0.1" 8082 ]
      bufferSize = 2048
      maxOps     = 10000
      logger     = logger }

let listening, server = startWebServerAsync config (choose [ GET >=> browseHome ])
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
