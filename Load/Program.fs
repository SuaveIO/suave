open System
open System.Net

open Suave.Web
open Suave.Http
open Suave.Types
open Suave.Session

async {
          (Console.OpenStandardOutput() |> log) >>= GET >>= browse
          |> web_server
              { bindings =
                [ HttpBinding.Create(HTTP, "127.0.0.1", 8082)]
              ; error_handler    = default_error_handler
              ; web_part_timeout = TimeSpan.FromMilliseconds 1000.
              ; listen_timeout   = TimeSpan.FromMilliseconds 2000.
              ; ct               = Async.DefaultCancellationToken
              ; buffer_size = 2048
              ; max_ops = 10000 }
} |> Async.Start
async {
    while true do
      for _ in [1 .. 20] do
        use wc = new WebClient()
        wc.DownloadStringAsync(Uri("http://localhost:8082/test.html"))
      do! Async.Sleep 1000
} |> Async.Start

Console.Read() |> ignore