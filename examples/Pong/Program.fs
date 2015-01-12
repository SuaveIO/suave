open Suave.Types
open Suave.Web
open Suave.Http
open Suave.Http.Successful
open System
open System.Net
open System.Threading

// NOTE: this test is only available on Linux

let app : WebPart = OK "PONG"

let config =
  let customProperties =
    { default_config.props with
        bindings = [ HttpBinding.mk HTTP IPAddress.Loopback 3000us ]
        buffer_size = 8192
        max_ops = 10000
    }
  { default_config with
      props = customProperties
  }

open System.Diagnostics

let execute cmd args =

  use proc = new Process();

  proc.StartInfo.FileName         <- cmd
  proc.StartInfo.CreateNoWindow   <- true
  proc.StartInfo.RedirectStandardOutput <- true
  proc.StartInfo.UseShellExecute  <- false
  proc.StartInfo.Arguments        <- args
  proc.StartInfo.CreateNoWindow   <- true

  let r = proc.Start()
  proc.WaitForExit()
  proc.StandardOutput.ReadToEnd()

open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
  let cts = new CancellationTokenSource()
  let listening, server = web_server_async config app
  Async.Start(server, cts.Token)

  // wait for the server to start listening
  listening |> Async.RunSynchronously |> printfn "start stats: %A"

  // launch httpref
  let output = execute "/usr/bin/httperf" "--hog --server=localhost --port=3000 --uri=/ --rate=1000 --num-conns=1000 --num-calls=1000 --burst-length=20"

  Console.WriteLine output

  //kill the server
  cts.Cancel()
  0
