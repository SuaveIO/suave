open Suave
open Suave.Http
open Suave.Successful
open System
open System.Net
open System.Threading
open System.Diagnostics

// NOTE: this test is only available on Linux

let app = OK "PONG"

let port = 3000us
let config =
  { defaultConfig with
     bindings = [ HttpBinding.mk HTTP IPAddress.Loopback port ]
     bufferSize = 8192
     maxOps = 10000
  }


let execute cmd args =

  use proc = new Process()

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
  let listening, server = startWebServerAsync config app
  Async.Start(server, cts.Token)

  // wait for the server to start listening
  listening |> Async.RunSynchronously |> printfn "start stats: %A"

  // launch httperf
  let output = execute "httperf" (sprintf "--hog --server=localhost --port=%d --uri=/ --rate=1000 --num-conns=1000 --num-calls=1000 --burst-length=20" port)

  Console.WriteLine output

  //kill the server
  cts.Cancel()
  0
