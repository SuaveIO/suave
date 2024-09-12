module Pong.Program

open Suave
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
     bindings = [ HttpBinding.create HTTP IPAddress.Loopback port ]
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

  let _ = proc.Start()
  proc.WaitForExit()
  proc.StandardOutput.ReadToEnd()

[<EntryPoint>]
let main argv =
  let cts = new CancellationTokenSource()
  let listening, server = startWebServerAsync config app

  // wait for the server to start listening
  listening |> Async.RunSynchronously |> printfn "start stats: %A"

  // launch httperf
  let output = execute "httperf" (sprintf "--hog --server=localhost --port=%d --uri=/ --rate=20 --num-conns=100 --num-calls=1000 --burst-length=10" port)

  Console.WriteLine output

  //kill the server
  cts.Cancel()
  0