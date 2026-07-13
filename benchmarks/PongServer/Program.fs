module PongServer.Program

open Suave
open Suave.Successful
open Suave.Filters
open Suave.Operators
open System.Net

// Minimal Suave server for profiling: a single static-bytes route.
// No `choose`, no routing tree — the simplest possible Suave webPart, so the
// profile reflects the request lifecycle (read, parse, write) and not user code.
let app : WebPart = OK "PONG"

[<EntryPoint>]
let main argv =
  let acceptors =
    match argv with
    | [| n |] ->
        match System.Int32.TryParse n with
        | true, v when v >= 0 -> v
        | _ -> 1
    | _ -> 1
  let config =
    { defaultConfig with
        bindings  = [ HttpBinding.create HTTP IPAddress.Loopback 3000us ]
        bufferSize = 8192
        maxOps     = 10000
        acceptorCount = acceptors }
  startWebServer config app
  0
