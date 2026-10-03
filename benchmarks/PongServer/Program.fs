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

/// Opt-in exact allocation probe for benchmarks/alloc-per-request.sh. It answers on a
/// side port (benchmark port + 100) so the measured request path is unchanged.
let startAllocationProbe (port : int) =
  let listener = new Sockets.TcpListener(IPAddress.Loopback, port)
  listener.Start()
  let thread =
    System.Threading.Thread((fun () ->
      while true do
        use client = listener.AcceptTcpClient()
        let body = string (System.GC.GetTotalAllocatedBytes true)
        let response = sprintf "HTTP/1.1 200 OK\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s" body.Length body
        let bytes = System.Text.Encoding.ASCII.GetBytes response
        client.GetStream().Write(bytes, 0, bytes.Length)), IsBackground = true)
  thread.Start()

let runServer argv =
  let comparison = argv |> Array.contains "--comparison"
  let port =
    match System.Environment.GetEnvironmentVariable "SUAVE_BENCHMARK_PORT" |> System.UInt16.TryParse with
    | true, value when comparison -> value
    | _ -> 3000us
  let acceptors =
    match argv with
    | [| n |] ->
        match System.Int32.TryParse n with
        | true, v when v >= 0 -> v
        | _ -> 1
    | _ -> 1
  let benchmarkApp =
    Suave.Router.router {
      get "/" (OK "")
      get "/user/:id" (fun ctx -> OK (Suave.Router.routeParam "id" ctx |> Option.defaultValue "") ctx)
      post "/user" (OK "")
    }
  if System.Environment.GetEnvironmentVariable "SUAVE_ALLOC_PROBE" = "1" then
    startAllocationProbe (int port + 100)
  let config =
    { defaultConfig with
        bindings  = [ HttpBinding.create HTTP IPAddress.Loopback port ]
        bufferSize = 8192
        maxOps     = if comparison then defaultConfig.maxOps else 10000
        hideHeader = comparison
        acceptorCount = acceptors }
  startWebServer config (if comparison then benchmarkApp else app)
  0

[<EntryPoint>]
let main argv =
  if argv |> Array.contains "--allocations" then RequestAllocations.run ()
  else runServer argv