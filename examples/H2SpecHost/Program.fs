/// Minimal Suave fixture used to drive the h2spec conformance suite against
/// the in-tree HTTP/2 implementation. Builds on the h2c upgrade machinery
/// landed in earlier steps; h2spec connects over HTTP/1.1 cleartext, requests
/// `Upgrade: h2c`, and then drives the full HTTP/2 protocol.
///
/// Routes (matching the conventional h2spec fixture surface):
///   GET /        -> "200 OK" with a tiny text/plain body
///   GET /echo    -> the request body (or empty string), so DATA round-trip
///                   tests have something to inspect.
///
/// CLI:
///   H2SpecHost [--port N]
/// Prints a single line "listening on <port>" to stdout once ready, suitable
/// for the CI workflow to grep before invoking h2spec.
module H2SpecHost.Program

open System
open System.Net
open System.Threading
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

let private parsePort (argv: string[]) =
  let rec loop i =
    if i >= argv.Length then 0us
    elif argv.[i] = "--port" && i + 1 < argv.Length then
      match UInt16.TryParse argv.[i + 1] with
      | true, p -> p
      | false, _ -> 0us
    else loop (i + 1)
  loop 0

/// WebPart: echo back the raw request body so h2spec body round-trip checks
/// have content to compare. text/plain to avoid any content-type sniffing.
let private echo : WebPart =
  fun ctx ->
    let body = ctx.request.rawForm
    (Writers.setMimeType "text/plain; charset=utf-8"
      >=> Successful.ok body) ctx

let app : WebPart =
  choose [
    path "/" >=> OK "h2spec host"
    path "/echo" >=> echo
    // h2spec sometimes probes arbitrary paths during generic tests.
    OK "h2spec host"
  ]

[<EntryPoint>]
let main argv =
  // Allow either an explicit --port or a default ephemeral binding. Ephemeral
  // binding requires the OS to assign a port, but Suave's configuration takes
  // an explicit port. We therefore acquire a free port up-front when no
  // --port is supplied.
  let port =
    match parsePort argv with
    | 0us ->
      let l = new System.Net.Sockets.TcpListener(IPAddress.Loopback, 0)
      l.Start()
      let p = uint16 ((l.LocalEndpoint :?> IPEndPoint).Port)
      l.Stop()
      p
    | p -> p

  let cts = new CancellationTokenSource()
  let cfg =
    { defaultConfig with
        bindings = [ HttpBinding.create HTTP IPAddress.Loopback port ]
        cancellationToken = cts.Token }

  let ready, server = Web.startWebServerAsync cfg app
  ready |> Async.RunSynchronously |> ignore
  // Single-line readiness marker that the CI workflow parses.
  Console.WriteLine(sprintf "listening on %d" (int port))
  Console.Out.Flush()

  // Graceful shutdown on SIGTERM / SIGINT (Ctrl+C). h2spec finishes quickly
  // but CI may still want to terminate us deterministically.
  Console.CancelKeyPress.Add(fun e ->
    e.Cancel <- true
    cts.Cancel())

  server.Wait()
  0
