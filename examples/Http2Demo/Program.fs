/// Non-trivial HTTP/2 demo for Suave.
///
/// This example exercises every major HTTP/2 feature implemented in
/// `Suave.Http2` so that the protocol can be observed end-to-end with any
/// HTTP/2 client (curl, nghttp, h2load, a browser via a TLS proxy):
///
///   * h2c upgrade   — clients that send `Upgrade: h2c` over HTTP/1.1 are
///                     switched to HTTP/2 by `Suave.Http2.H2cUpgrade`.
///   * h2c prior     — clients that begin with the HTTP/2 connection preface
///     knowledge       are picked up by `Suave.Http2.H2cPriorKnowledge`.
///   * Multiplexing  — `/` serves an HTML page that references several
///                     sub-resources; the client retrieves them concurrently
///                     on the same TCP connection, each on its own stream.
///   * Server Push   — `/` records PUSH_PROMISE intents for `/style.css` and
///                     `/app.js` via `Suave.Http2.Push.push`. The HTTP/2
///                     writer emits the corresponding PUSH_PROMISE frames
///                     and synthesised responses before the parent stream is
///                     fully delivered.
///   * HPACK         — every response goes through the in-tree HPACK
///                     encoder; repeated headers (`Content-Type`, `Server`,
///                     `Date`, …) compress to one or two bytes after warm-up.
///   * Prioritization— `/slow` artificially delays its body so that
///                     concurrent requests to `/fast` overtake it,
///                     demonstrating that frame ordering on the wire is
///                     driven by stream readiness, not request arrival.
///   * Flow control  — `/large` emits a body larger than the default 64 KiB
///                     initial window. The client must send WINDOW_UPDATE
///                     frames before the rest of the body can be delivered.
///   * Interleaving  — fetching `/large` together with `/fast` and `/tile/N`
///     frames          shows DATA frames from different streams interleaved
///                     on the connection. The connection trace (visible via
///                     `nghttp -v` or `h2load`) makes this explicit.
///
/// Run:
///     dotnet run --project examples/Http2Demo/Http2Demo.fsproj -- --port 8443
///
/// Then try:
///     curl --http2-prior-knowledge -v http://localhost:8443/
///     nghttp -nv http://localhost:8443/
///     h2load -n 100 -c 1 -m 10 http://localhost:8443/tile/1
///     curl --http2-prior-knowledge -o /dev/null http://localhost:8443/large
module Http2Demo.Program

open System
open System.Net
open System.Text
open System.Threading
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.Writers

// ---------------------------------------------------------------------------
// Static-ish assets. Kept as in-process byte arrays so the example has zero
// filesystem dependencies and can be run from any working directory.
// ---------------------------------------------------------------------------

let private indexHtml = """<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Suave HTTP/2 demo</title>
  <link rel="stylesheet" href="/style.css">
  <script src="/app.js" defer></script>
</head>
<body>
  <h1>Suave HTTP/2 demo</h1>
  <p>
    This page references a stylesheet and a script that the server has
    already <em>pushed</em> on its own initiative (see PUSH_PROMISE frames
    on the wire). The grid below loads 16 tiny SVG tiles concurrently on
    the same TCP connection — each on its own HTTP/2 stream.
  </p>
  <div id="tiles">
    <img src="/tile/1"  alt="tile 1"  width="48" height="48">
    <img src="/tile/2"  alt="tile 2"  width="48" height="48">
    <img src="/tile/3"  alt="tile 3"  width="48" height="48">
    <img src="/tile/4"  alt="tile 4"  width="48" height="48">
    <img src="/tile/5"  alt="tile 5"  width="48" height="48">
    <img src="/tile/6"  alt="tile 6"  width="48" height="48">
    <img src="/tile/7"  alt="tile 7"  width="48" height="48">
    <img src="/tile/8"  alt="tile 8"  width="48" height="48">
    <img src="/tile/9"  alt="tile 9"  width="48" height="48">
    <img src="/tile/10" alt="tile 10" width="48" height="48">
    <img src="/tile/11" alt="tile 11" width="48" height="48">
    <img src="/tile/12" alt="tile 12" width="48" height="48">
    <img src="/tile/13" alt="tile 13" width="48" height="48">
    <img src="/tile/14" alt="tile 14" width="48" height="48">
    <img src="/tile/15" alt="tile 15" width="48" height="48">
    <img src="/tile/16" alt="tile 16" width="48" height="48">
  </div>
  <ul>
    <li><a href="/fast">/fast</a> — instant response</li>
    <li><a href="/slow">/slow</a> — delayed, demonstrates prioritization</li>
    <li><a href="/large">/large</a> — 256 KiB body, demonstrates flow control</li>
    <li><a href="/info">/info</a> — per-feature explanation</li>
  </ul>
</body>
</html>
"""

let private styleCss = """body { font-family: system-ui, sans-serif; margin: 2rem; max-width: 48rem; }
h1   { color: #2b6cb0; }
#tiles { display: grid; grid-template-columns: repeat(8, 48px); gap: 4px; margin: 1rem 0; }
img  { border-radius: 4px; }
ul   { line-height: 1.6; }
code { background: #f1f5f9; padding: 0 0.25rem; border-radius: 3px; }
"""

let private appJs = """// Trivial client-side hook so the script body is non-empty and HPACK has
// something to compress on subsequent loads.
(function () {
  document.addEventListener('DOMContentLoaded', function () {
    var p = document.createElement('p');
    p.textContent = 'Page hydrated by /app.js on stream ' +
      (performance && performance.now ? performance.now().toFixed(1) : '?');
    document.body.appendChild(p);
  });
})();
"""

/// Build a 48x48 SVG tile whose colour is derived from `n` so each tile is
/// visually distinct and the response bodies differ — preventing any cache /
/// proxy collapsing.
let private tileSvg (n: int) : byte[] =
  let hue = (n * 23) % 360
  let svg =
    sprintf
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n\
       <svg xmlns=\"http://www.w3.org/2000/svg\" width=\"48\" height=\"48\" viewBox=\"0 0 48 48\">\
       <rect width=\"48\" height=\"48\" fill=\"hsl(%d,70%%,55%%)\"/>\
       <text x=\"24\" y=\"30\" font-family=\"sans-serif\" font-size=\"18\" \
       text-anchor=\"middle\" fill=\"white\">%d</text></svg>"
      hue n
  Encoding.UTF8.GetBytes svg

/// 256 KiB of deterministic bytes — comfortably larger than the default
/// HTTP/2 initial window (65535) so the client has to issue WINDOW_UPDATE
/// frames before the writer can finish sending the body.
let private largeBody : byte[] =
  let size = 256 * 1024
  let buf = Array.zeroCreate<byte> size
  for i in 0 .. size - 1 do
    // printable ASCII so the body inspects nicely in curl / nghttp output
    buf.[i] <- byte (33 + (i % 94))
  buf

let private infoText = """Suave HTTP/2 demo — feature map

  /              HTML page; the handler records PUSH_PROMISE intents for
                 /style.css and /app.js, then returns the page. The HTTP/2
                 writer emits those PUSH_PROMISEs on the parent stream and
                 delivers the synthesised responses before the client has
                 a chance to request them.

  /style.css     small CSS resource (also served via push).
  /app.js        small JS resource  (also served via push).

  /tile/N        48x48 SVG tile. The index page references 16 of these so a
                 browser/h2load fires 16 concurrent GETs on the same TCP
                 connection — each on its own stream id. HPACK shrinks the
                 mostly-identical request and response headers down to a
                 handful of bytes per stream after the first one.

  /fast          immediate 200 OK; useful as a control for prioritization.
  /slow          200 OK after ~500 ms; concurrent /fast requests overtake
                 it, demonstrating that frame ordering is driven by stream
                 readiness rather than request arrival.

  /large         262 144-byte body. Exceeds the default HTTP/2 initial
                 window (65535) so the connection trace shows WINDOW_UPDATE
                 frames flowing from the client and DATA frames being
                 interleaved with other streams.

  /info          this page.
"""

// ---------------------------------------------------------------------------
// WebParts.
// ---------------------------------------------------------------------------

/// Index handler: record two PUSH_PROMISE intents, then serve the HTML.
/// Suave's HTTP/2 writer (when this connection is in HTTP/2 mode) consumes
/// the list set on userState and emits PUSH_PROMISE + synthesised responses
/// before the parent stream is closed. Over HTTP/1.1 the recorded intents
/// are simply ignored — so the same WebPart serves both protocols cleanly.
let private index : WebPart =
  Http2.Push.push "/style.css" [ "accept", "text/css" ]
  >=> Http2.Push.push "/app.js"   [ "accept", "application/javascript" ]
  >=> setMimeType "text/html; charset=utf-8"
  >=> OK indexHtml

/// Tile handler — pulls the integer parameter from `pathScan` and renders an
/// SVG. Restricted to a sensible range so a malicious client cannot ask for
/// arbitrarily large allocations.
let private tile (n: int) : WebPart =
  if n < 1 || n > 1024 then
    RequestErrors.BAD_REQUEST "tile index out of range (1..1024)"
  else
    setMimeType "image/svg+xml"
    >=> ok (tileSvg n)

/// /slow — delay before responding. The delay happens inside the WebPart so
/// Suave's HTTP/2 dispatch is free to interleave other streams' frames on
/// the wire while this stream sits idle.
let private slow : WebPart =
  fun ctx ->
    async {
      do! Async.Sleep 500
      return!
        (setMimeType "text/plain; charset=utf-8"
          >=> OK "slow: 500 ms elapsed before the body was queued") ctx
    }

/// /large — body larger than the default flow-control window.
let private large : WebPart =
  setMimeType "application/octet-stream"
  >=> ok largeBody

let private app : WebPart =
  choose [
    GET >=> choose [
      path "/"          >=> index
      path "/style.css" >=> setMimeType "text/css"
                         >=> OK styleCss
      path "/app.js"    >=> setMimeType "application/javascript"
                         >=> OK appJs
      pathScan "/tile/%d" tile
      path "/fast"      >=> setMimeType "text/plain; charset=utf-8"
                         >=> OK "fast"
      path "/slow"      >=> slow
      path "/large"     >=> large
      path "/info"      >=> setMimeType "text/plain; charset=utf-8"
                         >=> OK infoText
    ]
    RequestErrors.NOT_FOUND "not found"
  ]

// ---------------------------------------------------------------------------
// Entry point — same minimal CLI shape as the H2SpecHost fixture so this
// example can also be driven from scripts.
// ---------------------------------------------------------------------------

let private parsePort (argv: string[]) =
  let rec loop i =
    if i >= argv.Length then 8443us
    elif argv.[i] = "--port" && i + 1 < argv.Length then
      match UInt16.TryParse argv.[i + 1] with
      | true, p -> p
      | false, _ -> 8443us
    else loop (i + 1)
  loop 0

[<EntryPoint>]
let main argv =
  let port = parsePort argv
  let cts = new CancellationTokenSource()
  let cfg =
    { defaultConfig with
        bindings          = [ HttpBinding.create HTTP IPAddress.Loopback port ]
        cancellationToken = cts.Token }

  let ready, server = Web.startWebServerAsync cfg app
  ready |> Async.RunSynchronously |> ignore

  printfn ""
  printfn "Suave HTTP/2 demo listening on http://127.0.0.1:%d/" (int port)
  printfn ""
  printfn "Try one of:"
  printfn "  curl --http2-prior-knowledge -v http://127.0.0.1:%d/" (int port)
  printfn "  nghttp -nv http://127.0.0.1:%d/" (int port)
  printfn "  h2load -n 100 -c 1 -m 10 http://127.0.0.1:%d/tile/1" (int port)
  printfn ""

  Console.CancelKeyPress.Add(fun e ->
    e.Cancel <- true
    cts.Cancel())

  server.Wait()
  0
