# Suave HTTP/2 demo

A miniature web application that exercises every major feature of Suave's
in-tree HTTP/2 implementation. Use it as both a manual smoke test and a guided
tour of the protocol.

## Run

```bash
dotnet run --project examples/Http2Demo/Http2Demo.fsproj -- --port 8443
```

The server prints the URL it is listening on. The port defaults to `8443`;
pass `--port N` to override.

## What gets demonstrated, and where

| HTTP/2 feature        | Where to see it                                                                                                                                                          |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **h2c upgrade**       | `curl --http2 http://localhost:8443/` — Suave handles the `Upgrade: h2c` handshake via `Suave.Http2.H2cUpgrade`.                                                          |
| **h2c prior knowledge** | `curl --http2-prior-knowledge http://localhost:8443/` — Suave consumes the HTTP/2 connection preface directly via `Suave.Http2.H2cPriorKnowledge`.                     |
| **Multiplexing**      | The `/` page references 16 SVG tiles plus a CSS + JS file. A browser (or `nghttp`) fires one TCP connection and many concurrent streams to pull them.                    |
| **Server Push**       | `/` calls `Suave.Http2.Push.push "/style.css" …` and `Suave.Http2.Push.push "/app.js" …` before returning the HTML. The writer emits `PUSH_PROMISE` frames on the parent stream and synthesises the pushed responses. |
| **HPACK**             | Every response runs through `Suave.Http2.encodeHpackHeaderBlock`. Repeated headers compress to one or two bytes per stream after the first request on a connection.     |
| **Prioritization**    | `/slow` sleeps 500 ms before its body is queued. Concurrent requests to `/fast` overtake it on the wire, because frame ordering is driven by stream readiness.           |
| **Flow control**      | `/large` returns 256 KiB — comfortably larger than the default 64 KiB initial window. The client must send `WINDOW_UPDATE` frames before the writer can finish the body. |
| **Interleaving frames** | Fetch `/large` and `/fast` in parallel; the connection trace shows `DATA` frames from each stream interleaved.                                                         |

## Suggested invocations

```bash
# Show the negotiated protocol and pushed resources:
curl --http2-prior-knowledge -v http://localhost:8443/

# Verbose HTTP/2 frame trace (PUSH_PROMISE, HEADERS, DATA, WINDOW_UPDATE):
nghttp -nv http://localhost:8443/

# Multiplex 100 requests over a single connection, 10 in flight at a time:
h2load -n 100 -c 1 -m 10 http://localhost:8443/tile/1

# Confirm flow-control / WINDOW_UPDATE for the 256 KiB body:
curl --http2-prior-knowledge -v -o /dev/null http://localhost:8443/large
```

## Routes

| Path        | Description                                                              |
| ----------- | ------------------------------------------------------------------------ |
| `/`         | HTML index. Records two `PUSH_PROMISE` intents, then renders.             |
| `/style.css`| Small CSS asset (also pushed alongside `/`).                              |
| `/app.js`   | Small JS asset (also pushed alongside `/`).                               |
| `/tile/N`   | 48 × 48 SVG tile, deterministic colour per `N` (1 ≤ N ≤ 1024).            |
| `/fast`     | Immediate 200 OK with body `fast`.                                        |
| `/slow`     | 200 OK with body queued after a 500 ms delay.                             |
| `/large`    | 256 KiB application/octet-stream body — exceeds the default HTTP/2 window.|
| `/info`     | Plain-text version of this table.                                         |

## Notes

* The example is plain HTTP/2 over cleartext (h2c). Browsers only speak
  HTTP/2 over TLS, so to see Server Push in a browser you need to put this
  behind a reverse proxy that terminates TLS (or extend the example with
  TLS bindings). Command-line clients (`curl`, `nghttp`, `h2load`) talk h2c
  directly and exercise every feature listed above.
* The same `WebPart` serves both HTTP/1.1 and HTTP/2 clients. The push
  intents recorded by `Suave.Http2.Push.push` are silently ignored on
  HTTP/1.1 connections, so the example degrades cleanly.
