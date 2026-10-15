## Unreleased
* **Behaviour change:** `defaultErrorHandler` no longer returns the exception message and stack trace to "local" clients; every client gets a plain `500 Internal Server Error`. The local check used the connection address, or an `X-Forwarded-For`/`X-Real-IP` header the client could set to `127.0.0.1`; behind a reverse proxy on the same host (nginx -> 127.0.0.1) every remote client received stack traces. For development, opt in with the new `developmentErrorHandler` (`{ defaultConfig with errorHandler = developmentErrorHandler }`), which shows the HTML-encoded details to every client
* HTTP/2 requests now carry the binding they arrived on in `request.binding`; it was the default `http://0.0.0.0:8080`, so `request.binding.scheme.secure` was `false` and `request.url` was wrong for HTTPS clients that negotiated HTTP/2 (a redirect-to-HTTPS check looped for them)
* Docs: the deployment guide uses .NET 10 and the `runtime` image with a non-root user and a curl-free health check, shows graceful shutdown on SIGTERM, fixes the nginx WebSocket/keep-alive setup, uses `X509CertificateLoader`, and replaces the ASP.NET Core Module IIS example with HttpPlatformHandler; the HTTPS recipe's redirect keeps the query string and port
* `Validation.String` validators (`minLength`, `maxLength`, `exactLength`, `lengthBetween`, `matches`, `email`, `alphanumeric`, `alpha`, `numeric`, `contains`, `startsWith`, `endsWith`) return `Invalid` with "Value is required" for a `null` input instead of throwing `NullReferenceException`, e.g. for a field System.Text.Json left `null`; `notEmpty` is unchanged
* `ValidationWebParts.validationErrorsToJson` writes its output with `Utf8JsonWriter`, so fields and messages containing `"`, `\` or control characters produce valid JSON; the shape is unchanged, the output is now compact (no spaces), and characters such as `'`, `<` and non-ASCII are written as `\uXXXX` escapes
* Breaking: the `<++>` operator now preserves left-to-right order: `a <++> b` reports `a`'s errors before `b`'s and returns `a`'s value when both pass (previously the reverse). `a |> andAlso b` is unchanged
* `HttpRequest.query` returns `(key, None)` for a query key without a value (`?flag`), as documented, instead of `(key, Some "")`; `?key=` is still `(key, Some "")`. `queryFlag "flag"` is now `true` for `?flag`, and `queryParam "flag"` returns `Choice2Of2` for it. Form parsing (`form`, `formData`) is unchanged
* `pathScan`, `pathScanCi` and `Sscanf.sscanf` no longer wrap integers that are out of range for the target type: `/items/99999999999` against `pathScan "/items/%d"` with an `int` handler now declines instead of passing `1215752191`; the same applies to `int16`, `uint16` and `uint32` (and `%x`/`%X`/`%o` values that do not fit)
* **Behaviour change:** `CORS.defaultCORSConfig` no longer allows credentials (`allowCookies = false`). It used to reflect any `Origin` together with `Access-Control-Allow-Credentials: true`, which lets any web site make authenticated requests as your users. To allow credentials, list the trusted origins too: `{ defaultCORSConfig with allowedUris = InclusiveOption.Some [ "https://app.example.com" ]; allowCookies = true }`
* `cors` adds `Vary: Origin` (merged into an existing `Vary`) so caches do not serve one origin's CORS response to another
* `cors` no longer sends `Access-Control-Allow-Methods: GET,` for a single allowed method; methods are joined as `GET, POST`
* Add `CORS.withCORS config handler`, which answers preflight requests with 204 itself and runs `handler` only for actual requests, and `CORS.corsPreflight config`, which answers preflights and declines other requests (`choose [ corsPreflight cfg; cors cfg >=> app ]`). `cors cfg >=> handler` still lets the handler replace the preflight's 204, as documented
* **Behaviour change:** `RateLimit.defaultKeyExtractor` keys by the connection's remote address and ignores `X-Forwarded-For`/`X-Real-IP`, which a client could change on every request to avoid the limit. Behind a reverse proxy use the new `RateLimit.trustedProxyKeyExtractor [ proxyAddress ]`, which reads `X-Forwarded-For` only on connections from the listed proxies. `userKeyExtractor` and `headerKeyExtractor` fall back to the new default
* Each `rateLimit` WebPart keeps its own counters. All limiters used to share one store keyed only by the extracted key, so two limiters keyed by IP counted together, and limiters with different strategies denied every request after the first (e.g. a `FixedWindow` limiter returned 429 after a `SlidingWindow` one had seen the same IP). Idle entries are still cleaned up. `getStats` returns the first limiter that has an entry for the key
* Add `RateLimit.defaultRateLimitConfig`; `RateLimit.defaultConfig` is obsolete because after `open Suave.RateLimit` it shadows the server's `defaultConfig`. The obsolete alias still shadows it until it is removed; use `Web.defaultConfig` meanwhile
* `securityHeaders` replaces headers of the same name already on the response instead of adding a second copy (e.g. two `Cache-Control` headers)
* Security header presets send `X-XSS-Protection: 0` instead of `1; mode=block`. `strictConfig` no longer sends the deprecated `block-all-mixed-content` or HSTS `preload` (opt in with `withHSTS 31536000 true true`), and sends `Cache-Control: no-store` instead of `no-store, no-cache`
* `Files.sendFile` (and so `file`, `browse`, `browseHome`, `browseFile`) treats `Range` byte positions as inclusive: `bytes=2-5` is now answered with `Content-Range: bytes 2-5/10` and four bytes instead of three. Open-ended (`bytes=5-`) and suffix (`bytes=-3`) ranges are supported, a range past the end is clamped, a range starting at or after the end is answered with `416` and `Content-Range: bytes */<length>`, and malformed, multi-range or non-`bytes` headers get the whole file. Partial responses are never compressed
* `Files.sendFile` no longer sends `Content-Range` on full `200` responses, compressed or not; it is only sent with `206` and `416`
* Upload temp files written for `request.files` are deleted once the response has been written (or the connection ends) instead of being left in the temp folder. Move or copy an upload during the request to keep it; streaming it back with `Files.sendFile` still works. Files written by a custom `filePartSink` are not touched
* `Files.browse`, `browseHome`, `browseFile`, `browseFileHome` and `dir` decline a request whose path escapes the root (such as `/../secret.txt`) instead of throwing, so it gets the application's 404 rather than a 500. New `Files.tryResolvePath`; `resolvePath` still throws
* When the compressed-files folder cannot be written to (the default, `AppContext.BaseDirectory`, is read-only under `dotnet fsi` and in read-only containers), compressible files are served uncompressed instead of the connection being closed without a response; the failure is reported once per folder through `Globals.writeMessage`
* Docs: the file upload recipe and the file handling guide no longer build the destination path from the client-supplied file name, which allowed writing outside the upload folder with `filename=../../x`
* The Suave.DotLiquid package declares `DotLiquid >= 2.3.197`; 3.6.1 and earlier declared `DotLiquid` with no lower bound (`0.0`), so NuGet picked DotLiquid 0.0.0 and the first render failed with `MissingMethodException` (`Template.set_NamingConvention`). Workaround for those versions: reference `DotLiquid` 2.3.197 explicitly. The DotLiquid docs now use `| escape` in their examples (DotLiquid does not HTML-escape output) and cover the response charset and template field naming
* `open Suave` no longer shadows F#'s `Result.Ok`/`Result.Error`: the parser's `ScanResult` type (`NeedMore | Found | Error`) leaked its `Error` case into the `Suave` namespace, so bare `Error "x"` or `| Error e ->` in user code resolved to it. `ScanResult` is now internal and requires qualified access, and `HttpReader.scanMarker` and `HttpReader.skip`, which returned it, are now internal too. Code that matched on `ScanResult` or called those members must use `HttpReader.readUntilPattern` instead
* Fix multipart/form-data bodies whose `multipart/mixed` part is followed by another part: the parser did not wait for the rest of the outer boundary line to be skipped, so when that data had not arrived yet the request hung or lost the parts after it

## New in v3.6.1 (Released 2026-10-07)
* Keep-alive CPU per request is now within ≈1% of ASP.NET Core Minimal API on Linux with the server pinned to 4 cores (8.98 vs 8.89 µs, down from ≈9.7 µs in v3.6.0) (#865)
* Suave is compiled without F# `.tail` calls: `async` continuations in `bind` (`>=>`), `choose`, `tryThen`, `inject`, `<|>`, the router and others passed the `HttpContext` struct on as a tail call, which went through the runtime's slow, copying tail-call helper (#865)
* The request head is taken from the input with one pipe read and parsed in place, instead of three read/advance pairs; `TcpTransport` no longer takes a lock to fetch the socket on every read and write (#865)
* Benchmarks: `benchmarks/cpu-per-request.sh` measures server CPU time per request; `benchmarks/HTTP_COMPARISON.md` documents the pinned-core and `perf` profiling method (#865)

## New in v3.6.0 (Released 2026-10-05)
* Keep-alive performance: allocations per request fall from ≈5.7 KB to ≈1.24 KB for a routed empty response. In a same-host Linux comparison against ASP.NET Core Minimal API, Suave is now ahead at 256 connections and within ≈5% at 64 (#862, #864):
  * the connection's request loop waits for a complete request head, then parses the request line and headers synchronously; fragmented, oversized (beyond the 8 KiB line buffer), non-HTTP/1.x and body-carrying requests continue on the streaming parser
  * a request whose parsing, web part and response complete synchronously allocates no `Task`; `processRequest` and `run` keep their `Task` signatures
  * TCP and HTTPS reads and web part executions complete through reusable per-connection completion sources instead of a `task { }` wrapper and `Async.StartImmediateAsTask` per request; web part errors are recovered there with the same semantics as `async.TryWith`
  * reads no longer register on the server's cancellation token individually: each connection registers once and is shut down when the server stops, which also ends a pending read
  * no error message allocations for absent request headers, h2c checks only for `Upgrade: h2c` requests, allocation-free `Content-Length` formatting, status line and ASCII response headers written without task state or `ToLowerInvariant`, no URL decoding of paths without escapes, and a single exact-route lookup in `Router`
* New `HttpReader` members (`requestHeadReady`, `receive`, `commitReceived`, `tryReadRequestLineBuffered`, `tryReadHeadersBuffered`) used by the request loop (#862)
* Request header names are matched with ordinal case-insensitive comparison; names with invalid Unicode no longer match through linguistic equivalence (#862)
* Add `Globals.messageWriter` to redirect or silence Suave's own console messages (startup banner, "Stopping TCP server", websocket disconnects); the test suite uses it to avoid a Console lock deadlock with Expecto that caused intermittent hangs (#863)
* Evict obsolete compressed artifacts: a recompressed resource's superseded copy is now deleted as part of the atomic cache swap, and the `_temporary_compressed_files` folder is swept - age- and count-bounded, see `Compression.cleanupFolder` - on server startup and shutdown (#655)
* Cached compressed copies that have gone missing from disk are treated as a cache miss instead of failing the request
* Benchmarks: a Minimal API comparison server, balanced comparison and profiling scripts, and an exact per-request allocation probe; see `benchmarks/HTTP_COMPARISON.md` (#862)

## New in v3.5.0 (Released 2026-09-09)
* Connection health checker: `startHealthChecker` now takes the server's cancellation token and returns a disposable handle, which each connection pool owns and disposes; `stopHealthChecker` actually cancels the loop (#854)
* Mark EventSource (SSE) connections as long-lived so the health checker's maximum connection age no longer closes them, and reset the flag when the stream ends (#854)
* Add hideStartupMessage config flag to silence the listener banner (#850)
* Preserve Secure on CookieStateStore updates (#852)
* Include compression algorithm in the file-cache key (#848)
* Fix canonicalization issue in Files.resolvePath (#847)

## New in v3.4.6 (Released 2026-08-26)
* Fix static file fallback for missing resources (#844)

## New in v3.4.5 (Released 2026-08-26)
* Packages now declare the Apache-2.0 license as an SPDX expression (`licenseExpression`, with `licenseUrl` kept at the canonical `licenses.nuget.org` address for older clients) and carry repository metadata
* Allow consuming projects to use FSharp.Core versions newer than 10.0.102 (#840)
* Refresh `README.md` to describe current v3.x capabilities and remove the legacy Jekyll `docs/` tree in favor of `website/` (guides) and `docs-api/` (API reference)
* Modernize the reverse proxy to use `System.Net.Http.HttpClient` (shared, static instance) instead of the obsolete `WebRequest`/`HttpWebRequest` API
* Proxy: set `X-Forwarded-For` from the client address (appending to any existing chain), target `Host` at the upstream authority, and forward the client's host/scheme as `X-Forwarded-Host`/`X-Forwarded-Proto`
* Fix HPACK empty string and overlong len decode

## New in v3.4.4 (Released 2026-07-13)
* HPACK bomb defence: bound decoded header-list size and reassembly buffer (#829)

## New in v3.4.3 (Released 2026-06-01)
* Fix broken While implementation in the socket CE (00ab06e)

## New in v3.4.2 (Released 2026-05-31)
* Fix CPU spin in readPostData on zero-length first pipe segment (8c2c1d6)

## New in v3.4.1 (Released 2026-05-30)
* Fix CPU spin on half-closed WebSocket (#828)

## New in v3.4.0 (Released 2026-05-18)
* Full HTTP/2 support.

## New in v3.4.0-beta (Released 2026-05-14)
* Full HTTP/2 support.

## New in v3.3.1 (Released 2026-05-06)
* Multiple listening sockets / SO_REUSEPORT support on Linux/BSD.
* Several performance optimizations.

## New in v3.3.1-beta3 (Released 2026-05-06)
* Multiple listening sockets / SO_REUSEPORT support on Linux/BSD.

## New in v3.3.1-beta2 (Released 2026-05-05)
* Remove unneeded pipe lock, optimize write response.

## New in v3.3.1-beta1 (Released 2026-05-05)
* Reduce string allocations, vectorized line-marker scanning.

## New in v3.3.1-beta (Released 2026-05-04)
* Several performance optimizations (#798)

## New in v3.3.0 (Released 2026-05-02)
* Add optional FilePartSink to configuration record to stream uploads without temp files. (#796)

## New in v3.2.3 (Released 2026-01-16)
* Update dependencies.

## New in v3.2.2 (Released 2026-01-13)
* FIX: Remove incorrect raise exception within accept loop leading to premature exit.

## New in v3.2.1 (Released 2026-01-04)
* Rewrite connection shutdown sequence to avoid race conditions under high concurrency load.

## New in v3.2.0 (Released 2025-12-11)
* Target .NET 10

## New in v3.2.0-beta (Released 2025-12-11)
* Target .NET 10

## New in v3.1.1 (Released 2025-12-11)
* FIX: Equal signs in query parameter values are not parsed correctly (#774)

## New in v3.1.0 (Released 2025-12-9)
* Target .NET 9
* Rewrite of internals using Task, Memory and Pipelines.
* Many optimizations focusing on reducing memory allocations, improving throughput, and lowering latency.
* Rate limiting, security headers, validation webparts and new router module.

## New in v3.1.0-beta8 (Released 2025-12-6)
* Correct the connection shutdown sequence.

## New in v3.1.0-beta7 (Released 2025-11-24)
* FIX: Flush after serving a file.

## New in v3.1.0-beta6 (Released 2025-11-24)
* Improved KMP string matching algorithm
* Get rid of Globals.numberOfClients

## New in v3.1.0-beta5 (Released 2025-11-23)
* FIX: HTTP response data bleeding between requests during stress load

## New in v3.1.0-beta4 (Released 2025-11-23)
* Security fix: clear buffers for reuse.
* Handle malformed HTTP headers.

## New in v3.1.0-beta3 (Released 2025-11-22)
* Implements a connection health monitor.

## New in v3.1.0-beta2 (Released 2025-11-19)
* Enable SSL/TLS support.
* Adopting ValueTask<T> to achieve allocation-free synchronous completions
* Plus many more performance optimizations.

## New in v3.1.0-beta1 (Released 2025-11-05)
* Rate limiting, security headers, validation webparts and new router module.

## New in v3.1.0-beta (Released 2025-11-02)
* Target net9.0
* Many optimizations focusing on reducing memory allocations, improving throughput, and lowering latency.

## New in v2.7.0-beta1 (Released 2023-07-03)
* Rewrite of internals using Task, Memory and Pipelines. Introduces breaking changes.

## New in v2.7.0-beta (Released 2023-03-03)
* Target net7.0
* Introduces `okStream` and `okStreamChunked` (#765)

## New in v2.6.2 (Released 2022-01-26)
* Removing keys from session state (#757)
* FIX: Issue parsing boundary from multipart/form-data Content-Type header (#759)

## New in v2.6.1 (Released 2021-05-15)
* Allow to set custom Server header (#755)
* Report assigned ephemeral port  when binding to port 0 (#754)

## New in v2.6.0 (Released 2020-12-07)
* Target netstandard2.1 and drop net461
* Introduces proxy web part
* Optimization: use byte arrays instead of strings for hardcoded literals
* FIX: Sequence map bug in getHeaders (#747)

## New in v2.6.0-beta (Released 2020-07-18)
* Makes HttpContext.userState a mutable dictionary enabling global user state per request.
* FIX: Prevent showing detailed errors when proxied
* Removes Owin module.
* Removes lensed properties.

## New in v2.5.6 (Released 2019-09-06)
* Added method NOT_IMPLEMENTED (#733)
* FIX: CoreRT compiled binaries not working (#735)

## New in v2.5.5 (Released 2019-06-04)
* Support for "x-forwarded-for".
* A few changes to logging.

## New in v2.5.4 (Released 2019-04-14)
* Fix compilation error (#728)

## New in v2.5.3 (Released 2018-11-10)
* Fix regression in Owin module (#717)

## New in v2.5.2 (Released 2018-10-11)
* Fixes type name collision issue (#715)

## New in v2.5.1 (Released 2018-09-29)
* Fix multiple problems with last-modified handling (#713).
* Introduces combinator `Redirection.see_other` (#712).

## New in v2.5.0 (Released 2018-09-08)
* This release brings a few performance fixes which allows Suave to start competing in the TechEmpower Framework Benchmarks.
* Fix #706 - Websockets don't work in Firefox.
* This release also includes breaking changes in the Suave.Logging namespace as we diverge from Logary facade.

## New in v2.5.0-beta2 (Pre-released 2018-07-31)
* Fix #706 - Websockets don't work in firefox

## New in v2.5.0-beta (Pre-released 2018-07-27)
* Performance optimizations (mostly by removing harmful or unneeded async code)

## New in v2.4.3 (Released 2018-06-24)
* Fix #702 - Can't parse  negatives number in forms

## New in v2.4.2 (Released 2018-06-20)
* Improve pathScan logic

## New in v2.4.1 (Released 2018-06-10)
* Removed external source code references from build process to address licensing concerns.

## New in v2.4.0 (Released 2018-04-23)
* Cleanup of non-.Net Core cruft (reducing maintenance burden)
* Move build to FAKE 5
* Upgrade Logary Facade to v3 (Gauge Int64 -> Float)
* More liberal nuget constrains
