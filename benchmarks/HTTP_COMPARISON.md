# Suave and Minimal API HTTP comparison

This harness compares the current Suave source with ASP.NET Core Minimal API.
It is a local diagnostic, not a reproduction of the Linux leaderboard at
<https://web-frameworks-benchmark.vercel.app/>. In particular, the generator is
`oha`, not the leaderboard's `zrk` rate sweep, and client and servers share a host.

## Workload

Both servers run .NET 10 Release builds with server GC and tiered PGO. The
comparison uses HTTP/1.1 over loopback, without TLS or request logging. They expose:

| Request | Status | Body |
| --- | --- | --- |
| `GET /` | 200 | Empty |
| `GET /user/42` | 200 | `42` |
| `POST /user` | 200 | Empty |

Responses use `Content-Type: text/html`, an explicit content length, and no
`Server` header. Minimal API explicitly sets these headers to match Suave, so
its endpoint differs slightly from the upstream benchmark's no-op endpoint.
Only `GET /` is load-tested; the other routes are correctness smoke checks.
Suave uses its router, default connection limit and health checker, one acceptor,
and an 8192-byte buffer. The existing Pong workload is unchanged without
`--comparison`.

## Run

Requirements: the repository's .NET SDK and restored Paket dependencies,
`oha`, `jq`, `curl`, and Bash. Run commands from the repository root.

```sh
dotnet build benchmarks/PongServer -c Release
dotnet build benchmarks/MinimalApi -c Release
```

Start each server in a separate terminal; stop them with Ctrl+C afterwards:

```sh
dotnet benchmarks/PongServer/bin/Release/net10.0/PongServer.dll --comparison
```

```sh
dotnet benchmarks/MinimalApi/bin/Release/net10.0/MinimalApi.dll
```

Suave listens on port 3000 and Minimal API on 3001. Use a fresh output directory
for every run. Each server receives a 10-second warmup before measured trials.
Trials run sequentially with alternating order; raw JSON includes errors,
status codes, throughput, and latency. Any unsuccessful measured trial aborts
the comparison. Printed columns are server, mode, concurrency, repetition,
requests/second, and p99 milliseconds.

```sh
MODES=keepalive REPETITIONS=6 DURATION=10s \
  bash benchmarks/compare-http.sh profile-output/comparison/my-run
```

Defaults are concurrency 64, 256, and 512; three repetitions; and both
`keepalive` and `close` modes. Override `CONCURRENCIES`, `REPETITIONS`, `DURATION`,
`WARMUP`, or `MODES` as needed. `REQUESTS` replaces timed trials with a fixed
request count. Long connection-close runs can exhaust macOS client ephemeral
ports and produce `Can't assign requested address`; these trials are invalid.
A bounded smoke check is:

```sh
MODES=close CONCURRENCIES=64 REPETITIONS=3 REQUESTS=3000 \
  bash benchmarks/compare-http.sh profile-output/comparison/my-close-smoke
```

For before/after comparisons, preserve a published build of the current source
**before making the change**, then run that build on port 3000. This is a source
baseline, not a comparison against an older Suave release:

```sh
dotnet publish benchmarks/PongServer -c Release \
  -o profile-output/comparison/baseline-server
dotnet profile-output/comparison/baseline-server/PongServer.dll --comparison
```

After rebuilding the changed source, start its server in another terminal:

```sh
SUAVE_BENCHMARK_PORT=3002 \
  dotnet benchmarks/PongServer/bin/Release/net10.0/PongServer.dll --comparison
```

With Minimal API also running, use six repetitions to cover all six server-order
permutations:

```sh
SUAVE_PORT=3002 BASELINE_PORT=3000 MODES=keepalive REPETITIONS=6 DURATION=5s \
  bash benchmarks/compare-http.sh profile-output/comparison/my-three-way-run
```

Outputs record the SDK, OS, generator version, parameters, revision and tracked
source diff. They do not capture untracked source files or identify previously
published binaries; preserve those separately when sharing results. Keep builds,
profilers, and unrelated CPU-heavy applications out of throughput runs. A
dedicated server and separate load generator are needed for capacity claims.

## Profile

Install `dotnet-trace` and `dotnet-counters` separately. The script starts a
25-second load alongside a 20-second capture. Pass the actual server PID and
port, not the PID of a `dotnet run` parent. Trace collection uses the
`dotnet-sampled-thread-time,gc-verbose` profiles (tested with dotnet-trace 9).

```sh
bash benchmarks/profile-http.sh SERVER_PID profile-output/comparison/cpu 3000
COUNTERS=1 RATE=10000 bash benchmarks/profile-http.sh \
  SERVER_PID profile-output/comparison/allocations 3000
```

The second command caps the load at 10,000 requests/second. Check the generated
load JSON for successful responses and achieved rate before interpreting the
counters. Exclude startup/shutdown counter samples, then divide allocation
bytes/second by achieved requests/second. This estimates process-wide bytes per
request, including diagnostic and background activity. Profiled throughput is
not a peak score; sampled thread time also includes waiting, not just CPU work.

## Local Results: 2026-10-03

Environment: macOS ARM64, eight cores, 16 GiB RAM, .NET SDK 10.0.400,
`oha` 1.12.1. Baseline was the unmodified current library at `edb9c743`, using
the same comparison application. Candidate contains the header-lookup and h2c
short-circuit changes only. Each concurrency received its own 10-second warmup
per server and six 5-second trials per server, in balanced order.

Median successful requests/second, rounded:

| Connections | Current-source baseline | Candidate | Minimal API |
| --- | ---: | ---: | ---: |
| 64 | 152,435 | 154,483 | 173,435 |
| 256 | 148,848 | 151,885 | 172,856 |
| 512 (unstable) | 92,152 | 96,850 | 123,995 |

At 64 and 256 connections, candidate medians were about 1.3% and 2.0% above
baseline, but still 10.9% and 12.1% below Minimal API. These small gains are not
statistically established: drift was visible even after other workloads were
paused. The 512-connection run was especially unstable: candidate throughput
ranged from 59,911 to 116,516 requests/second, making its median unsuitable as
evidence of an improvement. No keep-alive trial had failed responses.

Fixed-rate allocation captures achieved approximately 9,999 successful
requests/second on each server. Averaging 17 steady samples (discarding the first
two and last one) gave approximately:

| Server | Allocated bytes/request |
| --- | ---: |
| Current-source baseline | 6,521 |
| Candidate | 6,097 |
| Minimal API | 1 |

The candidate saved about 424 bytes/request, or 6.5%. Minimal API was effectively
allocation-free in steady state for this empty response; its small residual is
process-level background/diagnostic activity, not a precise endpoint allocation
measurement. Suave still allocates roughly 6 KB/request. Profiles point to parser
task-state-machine and request-pipeline allocations as the next investigation,
not proof that any particular rewrite will improve throughput.

All three servers also completed three 3000-request connection-close trials at
64 connections without errors. These short trials validate behavior, not
sustained connection-close capacity. Earlier long close trials exhausted client
ports and were rejected.

The local raw evidence is under `profile-output/comparison/`: `final-c64`,
`final-c256`, `final-c512`, `final-close-smoke`, and
`{baseline,candidate,minimal}-allocations-{load,counters}.json`. This directory is
ignored by Git. Earlier exploratory results there are not final evidence.
Request prefetch, a reusable async completion bridge, and socket inline
completions did not show convincing gains and were not retained.

**Parity has not been reached.** The retained changes reduce allocations with
small local throughput gains, but dedicated-host Linux measurements and further
work on request-processing allocations are still needed.

## Task/Async Allocation Research

The request boundary is in `HttpOutput.run`: a Task computation awaits the
Async returned by `HttpOutput.executeTask`, which wraps the Async-based WebPart
in error handling. This path can complete synchronously on the calling thread;
the boundary does not inherently require a thread-pool hop. Replacing an implicit
Async bind with an explicit `Async.StartImmediateAsTask` is not an optimization.

Local inspection checklist for `HttpOutput`: one implicit Async-to-Task await
in `run`; one Async error-handling wrapper in `executeTask`; zero explicit
`Task.Run`, `StartAsTask`, `AwaitTask`, `.AsTask()`, `.Result`, or `.Wait()` calls.
The empty benchmark WebPart does not perform an application Task-to-Async-to-Task
round trip. Applications awaiting Task-based I/O from an Async WebPart can add
such round trips, but that is a different workload.

The allocation-only mode in [PongServer/RequestAllocations.fs](PongServer/RequestAllocations.fs)
uses the real WebPart, router, and `HttpOutput.executeTask`, without sockets or
response writing:

```sh
dotnet run -c Release --project benchmarks/PongServer -- --allocations
```

Each case warms up for 20,000 operations, measures 200,000 operations, and repeats
three times. Results must already have completed successfully before consumption,
so `GC.GetAllocatedBytesForCurrentThread` covers the whole measured operation
without blocking or missing allocations on continuation threads. These are
synchronous-path measurements, not estimates for suspended I/O. Use stabilized
later rounds: tiered compilation changed allocations during the first round.
Printed nanosecond timings are diagnostic only, not HTTP throughput predictions.

On .NET runtime 10.0.11, FSharp.Core assembly 10.0.0.0, server GC, the final two
rounds of the initial research agreed on these allocations, before the wrapper
simplification described below:

| Case | Bytes/call |
| --- | ---: |
| Task awaiting an already-completed Task (control) | 72 |
| `Async.StartImmediateAsTask` on a prebuilt Async | 496 |
| Task awaiting a prebuilt Async | 568 |
| Task awaiting `OK ""` | 824 |
| Task awaiting the routed WebPart | 1,152 |
| Task awaiting the original `executeTask(routed WebPart)` | 1,712 |
| Same shape with tail-return Async error wrapper | 1,544 |
| Same shape with direct `async.TryWith` and precreated error handler | 1,344 |

The 1,712-byte synchronous probe decomposes into a 72-byte containing Task,
496 bytes for Async startup/bridging, 256 bytes for the `OK` WebPart, 328 bytes
for routing, and 560 bytes for the original error wrapper. These are differences
between controlled cases, not independent numbers to add to the live request
counter. The actual request also parses input, handles connection state, and
writes a response.

The direct `TryWith` prototype saves 368 bytes/call without replacing the public
Async-based WebPart API or implementing a custom completion source. The probe
checks success, delayed completion, ordinary faults, an ordinarily raised
`OperationCanceledException`, and cancellation through the Async cancellation
continuation. These are basic checks, not complete integration coverage of
ambient cancellation, execution context, or application error handlers.
The initial research was benchmark-only. The production follow-up below adopts
direct `TryWith`; the probe retains the original wrapper for comparison.

The earlier `quiet-candidate.nettrace` also confirms a separate allocation
source. Reading GC allocation ticks between 2 and 19 seconds gives these leading
sample weights: strings 10.1%; `readUntilPattern` Task state machines 9.1%;
`processRequest` Task state machines 7.2%; `readHeadersInto` callbacks 6.8%;
`scanMarkerAsync` Task state machines 6.2%; and `readRequest` Task state machines
4.5%. The listed Task state-machine types together account for roughly 35% of
sample weight. **`AsyncStateMachineBox` is a .NET Task implementation type, not
evidence of an F# Async conversion.** Waiting for incoming bytes can suspend
several nested Task computations and allocate each state-machine box.

Allocation ticks are sampled and cannot give exact per-type bytes/request.
That capture predates the final header-comparison adjustment and the wrapper
simplification below. Its weights must not be treated as an
exact partition of the later 6.1 KB/request counter measurement. Local evidence:
`profile-output/comparison/boundary-allocations.log`, `allocation-types.log`, and
`read-allocations.fsx` (the latter uses TraceEvent assemblies bundled with the
installed dotnet-trace tool).

The next research target after simplifying the wrapper is parser callback
captures and the nested suspension path.
Moving the entire I/O loop to Async, adding `Task.Run`, or replacing every Task
with ValueTask is not supported by these results. The Task/Async boundary is a
measurable contributor, but not the sole or largest combined allocation source.

### Implemented Error Wrapper

`HttpOutput.executeTask` now calls `async.TryWith` directly with an error-handler
closure created once per `HttpOutput`. Error-context creation remains inside the
handler, so it occurs only on a fault. The WebPart API and Async return type are
unchanged. The production allocation probe now reports 1,344 bytes/call for the
routed wrapped WebPart, matching the prototype and saving 368 bytes/call.

Six regression tests cover lazy/repeated execution, handled and unhandled
results, synchronous and delayed exceptions, error-handler arguments and
recovery, failing error handlers, cancellation continuations, pre-canceled and
in-flight ambient tokens, and execution-context flow. All 409 tests passed;
the complete solution built with zero errors (38 existing warnings).

The live follow-up used a preserved pre-wrapper build of the current source,
including the earlier header optimizations, as its baseline. The candidate
differs only in the wrapper implementation. The same matched server applications
and machine were used, with six balanced 5-second trials at 64 connections after
10-second warmups. All responses succeeded:

| Server | Median requests/second |
| --- | ---: |
| Pre-wrapper Suave baseline | 172,044 |
| Simplified-wrapper Suave | 177,405 |
| Minimal API | 191,324 |

The candidate was faster than baseline in all six rounds; its median was 3.1%
higher. It remained 7.3% below Minimal API. These are same-host observations,
not a statistically established gain or a Linux leaderboard result; compare
within this batch rather than against the earlier runs' absolute rates.

Separate fixed-rate captures achieved approximately 9,999 requests/second with
no errors on both Suave builds. After excluding the first two and last counter
samples, the mean of 17 samples divided by achieved rate gave:

| Server | Approximate allocated bytes/request |
| --- | ---: |
| Pre-wrapper Suave baseline | 6,097 |
| Simplified-wrapper Suave | 5,729 |

The live reduction was approximately 368 bytes/request (6.0%), consistent with
the isolated probe. This remains process-level counter accounting rather than
an exact object-by-object allocation census.

Local artifacts under `profile-output/comparison/`: `wrapper-baseline-server`,
`wrapper-c64`, `wrapper-{before,after}-allocations-{load,counters}.json`,
`wrapper-candidate-probe.log`, `wrapper-tests.log`, `wrapper-full-tests.log`, and
`wrapper-solution-build.log`.
## Request-Processing Allocations: 2026-10-03 (second pass)

### Exact allocation metric

Counter averages are coarse, so this pass measures allocations exactly. Started
with `SUAVE_ALLOC_PROBE=1`, the comparison server also answers on port + 100 with
`GC.GetTotalAllocatedBytes(true)`. The probe listens on a side port so the measured
WebPart is unchanged; wrapping it in `choose` for an extra route would add about
10% of allocations. The script warms up, reads the counter, sends a fixed number of
keep-alive requests with `oha`, and reads the counter again:

```sh
SUAVE_ALLOC_PROBE=1 SUAVE_BENCHMARK_PORT=3002 \
  dotnet benchmarks/PongServer/bin/Release/net10.0/PongServer.dll --comparison
bash benchmarks/alloc-per-request.sh 3002 200000 64
```

The result counts all server allocations, including suspended I/O paths, divided by
requests. It was stable to within a few bytes between runs and between 1 and 256
connections. Allocation sites came from `dotnet-trace --profile gc-verbose`
allocation ticks with stacks, aggregated by type and allocating method.

### Changes and their effect

Each step was measured with the exact metric at 64 connections:

| Step | Bytes/request |
| --- | ---: |
| Start of pass (header and wrapper changes above) | 5,655 |
| Wait for the request head in `requestLoop` | 4,235 |
| Null-returning lookups for absent headers | 3,654 |
| Synchronous parse of a buffered request line and headers | 2,747 |
| Reusable TCP receive source; loop awaits receive and commit | 2,297 |
| Response writer, `Content-Length` formatting, path decode, route lookup | 1,930 |
| No request-line tuple on the fast path | 1,882 |
| Reusable WebPart completion instead of `StartImmediateAsTask` | 1,714 |

* **Waiting at the top.** A keep-alive request used to suspend inside the parser,
  through `processRequest → readRequest → readRequestLine → readUntilPattern →
  scanMarkerAsync → readMoreData → transport.read`, allocating a state machine
  box at every level. A .NET async method allocates its box once per invocation
  and reuses it across that invocation's suspensions. So `requestLoop`, which
  lives as long as the connection, now waits until `HttpReader.requestHeadReady`
  sees a complete head. Everything below then completes synchronously.
  `requestHeadReady` also releases on heads larger than the 8 KiB line buffer, on
  a complete first line that is not HTTP/1.x (the HTTP/2 prior-knowledge preface
  and garbage are not followed by an empty line), and on canceled or completed
  input. All of these continue on the unchanged streaming parser.
* **Synchronous head parse.** `tryReadRequestLineBuffered` and
  `tryReadHeadersBuffered` copy the buffered line or header block into the line
  buffer and parse it without tasks, per-line closures or ref cells.
  `readRequest` builds the request synchronously unless it has a
  `Content-Length` or `Expect` header, an invalid version or the HTTP/2 preface;
  those cases resume in `readRequestAsync`, the previous streaming code.
* **Receive without a box.** `TcpTransport` completes a pending
  `Socket.ReceiveAsync` through a reusable `IValueTaskSource`. `HttpReader.receive`
  and `commitReceived` split `readMoreData` so that the loop awaits them directly.
* **WebPart bridge.** `HttpOutput.run` starts the WebPart with
  `Async.StartWithContinuations` and cached continuations completing a reusable
  `IValueTaskSource`. It keeps `StartImmediateAsTask`'s semantics: immediate start,
  `Async.DefaultCancellationToken`, and exceptions surfacing at the await. An
  overlapping start falls back to `StartImmediateAsTask`.
* **Smaller items.** Absent-header lookups on the request path no longer build
  "Key … was not present" strings. `Connection.tryAppendInt` no longer allocates a
  `char[]`, and `writeContent` no longer returns a closure per response. Header
  exclusion lists are static, and `HttpRequest.path` skips `UrlDecode` without
  `%` or `+`. `Router` does one exact-route lookup instead of two; deferring the
  handler inside `async` keeps synchronous handler exceptions on the error path.

The remaining ≈1.7 KB is mostly inherent to the public API: header strings and
tuples for `List<string*string>`, and the `HttpContext option`, `HttpStatus` and
`Bytes` that `OK ""` builds. FSharp.Core's Async machinery (trampoline,
activation records, `TryWith`) accounts for most of the rest, plus the `Task`
objects that `processRequest` and `run` return.

**Cancellation registrations.** CPU sampling showed contention in
`CancellationTokenSource` registration: every pending receive registered on the
server-wide token. TCP receives no longer pass that token. Instead,
`ConnectionFacade.accept` registers once per connection to shut the connection
down when the server stops, which ends a pending receive. The test suite's
runtime (≈22 s), which stops many servers, did not change. In a balanced A/B
against the build just before this change, the 64-connection median rose from
180,724 to 188,174 requests/second. At 256 connections, all three servers tied
at ≈198,000, so the shared host was the limit there.

Contention events (`Microsoft-Windows-DotNETRuntime:0x4000`) showed ≈157 real
contentions per second, all inside the BCL socket engine. `Monitor.Enter_Slowpath`
frames under `Pipe` in thread-time samples are not contention.

### Results

These are balanced trials as above: six rounds of 5 seconds per server and
concurrency, after 10-second warmups, on a quiet machine. The baseline is the
build from the start of this pass.

| Connections | Baseline | Suave | Minimal API |
| --- | ---: | ---: | ---: |
| 64 | 174,206 | 192,398 | 189,144 |
| 256 | 169,488 | 190,905 | 186,743 |

Suave beat the baseline in all 12 rounds and Minimal API in 11 of 12 (the
exception: 256 connections, round 6). These are same-host loopback measurements
on macOS, so they are not a Linux leaderboard result. Connection-close smoke trials
(3 × 3,000 requests at 64 connections) all succeeded, with Suave at or above both
other servers.

Raw results: `profile-output/comparison/session2-final`, `session2-cts`,
`session2-c64-c256` and `session2-close-smoke`.

New regression tests (`request head parsing` in `src/Suave.Tests/Parsing.fs`)
cover the fast path and each handover to the streaming parser over a raw
keep-alive connection. They cover pipelined requests, a head split across reads,
header trimming and case, a head larger than the line buffer, a body followed by
a pipelined request, and a malformed header. All 415 tests pass.

### Test-run hangs

Two full runs hung during this pass. Thread stacks showed a console lock
inversion between Expecto's redirected `Console.Out` and .NET's Unix
`ConsolePal`, reached when the server writes "Stopping TCP server …" while
Expecto flushes. It is not a Suave networking fault. Rerunning passes. This is
probably the websocket hang described in `AGENTS.md`.

## Follow-up: Synchronous Completion (2026-10-04)

A Linux run (same-host loopback, `oha`, six balanced rounds) left Suave about 8-10%
behind Minimal API at 64 connections, and level at 256. A larger gen0 budget
(`DOTNET_GCgen0size=0x10000000`) roughly halved that gap and brought Suave's p99
latency to Minimal API's. On Linux, at ≈600k requests/second, the remaining
allocations cost measurable GC time.

This follow-up removes more per-request allocations without changing public APIs.
Each step was measured with the exact probe at 64 connections:

| Step | Bytes/request |
| --- | ---: |
| Start (keep-alive pipeline PR) | 1,714 |
| `processRequestValue`/`runValue` complete synchronously; error recovery inside `AsyncCompletion` | 1,330 |
| Status line and ASCII headers written synchronously; no `ToLowerInvariant` per header | 1,242 |

`AsyncCompletion` takes an optional `recover` function that reproduces
`async.TryWith(workflow, recover)`. A failure after cancellation was requested
becomes cancellation, and failures of the recovery are not recovered. New tests
check this directly (`AsyncCompletion recovery`), and a raw-socket test checks
response header casing. `SslTransport` reads use the same reusable completion
source as TCP. A same-host Mac A/B against the PR build showed no regression
(193k vs 191k requests/second at 64 connections), but the Mac is already near
its loopback ceiling, so CPU savings need Linux to show.

The Linux CPU trace also showed ≈15% of request-processing time copying the
`HttpContext` struct (≈136 bytes of references, including `HttpRequest` and
`HttpResult` by value) with GC write barriers. The `experiment/httpcontext-class`
branch makes `HttpContext` and `HttpRequest` classes. In the socket-free probe, a
routed WebPart through the error wrapper then takes 192 ns instead of 262 ns.
Making `HttpResult` a class as well was slightly slower. That change is
binary-breaking, so it is kept separate for a major version.
