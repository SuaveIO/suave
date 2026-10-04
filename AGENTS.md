# AGENTS.md

Guidance for AI coding agents working in this repository. Keep changes small,
respect the existing conventions, and verify with the commands below before
opening a PR.

## Repository overview

Suave is an F# web server **library**, structured as a small monorepo of
NuGet packages plus tests, examples, and a documentation website.

- `src/Suave` — the core web server library.
- `src/Suave.Json` — JSON helpers.
- `src/Suave.DotLiquid` — DotLiquid view engine integration.
- `src/Suave.Tests` — Expecto-based test suite; tests self-host Suave on
  loopback ports (no external services required).
- `examples/` — small runnable example servers (`RouterExample`,
  `WebSocket`, `CORS`, `Pong`, `RateLimit`, `Stream`, etc.). Each defaults
  to HTTP on `127.0.0.1:8080`, so run only one at a time.
- `benchmarks/` — performance benchmarks (see `BENCHMARKS_GUIDE.md`).
- `website/` — the docs site (`0.0.0.0:8080`). Guides live under
  `website/content/docs/`; API reference is generated into
  `website/content/reference/`.
- `docs-api/` — fsdocs input used to generate the API reference.
- `build/` — FAKE build script (`build/build.fsproj`, targets `net10.0`).
- `scripts/` — helper scripts, notably `generate-api-docs.sh`.

## Toolchain

- `global.json` requires .NET SDK **`10.0.100` or newer** in the `10.0`
  band (`rollForward: latestFeature`), so any recent 10.0.x SDK works. No
  other runtime is needed — the FAKE build project targets `net10.0`.
- Dependencies are managed with **Paket**, not plain `PackageReference`.
  Local tools (`paket`, `fake-cli`, `fsdocs-tool`) are declared in
  `.config/dotnet-tools.json`.
- Before the first build in a fresh checkout, run:
  ```
  dotnet tool restore
  dotnet paket restore
  ```

## Build, test, run

- **Build the solution:**
  `dotnet build Suave.sln`
  (equivalent FAKE target: `./build.sh -t Build`). `build.sh` / `build.cmd`
  restore tools and packages, then pass their arguments straight to FAKE;
  with no arguments the default target is `Tests`.
  Expect a number of warnings but 0 errors.
- **Run the test suite (matches CI):**
  `dotnet run -c Release --framework net10.0 --project src/Suave.Tests -- --summary --sequenced`
- **Run an example server:**
  `dotnet run --project examples/RouterExample`, then hit
  `http://127.0.0.1:8080/`.
- **Preview the docs site:**
  `dotnet run --project website`. Generate the API reference first with
  `./scripts/generate-api-docs.sh` (or FAKE `-t Docs`) so `/reference/` is
  populated.

CI (`.github/workflows/build-suave.yml`) runs `./build.sh` on
`ubuntu-latest` with the .NET 10 SDK, then regenerates the API docs.

## Releasing

`.semver` is the single source of truth for the version. A release is cut by
pushing a `vX.Y.Z` tag, which triggers `.github/workflows/release.yml`:

1. Bump `.semver` and add a matching `## New in vX.Y.Z` section to
   `RELEASE_NOTES.md` (the `## Unreleased` section is skipped by the build).
2. Commit, then `./build.sh -t Tag` — it refuses to run
   unless the working copy is clean and `.semver` matches `RELEASE_NOTES.md`,
   then pushes the branch and the tag. `git tag vX.Y.Z && git push origin
   vX.Y.Z` does the same thing by hand.
3. The workflow re-checks that the tag matches `.semver` (FAKE
   `CheckVersion`), builds, tests, packs, publishes to nuget.org via Trusted
   Publishing (no API key secret — see the header of `release.yml`), and
   creates the GitHub Release.

The FAKE `Push` target reads `NUGET_API_KEY` (issued by the Trusted
Publishing login) or, for a manual push, the legacy `NUGET_KEY`.

## Code style

- No linter or formatter is configured. Style is enforced only by
  `.editorconfig`:
  - F# files (`*.fs`, `*.fsi`, `*.fsx`): 2-space indent, UTF-8, trim
    trailing whitespace, **no** final newline.
- Match the surrounding code — naming, module layout, and idioms — rather
  than introducing new conventions.

## Conventions for changes

- Prefer minimal, surgical changes that address the task; do not reformat
  or refactor unrelated code.
- Do not add new lint/format/test tooling unless the task requires it.
- Public API changes in `src/Suave*` are user-visible: keep them backward
  compatible where possible and update `RELEASE_NOTES.md` when appropriate.
- When editing anything under `src/`, run the test suite before finishing.
- When updating dependencies, edit `paket.dependencies` and re-run
  `dotnet paket install` / `restore` so `paket.lock` stays consistent.
  Do not hand-edit `paket.lock`.

## Non-obvious caveats

- **`.env` is dead weight.** It sets a Mono-based `FrameworkPathOverride`
  that was only needed while the build project targeted an old framework.
  Nothing sources it any more; do not reintroduce it.
- **Server threads must not write to `Console` during tests.** Expecto
  redirects `Console.Out` to a synchronized writer and holds its own lock
  while flushing to the real console; on Unix, .NET's console stream then
  locks `Console.Out`. A Suave server thread calling `Console.WriteLine`
  takes the two locks in the opposite order, which deadlocks the run. It
  used to show up as an intermittent hang after websocket or rate-limit
  tests, where a test then waits forever on its `ManualResetEvent`. Suave's
  own messages (startup banner, "Stopping TCP server", websocket
  disconnects) go through `Globals.writeMessage`, which
  `src/Suave.Tests/Program.fs` points at `TextWriter.Null`. Route any new
  unconditional server-side output the same way, and keep `printfn` out of
  test web parts. If a run still hangs, sample it with
  `dotnet-trace collect -p PID --profile dotnet-sampled-thread-time`.
- Examples and the `website` all default to port `8080`; only run one at a
  time or change the port.

## Further reading

- `README.md` — user-facing overview and quick start.
- `CONTRIBUTING.md` — contribution guidelines.
- `RELEASE_NOTES.md` — versioned change history.
- `BENCHMARKS_GUIDE.md` — how to run and interpret the benchmarks.
