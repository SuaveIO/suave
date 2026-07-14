# AGENTS.md

## Cursor Cloud specific instructions

Suave is an F# web server **library** (a small monorepo of NuGet packages: `Suave`, `Suave.Json`, `Suave.DotLiquid`, tests, examples, and a docs `website`). It builds and tests entirely in-process — there is **no database or external service** to run.

### Toolchain (already installed in the VM snapshot)
- .NET SDK `10.0.102` (pinned by `global.json`) plus the .NET 6 runtime (needed by the FAKE build project, which targets `net6.0`) live in `~/.dotnet`, symlinked at `/usr/local/bin/dotnet`.
- Dependencies use **Paket**, not plain `PackageReference`. The startup update script runs `dotnet tool restore` (restores the `paket` + `fake-cli` local tools from `.config/dotnet-tools.json`) followed by `dotnet paket restore`. Always ensure a Paket restore has run before building.

### Build / test / run
- Build the solution: `dotnet build Suave.sln` (or the FAKE target `dotnet run --project ./build/build.fsproj -- -t Build`). Expect ~46 warnings, 0 errors.
- Run the test suite (same command CI uses): `dotnet run -c Release --framework net10.0 --project src/Suave.Tests -- --summary --sequenced`. Tests self-host Suave on loopback ports.
- Run an example server (each defaults to HTTP `127.0.0.1:8080`; the `website` binds `0.0.0.0:8080` — run only one at a time): e.g. `dotnet run --project examples/RouterExample`, then hit `http://127.0.0.1:8080/`.

### Non-obvious caveats
- **Do not `source .env` / run `build.sh` as-is.** `.env` sets a Mono-based `FrameworkPathOverride` (`dirname $(which mono)/...`); Mono is not installed, so this yields a bogus path. It is unnecessary for the .NET SDK build — run the underlying `dotnet` commands directly instead.
- **The websocket test list can flaky-hang in this VM.** With `--sequenced`, the large 32-bit (66000-byte) binary-payload test under `/websocketAppSubprotocolUrl` occasionally blocks forever (the test uses `mre.WaitOne()` with no timeout, so a single missed frame over loopback hangs the whole run). The identical test passes for the other websocket URLs, and CI runs the full suite green in ~2m23s — so treat this as a flaky test in the VM, **not** an environment breakage. For a deterministic pass, filter to a list, e.g. `--filter-test-list miscellaneous` (364 tests), or be prepared to kill and retry the full run.
- No linter/formatter is configured; code style is only enforced by `.editorconfig` (2-space indent).
