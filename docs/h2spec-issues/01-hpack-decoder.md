# HTTP/2: HPACK decoder fails 2 h2spec conformance tests

**Labels (suggested):** `bug`, `http2`, `hpack`

The h2spec run on PR #814 ([job 75904815837](https://github.com/SuaveIO/suave/actions/runs/25833863507/job/75904815837), commit `2a35bda`) reports 32 conformance failures. Two of them are isolated to `Suave.Hpack` and can be fixed independently of everything else.

## Failing tests

| Test | Expected | Actual |
|------|----------|--------|
| `generic/5/15` Sends multiple dynamic table size update | HEADERS response | Connection closed |
| `hpack/2.3.3/1` Indexed header field with invalid index | `GOAWAY(COMPRESSION_ERROR)` | `DATA Frame (length:11, …)` (request served) |

## Required changes (in `src/Suave/Hpack.fs`)

1. **Allow up to two consecutive dynamic table size updates at the start of a header block** (RFC 7541 §4.2). The decoder currently treats the second `0x20`-prefixed octet as an error and the connection is torn down.
2. **Raise a decoding error on out-of-range indexed header references** (RFC 7541 §2.3.3). When the index exceeds `static + current dynamic table size`, surface a failure that the dispatch loop translates into a connection-level `GOAWAY(COMPRESSION_ERROR)` rather than silently returning an empty header and serving the request.

## Acceptance

- `h2spec generic/5/15` and `h2spec hpack/2.3.3/1` pass.
- Existing Suave HPACK unit tests still pass:
  ```sh
  dotnet run --project src/Suave.Tests/Suave.Tests.fsproj -c Release --framework net10.0 -- --summary
  ```
