# HTTP/2: missing flow control (§6.9) and HTTP semantics validation (§8.1.2) — 10 h2spec failures

**Labels (suggested):** `bug`, `http2`, `conformance`

The h2spec run on PR #814 ([job 75904815837](https://github.com/SuaveIO/suave/actions/runs/25833863507/job/75904815837), commit `2a35bda`) reports 10 failures that need two larger pieces of work:

1. real per-stream / per-connection flow control,
2. validation of incoming HTTP headers and request bodies before invoking the application.

These are the largest of the four h2spec work items and may be best split into two PRs.

## Failing tests

**Flow control (RFC 7540 §6.5.2 / §6.5.3 / §6.9)**
- `http2/6.5.2/2` `SETTINGS_INITIAL_WINDOW_SIZE` > 2^31-1 → expected `GOAWAY(FLOW_CONTROL_ERROR)`; server ACKs.
- `http2/6.5.3/1` Multiple values of `SETTINGS_INITIAL_WINDOW_SIZE` → expected chunked `DATA(len=1)`; got full-size DATA.
- `http2/6.9.1/1` `SETTINGS_INITIAL_WINDOW_SIZE = 1`, then HEADERS → expected `DATA(len=1)`; got full-size DATA.
- `http2/6.9.2/1` Changing `SETTINGS_INITIAL_WINDOW_SIZE` after HEADERS → expected `DATA(len=1)`; got full-size DATA.
- `http2/6.9.2/2` SETTINGS makes window negative → expected `DATA(len=1)` after WINDOW_UPDATE; got Timeout.
- `http2/6.9.2/3` `SETTINGS_INITIAL_WINDOW_SIZE` value above 2^31-1 → expected `GOAWAY(FLOW_CONTROL_ERROR)`; server ACKs.

**HTTP semantics (RFC 7540 §8.1.2)**
- `http2/8.1.2/1` HEADERS with uppercase header field name → expected stream `PROTOCOL_ERROR`; got DATA response.
- `http2/8.1.2.3/1` HEADERS with empty `:path` → expected stream `PROTOCOL_ERROR`; got DATA response.
- `http2/8.1.2.6/1` `content-length` mismatch (single DATA) → expected stream `PROTOCOL_ERROR`; got DATA response.
- `http2/8.1.2.6/2` `content-length` mismatch (multiple DATA) → expected stream `PROTOCOL_ERROR`; got DATA response.

## Required changes

### Flow control (`src/Suave/Http2.fs`)

- Track a `connectionWindow` and a per-stream `streamWindow` (both `int32`, can go negative on settings changes).
- When sending DATA, chunk into pieces no larger than `min(connectionWindow, streamWindow, SETTINGS_MAX_FRAME_SIZE)` and decrement both windows; block when either is ≤ 0 until a matching `WINDOW_UPDATE` is received.
- On `SETTINGS_INITIAL_WINDOW_SIZE` change, adjust every existing stream window by the delta (`new - old`); negative results are valid.
- Validate the value at parse time: anything > `2^31-1` is `GOAWAY(FLOW_CONTROL_ERROR)`.

### Request validation (header reassembly path)

- Reject any header name containing uppercase ASCII (stream `PROTOCOL_ERROR`).
- Reject empty `:path` for non-CONNECT methods (stream `PROTOCOL_ERROR`).
- If `content-length` is present, count actual DATA payload bytes received for the stream and emit stream `PROTOCOL_ERROR` if the totals don't match at end-of-stream.

## Acceptance

The 10 tests above pass; combined with the earlier issues the h2spec pass rate should reach 145/146 (1 skipped, 0 failed) for `h2spec generic http2 hpack`.
