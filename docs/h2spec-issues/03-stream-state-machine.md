# HTTP/2: stream state machine missing — 6 h2spec failures (§5.1, §5.3.1)

**Labels (suggested):** `bug`, `http2`, `conformance`

The h2spec run on PR #814 ([job 75904815837](https://github.com/SuaveIO/suave/actions/runs/25833863507/job/75904815837), commit `2a35bda`) reports 6 failures that stem from `src/Suave/Http2.fs` not maintaining proper per-stream state and not validating priority dependencies.

## Failing tests

**§5.1 — Stream States**
- `http2/5.1/1` idle: DATA frame → expected `GOAWAY(PROTOCOL_ERROR)`; got `RST_STREAM`.
- `http2/5.1/2` idle: RST_STREAM frame → expected `GOAWAY(PROTOCOL_ERROR)`; got Timeout.
- `http2/5.1/3` idle: WINDOW_UPDATE frame → expected `GOAWAY(PROTOCOL_ERROR)`; got Timeout.
- `http2/5.1/12` closed: HEADERS frame → expected `GOAWAY(STREAM_CLOSED)`; got `RST_STREAM`.

**§5.3.1 — Stream Dependencies**
- `http2/5.3.1/1` HEADERS frame depending on itself → expected stream `PROTOCOL_ERROR`; got normal DATA response.
- `http2/5.3.1/2` PRIORITY frame depending on itself → expected stream `PROTOCOL_ERROR`; got Timeout.

## Required changes

1. Introduce an explicit `StreamState` type (`Idle | ReservedLocal | ReservedRemote | Open | HalfClosedRemote | HalfClosedLocal | Closed`) per stream id and apply the transition table from RFC 7540 §5.1.
2. Receiving DATA / RST_STREAM / WINDOW_UPDATE / PUSH_PROMISE on an `Idle` stream is a connection-level `PROTOCOL_ERROR` (GOAWAY). Receiving HEADERS on a `Closed` stream is `STREAM_CLOSED`.
3. In the HEADERS and PRIORITY parsers, if the encoded `stream dependency` equals the frame's `stream id`, emit a stream error `PROTOCOL_ERROR` (RST_STREAM) and abort header processing for that stream.

## Acceptance

All 6 tests above pass.
