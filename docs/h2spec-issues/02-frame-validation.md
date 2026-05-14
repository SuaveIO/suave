# HTTP/2: frame parser does not enforce protocol-level validation (14 h2spec failures)

**Labels (suggested):** `bug`, `http2`, `conformance`

The h2spec run on PR #814 ([job 75904815837](https://github.com/SuaveIO/suave/actions/runs/25833863507/job/75904815837), commit `2a35bda`) reports 14 failures that all share the same root cause: the frame reader/dispatcher in `src/Suave/Http2.fs` does not validate fields before either ACKing or invoking the handler. In most cases the server happily serves the response (`DATA Frame (length:11, …)`) when it should be terminating the connection.

## Failing tests (grouped by RFC 7540 section)

**§3.5 — Connection preface**
- `http2/3.5/2` Invalid connection preface → expected `GOAWAY(PROTOCOL_ERROR)` then FIN; got bare EOF.

**§4.1 / §5.5 — Unknown frame types (must be ignored)**
- `http2/4.1/1` Unknown frame type → expected PING ACK to follow; got Connection closed.
- `http2/5.5/1` Unknown extension frame → same.

**§4.2 — Frame size**
- `http2/4.2/2` Oversized DATA frame → expected `GOAWAY/RST_STREAM(FRAME_SIZE_ERROR)`; got normal DATA response.
- `http2/4.2/3` Oversized HEADERS frame → same.

**§6.3 — PRIORITY**
- `http2/6.3/1` PRIORITY with stream id 0x0 → expected `GOAWAY(PROTOCOL_ERROR)`; got Timeout.
- `http2/6.3/2` PRIORITY with length != 5 → expected `GOAWAY/RST_STREAM(FRAME_SIZE_ERROR)`; got normal DATA response.

**§6.4 — RST_STREAM**
- `http2/6.4/1` RST_STREAM with stream id 0x0 → expected `GOAWAY(PROTOCOL_ERROR)`; got Timeout.
- `http2/6.4/2` RST_STREAM on idle stream → expected `GOAWAY(PROTOCOL_ERROR)`; got Timeout.

**§6.5 / §6.5.2 — SETTINGS validation**
- `http2/6.5/2` SETTINGS with stream id != 0 → server still ACKs.
- `http2/6.5.2/1` `SETTINGS_ENABLE_PUSH` value other than 0/1 → server still ACKs.
- `http2/6.5.2/3` `SETTINGS_MAX_FRAME_SIZE` < 2^14 → server still ACKs.
- `http2/6.5.2/4` `SETTINGS_MAX_FRAME_SIZE` > 2^24-1 → server still ACKs.

**§6.7 — PING**
- `http2/6.7/4` PING with length != 8 → server tears socket down with `FRAME_SIZE_ERROR` but does not write a `GOAWAY` frame first, so h2spec only sees an abrupt close.

## Required changes

In the frame reader / dispatcher (`src/Suave/Http2.fs`):

1. Add a generic "before processing" validation step:
   - reject frames whose length > current `SETTINGS_MAX_FRAME_SIZE` with connection error `FRAME_SIZE_ERROR`;
   - silently consume and discard payloads for unknown frame types;
   - validate frame-specific invariants (stream id == 0 / != 0, fixed payload sizes) per the table in RFC 7540 §6.
2. For the connection preface: after reading the 24 bytes, if they don't match the magic string, write a proper `GOAWAY(PROTOCOL_ERROR)` before closing.
3. For SETTINGS: validate each entry *before* sending the ACK; raise `GOAWAY(PROTOCOL_ERROR)` or `GOAWAY(FLOW_CONTROL_ERROR)` per §6.5.2.
4. Centralise GOAWAY emission so error paths consistently write the frame before closing the TCP socket.

## Acceptance

The 14 tests listed above pass; the remaining h2spec failures (state machine, flow control, HTTP semantics) are tracked in separate issues.
