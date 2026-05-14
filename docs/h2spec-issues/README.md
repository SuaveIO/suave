# h2spec conformance issues

These markdown files are issue-ready drafts derived from the h2spec run on
PR #814 ([job 75904815837](https://github.com/SuaveIO/suave/actions/runs/25833863507/job/75904815837),
commit `2a35bda`), which reported **32 failing conformance tests** out of 146.

Each file can be filed as a standalone GitHub issue, e.g.:

```sh
gh issue create \
  --title "$(head -n1 01-hpack-decoder.md | sed 's/^# //')" \
  --body-file 01-hpack-decoder.md \
  --label bug --label http2
```

| # | File | Scope | Failing tests |
|---|------|-------|---------------|
| 1 | [`01-hpack-decoder.md`](01-hpack-decoder.md) | HPACK decoder conformance | 2 |
| 2 | [`02-frame-validation.md`](02-frame-validation.md) | Frame-level validation (size, unknown frames, PRIORITY/RST_STREAM/SETTINGS/PING/preface) | 14 |
| 3 | [`03-stream-state-machine.md`](03-stream-state-machine.md) | Stream state machine + dependency self-loop checks | 6 |
| 4 | [`04-flow-control-and-http-semantics.md`](04-flow-control-and-http-semantics.md) | Per-stream flow control + HTTP request validation | 10 |

After all four are fixed, the workflow command `h2spec generic http2 hpack`
should report `146 tests, 145 passed, 1 skipped, 0 failed`.
