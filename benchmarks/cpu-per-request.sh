#!/usr/bin/env bash
# Server CPU time per request, independent of how fast the load generator is:
# total user+system CPU the server process consumed during a fixed number of
# keep-alive requests, divided by that number. A load generator sharing the host
# limits throughput comparisons but not this measure.
# Usage: bash benchmarks/cpu-per-request.sh PORT [REQUESTS] [CONCURRENCY]
set -euo pipefail
port=${1:?Usage: bash benchmarks/cpu-per-request.sh PORT [REQUESTS] [CONCURRENCY]}
requests=${2:-400000}
concurrency=${3:-64}
url="http://127.0.0.1:$port/"
if command -v lsof > /dev/null; then
  pid=$(lsof -t -iTCP:"$port" -sTCP:LISTEN | head -1)
else
  pid=$(ss -ltnpH "sport = :$port" | grep -o 'pid=[0-9]*' | head -1 | cut -d= -f2)
fi
# Cumulative user+system CPU seconds of the process. Linux: /proc in clock ticks
# (ps there only has whole seconds). macOS: ps prints [[dd-]hh:]mm:ss.cc.
cpu_seconds() {
  if [[ -r "/proc/$pid/stat" ]]; then
    awk -v hz="$(getconf CLK_TCK)" '{ sub(/.*\) /, ""); print ($12 + $13) / hz }' "/proc/$pid/stat"
  else
    ps -o time= -p "$pid" | awk -F'[:-]' '{ s = 0; for (i = 1; i <= NF; i++) s = s * 60 + $i; print s }'
  fi
}
oha --no-tui --http-version 1.1 -n 100000 -c "$concurrency" "$url" > /dev/null
before=$(cpu_seconds)
oha --no-tui --http-version 1.1 -n "$requests" -c "$concurrency" "$url" > /dev/null
after=$(cpu_seconds)
awk -v b="$before" -v a="$after" -v n="$requests" -v p="$port" -v c="$concurrency" \
  'BEGIN { printf "port=%s c=%d requests=%d cpu_us/request=%.2f\n", p, c, n, (a - b) * 1e6 / n }'
