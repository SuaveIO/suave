#!/usr/bin/env bash
# Exact server-side allocated bytes per request: reads GC.GetTotalAllocatedBytes(true)
# from the side port (PORT + 100) of a server started with SUAVE_ALLOC_PROBE=1 before and after a fixed request count.
# Usage: bash benchmarks/alloc-per-request.sh [PORT] [REQUESTS] [CONCURRENCY]
set -euo pipefail
port=${1:-3000}
requests=${2:-200000}
concurrency=${3:-64}
url="http://127.0.0.1:$port"
tmp=$(mktemp)
oha --no-tui --http-version 1.1 -n 50000 -c "$concurrency" "$url/" > /dev/null
before=$(curl --fail --silent "http://127.0.0.1:$((port + 100))/")
oha --no-tui --http-version 1.1 -n "$requests" -c "$concurrency" --output-format json "$url/" > "$tmp"
after=$(curl --fail --silent "http://127.0.0.1:$((port + 100))/")
jq -e '.summary.successRate == 1' "$tmp" > /dev/null
rps=$(jq '.summary.requestsPerSec|round' "$tmp")
rm -f "$tmp"
awk -v b="$before" -v a="$after" -v n="$requests" -v r="$rps" -v c="$concurrency" \
  'BEGIN { printf "c=%d requests=%d bytes/request=%.1f rps=%d\n", c, n, (a-b)/n, r }'
