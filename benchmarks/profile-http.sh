#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
process_id=${1:?Usage: bash benchmarks/profile-http.sh PROCESS_ID OUTPUT_PREFIX [PORT]}
output=${2:?Output prefix required}
port=${3:-3000}
mkdir -p "$(dirname "$output")"
rate=()
if [[ -n "${RATE:-}" ]]; then rate=(-q "$RATE"); fi
oha --no-tui --http-version 1.1 -w -t 5s -z 25s -c 64 \
  ${rate[@]+"${rate[@]}"} \
  --output-format json --output "$output-load.json" "http://127.0.0.1:$port/" </dev/null &
load_pid=$!
trap 'kill "$load_pid" 2>/dev/null || true' EXIT
if [[ "${COUNTERS:-0}" == 1 ]]; then
  dotnet-counters collect --process-id "$process_id" \
    --counters 'System.Runtime[alloc-rate,gc-heap-size,gen-0-gc-count,cpu-usage]' \
    --duration 00:00:20 --format json --output "$output-counters.json" </dev/null
else
  dotnet-trace collect --process-id "$process_id" \
    --profile dotnet-sampled-thread-time,gc-verbose --duration 00:00:20 \
    --format Speedscope --output "$output.nettrace" </dev/null
fi
wait "$load_pid"
trap - EXIT
if [[ "${COUNTERS:-0}" != 1 ]]; then
  dotnet-trace report "$output.nettrace" topN -n 30
fi