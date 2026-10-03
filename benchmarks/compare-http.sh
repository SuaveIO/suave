#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

output=${1:?Usage: bash benchmarks/compare-http.sh OUTPUT_DIRECTORY}
duration=${DURATION:-10s}
warmup=${WARMUP:-10s}
repetitions=${REPETITIONS:-3}
requests=${REQUESTS:-}
concurrencies=${CONCURRENCIES:-"64 256 512"}
modes=${MODES:-"keepalive close"}
suave_port=${SUAVE_PORT:-3000}
baseline_port=${BASELINE_PORT:-}
server_ports="$suave_port 3001 $baseline_port"
if [[ -e "$output/environment.json" ]]; then
  printf 'Refusing to overwrite results in %s\n' "$output" >&2
  exit 1
fi
mkdir -p "$output"
read -r -a servers <<< "$server_ports"
server_count=${#servers[@]}
limit=(-z "$duration")
if [[ -n "$requests" ]]; then limit=(-n "$requests"); fi

for port in $server_ports; do
  test "$(curl --fail --silent "http://127.0.0.1:$port/user/42")" = 42
  test "$(curl --silent --output /dev/null --write-out '%{http_code}' -X POST "http://127.0.0.1:$port/user")" = 200
done

jq -n --arg revision "$(git rev-parse HEAD)" --arg runtime "$(dotnet --version)" \
  --arg os "$(uname -a)" --arg generator "$(oha --version)" \
  --arg duration "$duration" --arg warmup "$warmup" --arg concurrencies "$concurrencies" \
  --arg modes "$modes" --arg repetitions "$repetitions" \
  --arg requests "$requests" --arg ports "$server_ports" \
  '{revision:$revision,runtime:$runtime,os:$os,generator:$generator,duration:$duration,warmup:$warmup,concurrencies:$concurrencies,modes:$modes,repetitions:$repetitions,requests:$requests,ports:$ports}' \
  > "$output/environment.json"
git diff > "$output/source.diff"

for port in $server_ports; do
  oha --no-tui --http-version 1.1 -w -t 5s -z "$warmup" -c 64 \
    --output-format json "http://127.0.0.1:$port/" > "$output/warmup-$port.json"
done

for mode in $modes; do
  extra=()
  if [[ "$mode" == close ]]; then extra+=(--disable-keepalive); fi
  for concurrency in $concurrencies; do
    for (( repetition=1; repetition<=repetitions; repetition++ )); do
      for (( position=0; position<server_count; position++ )); do
        direction=1
        if (( repetition % 2 == 0 )); then direction=-1; fi
        index=$(( (repetition - 1 + direction * position + server_count) % server_count ))
        port=${servers[$index]}
        name=suave
        if [[ "$port" == 3001 ]]; then name=minimal; fi
        if [[ "$port" == "$baseline_port" ]]; then name=baseline; fi
        result="$output/$name-$mode-c$concurrency-r$repetition.json"
        oha --no-tui --http-version 1.1 -w -t 5s "${limit[@]}" -c "$concurrency" \
          ${extra[@]+"${extra[@]}"} --output-format json "http://127.0.0.1:$port/" > "$result"
        if ! jq -e '.summary.successRate == 1 and (.errorDistribution | length) == 0 and (.statusCodeDistribution | keys) == ["200"]' "$result" > /dev/null; then
          printf 'Invalid trial: %s\n' "$result" >&2
          jq '{statusCodeDistribution,errorDistribution}' "$result" >&2
          exit 1
        fi
        jq -r --arg name "$name" --arg mode "$mode" --arg concurrency "$concurrency" --arg repetition "$repetition" \
          '[$name,$mode,$concurrency,$repetition,(.summary.requestsPerSec|round),(.latencyPercentiles.p99*1000)] | @tsv' "$result"
      done
    done
  done
done