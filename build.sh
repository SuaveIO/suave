#!/usr/bin/env bash
# Entry point for the FAKE build. Pass FAKE arguments straight through, e.g.
#   ./build.sh -t Build      ./build.sh -t Pack      ./build.sh --single-target -t Docs
# With no arguments the default target (Tests) runs.
set -euo pipefail
dotnet tool restore
dotnet paket restore
dotnet run --project ./build/build.fsproj -- "$@"
