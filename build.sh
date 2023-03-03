#!/bin/bash
source .env
dotnet tool restore
dotnet paket restore
dotnet run --project ./build/build.fsproj -- -t Tests
