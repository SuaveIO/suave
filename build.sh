#!/bin/bash
source .env
dotnet tool install paket
dotnet tool restore
dotnet paket restore
dotnet restore build.proj
dotnet tool install fake-cli
dotnet fake build $@
