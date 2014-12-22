#!/usr/bin/env bash

echo 'Restoring nugets'
mono tools/paket.bootstrapper.exe
mono tools/paket.exe restore

echo 'Building'
xbuild suave.sln

echo 'Running tests'
mono Tests/bin/Debug/Tests.exe
