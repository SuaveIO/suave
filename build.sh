#!/usr/bin/env bash

FOLDER=./packages/NuGet.CommandLine/tools
mkdir -p $FOLDER

NUGET=$FOLDER/NuGet.exe

if [ ! -f $NUGET ]; then
  curl -L -o $NUGET https://www.nuget.org/nuget.exe
  chmod a+x $NUGET
fi

echo 'Restoring nugets'
mono tools/paket.bootstrapper.exe
mono tools/paket.exe restore

echo 'Building'
xbuild suave.sln

echo 'Running tests'
mono Tests/bin/Debug/Tests.exe
