#!/usr/bin/env bash

set -eu
set -o pipefail

cd `dirname $0`
OS=${OS:-"unknown"}

function run() {
  if [[ "$OS" != "Windows_NT" ]]
  then
    mono "$@"
  else
    "$@"
  fi
}

[[ -f tools/paket.exe ]] || run tools/paket.bootstrapper.exe

if [[ "$OS" != "Windows_NT" ]] && [ ! -e ~/.config/.mono/certs ]
then
  mozroots --import --sync --quiet
fi

echo 'Update nugets'
run tools/paket.exe update 

echo 'Restoring nugets'
run tools/paket.exe restore

echo 'Building'
xbuild src/Suave.sln /p:Configuration=Release

echo 'Running tests'
mono src/Suave.Tests/bin/Release/Suave.Tests.exe --sequenced
