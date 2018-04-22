#!/bin/bash
source .env
dotnet restore build.proj
dotnet fake build $@
