@echo off
dotnet tool restore || exit /b 1
dotnet paket restore || exit /b 1
dotnet run --project ./build/build.fsproj -- %* || exit /b 1
