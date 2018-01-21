@echo off
.paket\paket.exe restore
packages\build\FAKE\tools\FAKE.exe build.fsx %*