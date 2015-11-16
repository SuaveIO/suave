@echo off

call %~dp0deploy.bat win7-x86         release %~dp0..\..\src\Suave
call %~dp0deploy.bat win7-x64         release %~dp0..\..\src\Suave
call %~dp0deploy.bat ubuntu.14.04-x64 release %~dp0..\..\src\Suave
call %~dp0deploy.bat osx.10.10-x64    release %~dp0..\..\src\Suave
