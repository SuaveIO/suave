@echo off

call %~dp0deploy.bat win7-x86 debug %~dp0..\..\src\Suave
call %~dp0deploy.bat win7-x64 debug %~dp0..\..\src\Suave
call %~dp0deploy.bat osx.10.10-x64 debug %~dp0..\..\src\Suave
call %~dp0deploy.bat osx.10.10-x64 debug %~dp0..\..\src\Suave
