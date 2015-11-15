@echo off
pushd %~dp0
if '%1'== '' ( echo "    deploy arg1 arg2 arg 3"
               echo "    arg1 = platform:  win7-x86 | win7-x64 | osx.10.10-x64 | ubuntu.14.04-x64"
               echo "    arg2 = flavour: debug, release"
               echo "    arg3 = path to project folder"
               popd
               goto :exit
             )
set platform=win7-x64
set command_line_args=
set command_line_args=%command_line_args% --exec %~dp0deploy.fsx
set command_line_args=%command_line_args% --targetPlatformName:DNXCore,Version=v5.0/%1
set command_line_args=%command_line_args% --packagesDir:%~d0%~p0..\..\packages 
set command_line_args=%command_line_args% --projectJsonLock:%3\project.lock.json
set command_line_args=%command_line_args% --projectOutput:%3\bin\coreclr\%2
set command_line_args=%command_line_args% --output:%3\bin\deployment\%1
set command_line_args=%command_line_args% --compilerPath:%~dp0..\..\packages\runtime.%1.Microsoft.FSharp.Compiler.netcore
set command_line_args=%command_line_args% --v:quiet
echo %command_line_args%
fsi %command_line_args%
echo Errorlevel: %errorlevel%
:exit
popd
