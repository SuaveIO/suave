(
SETLOCAL ENABLEDELAYEDEXPANSION
FOR %%I IN (%*) DO IF "%%I" == "--LCID:1033" ( 
      SET params=!params!
    ) ELSE (
      SET params=!params! %%I    
    )
)
(
ENDLOCAL
SET params=%params%
)
%~dp0packages\runtime.win7-x64.Microsoft.FSharp.Compiler.netcore\fsc.exe %params%