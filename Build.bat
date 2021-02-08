call "C:\BDS\Studio\21.0\bin\rsvars.bat"
msbuild.exe "Source\SVGShellExtensions.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=release 
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO INNO
pause
EXIT

:INNO
"C:\Program Files (x86)\Inno Setup 6\iscc.exe" "D:\ETHEA\SVGShellExtensions\SVGShellExtensions.iss"
set INNO_STATUS=%ERRORLEVEL%
if %INNO_STATUS%==0 GOTO END
pause
EXIT


:END 
pause
