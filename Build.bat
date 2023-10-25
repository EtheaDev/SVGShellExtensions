call "C:\BDS\Studio\23.0\bin\rsvars.bat"
msbuild.exe "Source\SVGShellExtensions.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=release
msbuild.exe "Source\SVGShellExtensions32.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=release
msbuild.exe "Source\SVGTextEditor.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=release
msbuild.exe "Source\SVGTextEditor.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=release

call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\SVGShellExtensions\Bin32\SVGTextEditor.exe
call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\SVGShellExtensions\Bin64\SVGTextEditor.exe

:INNO
"C:\Program Files (x86)\Inno Setup 6\iscc.exe" "D:\ETHEA\SVGShellExtensions\Setup\SVGShellExtensions.iss"
set INNO_STATUS=%ERRORLEVEL%
if %INNO_STATUS%==0 GOTO SIGNSETUP
pause
EXIT

:SIGNSETUP
call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\SVGShellExtensions\Setup\Output\SVGShellExtensionsSetup.exe

:END
pause
