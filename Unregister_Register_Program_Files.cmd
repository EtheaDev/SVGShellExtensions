echo Unregister dll
%systemroot%\SysWoW64\regsvr32 /u "%ProgramFiles%\Ethea\SVGShellExtensions\SVGShellExtensions32.dll"
%systemroot%\System32\regsvr32 /u "%ProgramFiles%\Ethea\SVGShellExtensions\SVGShellExtensions.dll"

echo Register dll
%systemroot%\SysWoW64\regsvr32 "%ProgramFiles%\Ethea\SVGShellExtensions\SVGShellExtensions32.dll"
%systemroot%\System32\regsvr32 "%ProgramFiles%\Ethea\SVGShellExtensions\SVGShellExtensions.dll"

pause