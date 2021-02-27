echo Unregister dll
call D:\ETHEA\SVGShellExtensions\Bin32\UnRegister32bit.bat
call D:\ETHEA\SVGShellExtensions\Bin64\UnRegister64bit.bat

echo Register dll
call D:\ETHEA\SVGShellExtensions\Bin32\Register32bit.bat
call D:\ETHEA\SVGShellExtensions\Bin64\Register64bit.bat

pause