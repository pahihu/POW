@echo off

set dst_dir="C:\Program Files\Pow!"

echo.
echo This batch file copies the OPAL library to the proper Pow! directories.
echo -----------------------------------------------------------------------
echo.
echo Prior to running this batch file you should run the batch file Make.bat!
echo.
echo The following directories are used:
echo.
echo   Pow!:    %dst_dir%
echo   Windows: %SystemRoot%
echo.
pause

copy *.sym             %dst_dir%\Oberon-2\Opal
copy opal32a_dll.lib   %dst_dir%\Oberon-2\Opal\opal32dll.lib
copy opal32dllhelp.lib %dst_dir%\Oberon-2\Opal\opal32dllhelp.lib
copy opal32lib.lib     %dst_dir%\Oberon-2\Opal\opal32lib.lib
copy opal32a_dll.dll   %SystemRoot%\System32
