@echo off

set dst_dir="C:\Pow"

echo.
echo This batch file copies the WINAPI files to the proper Pow! directories.
echo -----------------------------------------------------------------------
echo.
echo Prior to running this batch file you should run the batch file Make.bat!
echo.
echo The following directories are used:
echo.
echo   WinApi:  %dst_dir%
echo.
pause

echo.
echo The proper directories for the files generated with this batch file are:
echo ------------------------------------------------------------------------
echo.
copy *.sym     %dst_dir%\Oberon-2\WinApi
copy Win32.lib %dst_dir%\Oberon-2\WinApi
rem copy Win32.hlp %dst_dir%
if errorlevel 1 pause
