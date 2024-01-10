@echo off

echo.
echo This batch file creates the win32.lib containing the interface
echo specification for the Win32 API for Oberon-2.
echo.
echo Prior to running this batch job you need to compile any changes
echo using the project Win32.prj. You can safely ignore the linker
echo error after successful compilation, because the project is not
echo intended to create an executable application.
echo.
echo After changing the interface of any of the API definition modules,
echo all other libraries building upon those API modules need to be
echo recompiled before they can be used again.
echo.
echo ---------------------------------------------------------------------
echo For this batch file to run you need to have a library tool called
echo "lib" in your search path, which is not part of the Pow! distribution.
echo ---------------------------------------------------------------------
echo.
pause

del win32.lib
lib /out:win32.lib /LINK50COMPAT winnt.obj wingdi.obj winbase.obj windef.obj winuser.obj commdlg.obj commctrl.obj wincon.obj dde.obj ddeml.obj shellapi.obj
if errorlevel 1 pause