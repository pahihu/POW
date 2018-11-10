@echo off

echo.
echo This batch file creates the OPAL library for static and dynamic linking.
echo.
echo Prior to running this batch job you need to compile any changes using both
echo the projects Opal32??_Lib.prj and Opal32??_Dll. You can safely ignore the
echo linker warning when building the project Opal32??_lib.
echo.
echo After changing the interface of any of the OPAL modules, all other projects
echo based on the generated Opal32??_dll.dll need to be recompiled using the
echo "Compile - Build" menu entry before they can be used again.
echo.
echo ---------------------------------------------------------------------
echo For this batch file to run you need to have a library tool called
echo "lib" in your search path, which is not part of the Pow! distribution.
echo ---------------------------------------------------------------------
echo.
pause

rem -- create opal32lib.lib --
lib @libcmd
if errorlevel 1 goto end

rem -- create opal32dllhelp.lib --
del opal32dllhelp.lib
lib /OUT:opal32dllhelp.lib /SUBSYSTEM:WINDOWS /DEBUGTYPE:CV /LINK50COMPAT starthlp.obj starthlpint.obj
if errorlevel 1 goto end

:end

echo.
echo The proper directories for the files generated with this batch file are:
echo ------------------------------------------------------------------------
echo.
echo  - *.sym ................ \Pow!\Oberon-2\Opal
echo  - opal32??_dll.lib ..... \Pow!\Oberon-2\Opal\opal32dll.lib
echo  - opal32dllhelp.lib .... \Pow!\Oberon-2\Opal\opal32dllhelp.lib
echo  - opal32lib.lib ........ \Pow!\Oberon-2\Opal\opal32lib.lib
echo  - opal32??_dll.dll ..... %SystemRoot%\System32
