@echo off

echo.
echo The proper directories for the files generated with this batch file are:
echo ------------------------------------------------------------------------
echo.
echo  - *.sym ................ \Pow!\Oberon-2\Opal
echo  - opal32??_dll.lib ..... \Pow!\Oberon-2\Opal\opal32dll.lib
echo  - opal32dllhelp.lib .... \Pow!\Oberon-2\Opal\opal32dllhelp.lib
echo  - opal32lib.lib ........ \Pow!\Oberon-2\Opal\opal32lib.lib
echo  - opal32??_dll.dll ..... %SystemRoot%\System32

md c:\temp\opal32
md c:\temp\opal32\Pow!
md c:\temp\opal32\Pow!\Oberon-2
md c:\temp\opal32\Pow!\Oberon-2\Opal
md c:\temp\opal32\Winnt
md c:\temp\opal32\Winnt\System32

copy *.sym             c:\temp\opal32\Pow!\Oberon-2\Opal
copy opal32a_dll.lib   c:\temp\opal32\Pow!\Oberon-2\Opal\opal32dll.lib
copy opal32dllhelp.lib c:\temp\opal32\Pow!\Oberon-2\Opal
copy opal32lib.lib     c:\temp\opal32\Pow!\Oberon-2\Opal
copy opal32a_dll.dll   c:\temp\opal32\Winnt\System32

if exist c:\temp\opal.zip del c:\temp\opal.zip
"c:\program files\winzip\winzip32" -a -r c:\temp\opal.zip c:\temp\opal32\*.*

del c:\temp\opal32\*.* /s /q

rd c:\temp\opal32\Winnt\System32
rd c:\temp\opal32\Winnt
rd c:\temp\opal32\Pow!\Oberon-2\Opal
rd c:\temp\opal32\Pow!\Oberon-2
rd c:\temp\opal32\Pow!
rd c:\temp\opal32

