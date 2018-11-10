# Microsoft Developer Studio Project File - Name="pow" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=pow - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "pow.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "pow.mak" CFG="pow - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "pow - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "pow - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "pow - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /Od /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /stack:0x10000000 /subsystem:windows /machine:I386

!ELSEIF  "$(CFG)" == "pow - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /stack:0x10000000 /subsystem:windows /pdb:none /debug /machine:I386

!ENDIF 

# Begin Target

# Name "pow - Win32 Release"
# Name "pow - Win32 Debug"
# Begin Source File

SOURCE=.\Bmp00001.bmp
# End Source File
# Begin Source File

SOURCE=.\CHILDREN.ICO
# End Source File
# Begin Source File

SOURCE=.\COMP.BMP
# End Source File
# Begin Source File

SOURCE=.\Fimabout.bmp
# End Source File
# Begin Source File

SOURCE=.\Fimlogo.bmp
# End Source File
# Begin Source File

SOURCE=.\Fimlogo2.bmp
# End Source File
# Begin Source File

SOURCE=.\GREY.BMP
# End Source File
# Begin Source File

SOURCE=.\Logo.bmp
# End Source File
# Begin Source File

SOURCE=.\Logosel.bmp
# End Source File
# Begin Source File

SOURCE=.\main256.bmp
# End Source File
# Begin Source File

SOURCE=.\Make.bmp
# End Source File
# Begin Source File

SOURCE=.\Makesel.bmp
# End Source File
# Begin Source File

SOURCE=.\Open.bmp
# End Source File
# Begin Source File

SOURCE=.\Opengry.bmp
# End Source File
# Begin Source File

SOURCE=.\Opensel.bmp
# End Source File
# Begin Source File

SOURCE=.\POW.C
# End Source File
# Begin Source File

SOURCE=.\POW.ICO
# End Source File
# Begin Source File

SOURCE=.\POW.RC
# End Source File
# Begin Source File

SOURCE=.\pow32.def
# End Source File
# Begin Source File

SOURCE=.\POWBUG.C
# End Source File
# Begin Source File

SOURCE=.\POWCOMP.C
# End Source File
# Begin Source File

SOURCE=.\powCompiler.c
# End Source File
# Begin Source File

SOURCE=.\POWDDE.C
# End Source File
# Begin Source File

SOURCE=.\POWED.C
# End Source File
# Begin Source File

SOURCE=.\POWFILE.C
# End Source File
# Begin Source File

SOURCE=.\POWFIND.C
# End Source File
# Begin Source File

SOURCE=.\POWINIT.C
# End Source File
# Begin Source File

SOURCE=.\powintro.c
# End Source File
# Begin Source File

SOURCE=.\POWOPEN.C
# End Source File
# Begin Source File

SOURCE=.\POWOPTS.C
# End Source File
# Begin Source File

SOURCE=.\POWPRINT.C
# End Source File
# Begin Source File

SOURCE=.\POWPROJ.C
# End Source File
# Begin Source File

SOURCE=.\POWRIBB.C
# End Source File
# Begin Source File

SOURCE=.\POWRUN.C
# End Source File
# Begin Source File

SOURCE=.\POWSTAT.C
# End Source File
# Begin Source File

SOURCE=.\POWTEMP.C
# End Source File
# Begin Source File

SOURCE=.\POWTEXT.C
# End Source File
# Begin Source File

SOURCE=.\POWTOOLS.C
# End Source File
# Begin Source File

SOURCE=.\Print.bmp
# End Source File
# Begin Source File

SOURCE=.\Printsel.bmp
# End Source File
# Begin Source File

SOURCE=.\Rungry.bmp
# End Source File
# Begin Source File

SOURCE=.\Save.bmp
# End Source File
# Begin Source File

SOURCE=.\SAVEGRY.BMP
# End Source File
# Begin Source File

SOURCE=.\Savesel.bmp
# End Source File
# Begin Source File

SOURCE=.\SBAR.BMP
# End Source File
# Begin Source File

SOURCE=.\SHADE.BMP
# End Source File
# Begin Source File

SOURCE=.\STAT.BMP
# End Source File
# Begin Source File

SOURCE=.\Tool.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool1.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool10.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool11.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool12.bmp
# End Source File
# Begin Source File

SOURCE=.\TOOL13.BMP
# End Source File
# Begin Source File

SOURCE=.\Tool14.bmp
# End Source File
# Begin Source File

SOURCE=.\TOOL17.BMP
# End Source File
# Begin Source File

SOURCE=.\Tool2.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool23.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool24.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool25.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool26.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool27.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool3.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool30.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool33.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool34.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool35.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool36.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool37.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool38.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool39.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool4.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool40.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool41.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool42.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool43.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool5.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool6.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool7.bmp
# End Source File
# Begin Source File

SOURCE=.\Tool8.bmp
# End Source File
# Begin Source File

SOURCE=.\Toolsel6.bmp
# End Source File
# End Target
# End Project
