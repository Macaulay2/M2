# Microsoft Developer Studio Project File - Name="d" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=d - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "d.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "d.mak" CFG="d - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "d - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "d - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "d - Win32 Release"

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
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /Za /GX /O2 /I "../../include" /I "../msdos" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:I386 /out:"Release/Macaulay2.exe"
# SUBTRACT LINK32 /map

!ELSEIF  "$(CFG)" == "d - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "d___Win3"
# PROP BASE Intermediate_Dir "d___Win3"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /Za /Gm /GX /Zi /Od /I "../../include" /I "../msdos" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 ../../gmp/Debug/gmp.lib ../../gmp/mpn/Debug/mpn.lib ../../gmp/mpz/Debug/mpz.lib ../dbm/Debug/dbm.lib ../e/Debug/e.lib ../../gcStatic/Debug/gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:I386 /out:"Debug/Macaulay2.exe" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "d - Win32 Release"
# Name "d - Win32 Debug"
# Begin Source File

SOURCE=.\actors.c
# End Source File
# Begin Source File

SOURCE=.\actors2.c
# End Source File
# Begin Source File

SOURCE=.\actors3.c
# End Source File
# Begin Source File

SOURCE=.\actors4.c
# End Source File
# Begin Source File

SOURCE=.\actors5.c
# End Source File
# Begin Source File

SOURCE=.\arith.c
# End Source File
# Begin Source File

SOURCE=.\basic.c
# End Source File
# Begin Source File

SOURCE=.\binding.c
# End Source File
# Begin Source File

SOURCE=.\C.c
# End Source File
# Begin Source File

SOURCE=..\c\compat.h
# End Source File
# Begin Source File

SOURCE=..\msdos\compat.h
# End Source File
# Begin Source File

SOURCE=.\convertr.c
# End Source File
# Begin Source File

SOURCE=.\ctype.c
# End Source File
# Begin Source File

SOURCE=.\err.c
# End Source File
# Begin Source File

SOURCE=.\GB.c
# End Source File
# Begin Source File

SOURCE=.\GC.c
# End Source File
# Begin Source File

SOURCE=.\interp.c
# End Source File
# Begin Source File

SOURCE=.\lex.c
# End Source File
# Begin Source File

SOURCE=.\mp.c
# End Source File
# Begin Source File

SOURCE=.\nets.c
# End Source File
# Begin Source File

SOURCE=.\objects.c
# End Source File
# Begin Source File

SOURCE=.\parser.c
# End Source File
# Begin Source File

SOURCE=.\scclib.c
# End Source File
# Begin Source File

SOURCE=.\stdio.c
# End Source File
# Begin Source File

SOURCE=.\stdiop.c
# End Source File
# Begin Source File

SOURCE=.\strings.c
# End Source File
# Begin Source File

SOURCE=.\struct.c
# End Source File
# Begin Source File

SOURCE=.\system.c
# End Source File
# Begin Source File

SOURCE=..\msdos\tmp_init.c
# End Source File
# Begin Source File

SOURCE=.\tokens.c
# End Source File
# Begin Source File

SOURCE=.\varstrin.c
# End Source File
# End Target
# End Project
