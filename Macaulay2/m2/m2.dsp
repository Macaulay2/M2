# Microsoft Developer Studio Project File - Name="m2" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=m2 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "m2.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "m2.mak" CFG="m2 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "m2 - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "m2 - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "m2 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "m2 - Win32 Debug"

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
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:I386 /out:"m2-setup" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "m2 - Win32 Release"
# Name "m2 - Win32 Debug"
# Begin Source File

SOURCE=..\e\misc\cmdnames.input

!IF  "$(CFG)" == "m2 - Win32 Release"

# Begin Custom Build - Making files from cmdnames.input
InputPath=..\e\misc\cmdnames.input

BuildCmds= \
	awk -f gbdoc.awk <$(InputPath)  >gbdoc.m2 \
	awk -f gbfuns.awk <$(InputPath) >gbfunctions.m2 \
	

"gbdoc.m2" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"gbfunctions.m2" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "m2 - Win32 Debug"

USERDEP__CMDNA="gbdoc.awk"	"gbfuns.awk"	
# Begin Custom Build - Making files from cmdnames.input
InputPath=..\e\misc\cmdnames.input

BuildCmds= \
	awk -f gbdoc.awk <$(InputPath)  >gbdoc.m2 \
	awk -f gbfuns.awk <$(InputPath) >gbfunctions.m2 \
	

"gbdoc.m2" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"gbfunctions.m2" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\dumpseq
USERDEP__DUMPS="loads.awk"	

!IF  "$(CFG)" == "m2 - Win32 Release"

# Begin Custom Build - Making loads.m2 from dumpseq
InputPath=.\dumpseq

"loads.m2" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	awk -f loads.awk <dumpseq >loads.m2

# End Custom Build

!ELSEIF  "$(CFG)" == "m2 - Win32 Debug"

# PROP Ignore_Default_Tool 1
# Begin Custom Build - Making loads.m2 from dumpseq
InputPath=.\dumpseq

"loads.m2" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	awk -f loads.awk <dumpseq >loads.m2

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\gbdoc.awk
# End Source File
# Begin Source File

SOURCE=.\gbdoc.m2

!IF  "$(CFG)" == "m2 - Win32 Release"

!ELSEIF  "$(CFG)" == "m2 - Win32 Debug"

# PROP Ignore_Default_Tool 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\gbfunctions.m2
# End Source File
# Begin Source File

SOURCE=.\gbfuns.awk
# End Source File
# Begin Source File

SOURCE=.\loads.awk
# End Source File
# Begin Source File

SOURCE=.\loads.m2

!IF  "$(CFG)" == "m2 - Win32 Release"

!ELSEIF  "$(CFG)" == "m2 - Win32 Debug"

# PROP Ignore_Default_Tool 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Makefile
# End Source File
# Begin Source File

SOURCE=.\tutorial.awk

!IF  "$(CFG)" == "m2 - Win32 Release"

# Begin Custom Build - Making tutorials.m2
InputPath=.\tutorial.awk

"tutorials.m2" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	ls ../tutorial/final/*.out | awk -f tutorial.awk >tutorials.m2

# End Custom Build

!ELSEIF  "$(CFG)" == "m2 - Win32 Debug"

# Begin Custom Build - Making tutorials.m2
InputPath=.\tutorial.awk

"tutorials.m2" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	ls ../tutorial/final/*.out | awk -f tutorial.awk >tutorials.m2

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\tutorials.m2
# End Source File
# Begin Source File

SOURCE=.\version.m2
# End Source File
# Begin Source File

SOURCE=..\msdos\version.m2

!IF  "$(CFG)" == "m2 - Win32 Release"

# Begin Custom Build - Copying ../msdos/version.m2
InputPath=..\msdos\version.m2

"version.m2" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy ..\msdos\version.m2 version.m2

# End Custom Build

!ELSEIF  "$(CFG)" == "m2 - Win32 Debug"

# PROP Ignore_Default_Tool 1
# Begin Custom Build - Copying ../msdos/version.m2
InputPath=..\msdos\version.m2

"version.m2" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy ..\msdos\version.m2 version.m2

# End Custom Build

!ENDIF 

# End Source File
# End Target
# End Project
