# Microsoft Developer Studio Project File - Name="tutorial" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=tutorial - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "tutorial.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "tutorial.mak" CFG="tutorial - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "tutorial - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "tutorial - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "tutorial - Win32 Release"

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
# ADD CPP /nologo /Za /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "tutorial - Win32 Debug"

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
# ADD CPP /nologo /Za /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:I386 /out:"tutorial-setup" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "tutorial - Win32 Release"
# Name "tutorial - Win32 Debug"
# Begin Source File

SOURCE=.\final\canEmbed.m2

!IF  "$(CFG)" == "tutorial - Win32 Release"

# Begin Custom Build - Translating tutorial
InputPath=.\final\canEmbed.m2
InputName=canEmbed

"final/$(InputName).out" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	translate <$(InputPath) >final/$(InputName).out 
	echo made final/$(InputName).out 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "tutorial - Win32 Debug"

# Begin Custom Build - Translating tutorial
InputPath=.\final\canEmbed.m2
InputName=canEmbed

"final/$(InputName).out" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	translate <$(InputPath) >final/$(InputName).out 
	echo made final/$(InputName).out 
	
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\final\canEmbed.out
# End Source File
# Begin Source File

SOURCE=.\final\divisors.m2

!IF  "$(CFG)" == "tutorial - Win32 Release"

# Begin Custom Build - Translating tutorial
InputPath=.\final\divisors.m2
InputName=divisors

"final/$(InputName).out" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	translate <$(InputPath) >final/$(InputName).out 
	echo made final/$(InputName).out 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "tutorial - Win32 Debug"

# Begin Custom Build - Translating tutorial
InputPath=.\final\divisors.m2
InputName=divisors

"final/$(InputName).out" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	translate <$(InputPath) >final/$(InputName).out 
	echo made final/$(InputName).out 
	
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\final\divisors.out
# End Source File
# Begin Source File

SOURCE=.\final\elementary.m2

!IF  "$(CFG)" == "tutorial - Win32 Release"

# Begin Custom Build - Translating tutorial
InputPath=.\final\elementary.m2
InputName=elementary

"final/$(InputName).out" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	translate <$(InputPath) >final/$(InputName).out 
	echo made final/$(InputName).out 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "tutorial - Win32 Debug"

# Begin Custom Build - Translating tutorial
InputPath=.\final\elementary.m2
InputName=elementary

"final/$(InputName).out" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	translate <$(InputPath) >final/$(InputName).out 
	echo made final/$(InputName).out 
	
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\final\elementary.out
# End Source File
# Begin Source File

SOURCE=.\final\Fano.m2

!IF  "$(CFG)" == "tutorial - Win32 Release"

# Begin Custom Build - Translating tutorial
InputPath=.\final\Fano.m2
InputName=Fano

"final/$(InputName).out" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	translate <$(InputPath) >final/$(InputName).out 
	echo made final/$(InputName).out 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "tutorial - Win32 Debug"

# Begin Custom Build - Translating tutorial
InputPath=.\final\Fano.m2
InputName=Fano

"final/$(InputName).out" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	translate <$(InputPath) >final/$(InputName).out 
	echo made final/$(InputName).out 
	
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\final\Fano.out
# End Source File
# Begin Source File

SOURCE=.\final\HomAlg2.m2

!IF  "$(CFG)" == "tutorial - Win32 Release"

# Begin Custom Build - Translating tutorial
InputPath=.\final\HomAlg2.m2
InputName=HomAlg2

"final/$(InputName).out" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	translate <$(InputPath) >final/$(InputName).out 
	echo made final/$(InputName).out 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "tutorial - Win32 Debug"

# Begin Custom Build - Translating tutorial
InputPath=.\final\HomAlg2.m2
InputName=HomAlg2

"final/$(InputName).out" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	translate <$(InputPath) >final/$(InputName).out 
	echo made final/$(InputName).out 
	
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\final\HomAlg2.out
# End Source File
# Begin Source File

SOURCE=.\Makefile
# End Source File
# End Target
# End Project
