# Microsoft Developer Studio Project File - Name="basictests" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=basictests - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "basictests.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "basictests.mak" CFG="basictests - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "basictests - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "basictests - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "basictests - Win32 Release"

# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Cmd_Line "NMAKE /f basictests.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "basictests.exe"
# PROP BASE Bsc_Name "basictests.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Cmd_Line "nmake -f Makefile.NT -nologo -k"
# PROP Rebuild_Opt "-a"
# PROP Target_File "Nothing"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "basictests - Win32 Debug"

# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Cmd_Line "NMAKE /f basictests.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "basictests.exe"
# PROP BASE Bsc_Name "basictests.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Cmd_Line "nmake -f Makefile.NT -nologo -k"
# PROP Rebuild_Opt "-a"
# PROP Target_File "Nothing"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "basictests - Win32 Release"
# Name "basictests - Win32 Debug"

!IF  "$(CFG)" == "basictests - Win32 Release"

!ELSEIF  "$(CFG)" == "basictests - Win32 Debug"

!ENDIF 

# Begin Source File

SOURCE=.\Makefile.NT
# End Source File
# End Target
# End Project
