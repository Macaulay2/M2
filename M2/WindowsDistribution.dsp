# Microsoft Developer Studio Project File - Name="WindowsDistribution" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=WindowsDistribution - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "WindowsDistribution.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "WindowsDistribution.mak"\
 CFG="WindowsDistribution - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "WindowsDistribution - Win32 Release" (based on\
 "Win32 (x86) External Target")
!MESSAGE "WindowsDistribution - Win32 Debug" (based on\
 "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "WindowsDistribution - Win32 Release"

# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Cmd_Line "NMAKE /f WindowsDistribution.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "WindowsDistribution.exe"
# PROP BASE Bsc_Name "WindowsDistribution.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Cmd_Line "NMAKE /f WindowsNT/WindowsDistribution.mak"
# PROP Rebuild_Opt ""
# PROP Target_File "NoFile"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "WindowsDistribution - Win32 Debug"

# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Cmd_Line "NMAKE /f WindowsDistribution.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "WindowsDistribution.exe"
# PROP BASE Bsc_Name "WindowsDistribution.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Cmd_Line "NMAKE /f WindowsNT/WindowsDistribution.mak"
# PROP Rebuild_Opt ""
# PROP Target_File "NoFile"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "WindowsDistribution - Win32 Release"
# Name "WindowsDistribution - Win32 Debug"

!IF  "$(CFG)" == "WindowsDistribution - Win32 Release"

!ELSEIF  "$(CFG)" == "WindowsDistribution - Win32 Debug"

!ENDIF 

# Begin Source File

SOURCE=.\WindowsNT\DistributionFiles.lst
# End Source File
# Begin Source File

SOURCE=..\README.WINNT
# End Source File
# Begin Source File

SOURCE=.\WindowsNT\WindowsDistribution.mak
# End Source File
# End Target
# End Project
