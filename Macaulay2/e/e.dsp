# Microsoft Developer Studio Project File - Name="e" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=e - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "e.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "e.mak" CFG="e - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "e - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "e - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe

!IF  "$(CFG)" == "e - Win32 Release"

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
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "../../i nclude" /I "../../include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "e - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /I "../../include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /FD /c
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "e - Win32 Release"
# Name "e - Win32 Debug"
# Begin Source File

SOURCE=.\array.cpp
# End Source File
# Begin Source File

SOURCE=.\array.hpp
# End Source File
# Begin Source File

SOURCE=.\assprime.cpp
# End Source File
# Begin Source File

SOURCE=.\assprime.hpp
# End Source File
# Begin Source File

SOURCE=.\bin_io.cpp
# End Source File
# Begin Source File

SOURCE=.\bin_io.hpp
# End Source File
# Begin Source File

SOURCE=.\classes.hpp
# End Source File
# Begin Source File

SOURCE=.\cmdinst.hpp
# End Source File
# Begin Source File

SOURCE=.\cmdnames.hpp
# End Source File
# Begin Source File

SOURCE=.\misc\cmdnames.input

!IF  "$(CFG)" == "e - Win32 Release"

# Begin Custom Build - Processing cmdnames.input
InputPath=.\misc\cmdnames.input

BuildCmds= \
	gawk -f misc/cmdh.awk < misc\cmdnames.input > cmdnames.hpp \
	echo made cmdnames.hpp \
	gawk -f misc/cmdinst.awk <misc\cmdnames.input >cmdinst.hpp \
	echo made cmdinst.hpp \
	gawk -f misc/cmdg.awk <misc\cmdnames.input >..\m2\cmdnames.m2 \
	echo made ..\m2\cmdnames.m2 \
	

"cmdnames.hpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"cmdinst.hpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"../m2/cmdnames.m2" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "e - Win32 Debug"

# Begin Custom Build - Processing cmdnames.input
InputPath=.\misc\cmdnames.input

BuildCmds= \
	gawk -f misc/cmdh.awk < misc\cmdnames.input > cmdnames.hpp \
	echo made cmdnames.hpp \
	gawk -f misc/cmdinst.awk <misc\cmdnames.input >cmdinst.hpp \
	echo made cmdinst.hpp \
	gawk -f misc/cmdg.awk <misc\cmdnames.input >..\m2\cmdnames.m2 \
	echo made ..\m2\cmdnames.m2 \
	

"cmdnames.hpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"cmdinst.hpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"../m2/cmdnames.m2" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\comb.cpp
# End Source File
# Begin Source File

SOURCE=.\comb.hpp
# End Source File
# Begin Source File

SOURCE=.\comp.hpp
# End Source File
# Begin Source File

SOURCE=.\det.cpp
# End Source File
# Begin Source File

SOURCE=.\det.hpp
# End Source File
# Begin Source File

SOURCE=.\error.cpp
# End Source File
# Begin Source File

SOURCE=.\error.hpp
# End Source File
# Begin Source File

SOURCE=.\frac.cpp
# End Source File
# Begin Source File

SOURCE=.\frac.hpp
# End Source File
# Begin Source File

SOURCE=.\freemod.cpp
# End Source File
# Begin Source File

SOURCE=.\freemod.hpp
# End Source File
# Begin Source File

SOURCE=.\freemod2.cpp
# End Source File
# Begin Source File

SOURCE=.\gauss.cpp
# End Source File
# Begin Source File

SOURCE=.\gauss.hpp
# End Source File
# Begin Source File

SOURCE=.\gb.cpp
# End Source File
# Begin Source File

SOURCE=.\gb.hpp
# End Source File
# Begin Source File

SOURCE=.\gb2.cpp
# End Source File
# Begin Source File

SOURCE=.\gb2.hpp
# End Source File
# Begin Source File

SOURCE=.\gb_comp.hpp
# End Source File
# Begin Source File

SOURCE=.\gbbinom.cpp
# End Source File
# Begin Source File

SOURCE=.\gbbinom.hpp
# End Source File
# Begin Source File

SOURCE=.\gbinhom.cpp
# End Source File
# Begin Source File

SOURCE=.\gbinhom.hpp
# End Source File
# Begin Source File

SOURCE=.\gbnod.cpp
# End Source File
# Begin Source File

SOURCE=.\geores.hpp
# End Source File
# Begin Source File

SOURCE=.\geoT.hpp
# End Source File
# Begin Source File

SOURCE=.\geovec.hpp
# End Source File
# Begin Source File

SOURCE=.\GF.cpp
# End Source File
# Begin Source File

SOURCE=.\GF.hpp
# End Source File
# Begin Source File

SOURCE=.\handles.cpp
# End Source File
# Begin Source File

SOURCE=.\handles.hpp
# End Source File
# Begin Source File

SOURCE=.\hashtab.cpp
# End Source File
# Begin Source File

SOURCE=.\hashtab.hpp
# End Source File
# Begin Source File

SOURCE=.\hermite.cpp
# End Source File
# Begin Source File

SOURCE=.\hermite.hpp
# End Source File
# Begin Source File

SOURCE=.\hilb.cpp
# End Source File
# Begin Source File

SOURCE=.\hilb.hpp
# End Source File
# Begin Source File

SOURCE=.\index.hpp
# End Source File
# Begin Source File

SOURCE=.\int_bag.cpp
# End Source File
# Begin Source File

SOURCE=.\int_bag.hpp
# End Source File
# Begin Source File

SOURCE=.\intarray.cpp
# End Source File
# Begin Source File

SOURCE=.\intarray.hpp
# End Source File
# Begin Source File

SOURCE=.\interp.cpp
# End Source File
# Begin Source File

SOURCE=.\interp.hpp
# End Source File
# Begin Source File

SOURCE=.\mac2.c
# End Source File
# Begin Source File

SOURCE=.\matrix.cpp
# End Source File
# Begin Source File

SOURCE=.\matrix.hpp
# End Source File
# Begin Source File

SOURCE=.\mem.cpp
# End Source File
# Begin Source File

SOURCE=.\mem.hpp
# End Source File
# Begin Source File

SOURCE=.\monideal.cpp

!IF  "$(CFG)" == "e - Win32 Release"

# ADD CPP /Od

!ELSEIF  "$(CFG)" == "e - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\monideal.hpp
# End Source File
# Begin Source File

SOURCE=.\monoid.cpp

!IF  "$(CFG)" == "e - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "e - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\monoid.hpp
# End Source File
# Begin Source File

SOURCE=.\monomial.cpp
# End Source File
# Begin Source File

SOURCE=.\monomial.hpp
# End Source File
# Begin Source File

SOURCE=.\monorder.cpp
# End Source File
# Begin Source File

SOURCE=.\monorder.hpp
# End Source File
# Begin Source File

SOURCE=.\newspair.cpp
# End Source File
# Begin Source File

SOURCE=.\newspair.hpp
# End Source File
# Begin Source File

SOURCE=.\ntuple.cpp
# End Source File
# Begin Source File

SOURCE=.\ntuple.hpp
# End Source File
# Begin Source File

SOURCE=.\obj_iarr.hpp
# End Source File
# Begin Source File

SOURCE=.\obj_int.hpp
# End Source File
# Begin Source File

SOURCE=.\obj_prim.cpp
# End Source File
# Begin Source File

SOURCE=.\obj_prim.hpp
# End Source File
# Begin Source File

SOURCE=.\obj_ptr.hpp
# End Source File
# Begin Source File

SOURCE=.\obj_str.hpp
# End Source File
# Begin Source File

SOURCE=.\object.cpp
# End Source File
# Begin Source File

SOURCE=.\object.hpp
# End Source File
# Begin Source File

SOURCE=.\pfaff.cpp
# End Source File
# Begin Source File

SOURCE=.\pfaff.hpp
# End Source File
# Begin Source File

SOURCE=.\polyring.cpp
# End Source File
# Begin Source File

SOURCE=.\polyring.hpp
# End Source File
# Begin Source File

SOURCE=.\queue.cpp
# End Source File
# Begin Source File

SOURCE=.\queue.hpp
# End Source File
# Begin Source File

SOURCE=.\random.cpp
# End Source File
# Begin Source File

SOURCE=.\random.hpp
# End Source File
# Begin Source File

SOURCE=.\relem.cpp
# End Source File
# Begin Source File

SOURCE=.\relem.hpp
# End Source File
# Begin Source File

SOURCE=.\res.cpp
# End Source File
# Begin Source File

SOURCE=.\res.hpp
# End Source File
# Begin Source File

SOURCE=.\res2.cpp
# End Source File
# Begin Source File

SOURCE=.\res2.hpp
# End Source File
# Begin Source File

SOURCE=.\respair2.hpp
# End Source File
# Begin Source File

SOURCE=.\respoly.cpp
# End Source File
# Begin Source File

SOURCE=.\respoly.hpp
# End Source File
# Begin Source File

SOURCE=.\respoly2.cpp
# End Source File
# Begin Source File

SOURCE=.\respoly2.hpp
# End Source File
# Begin Source File

SOURCE=.\ring.cpp
# End Source File
# Begin Source File

SOURCE=.\ring.hpp
# End Source File
# Begin Source File

SOURCE=.\ringelem.hpp
# End Source File
# Begin Source File

SOURCE=.\ringmap.cpp
# End Source File
# Begin Source File

SOURCE=.\ringmap.hpp
# End Source File
# Begin Source File

SOURCE=.\sagbi.cpp
# End Source File
# Begin Source File

SOURCE=.\sagbi.hpp
# End Source File
# Begin Source File

SOURCE=.\schur.cpp
# End Source File
# Begin Source File

SOURCE=.\schur.hpp
# End Source File
# Begin Source File

SOURCE=.\spair.cpp
# End Source File
# Begin Source File

SOURCE=.\spair.hpp
# End Source File
# Begin Source File

SOURCE=.\stack.cpp
# End Source File
# Begin Source File

SOURCE=.\stack.hpp
# End Source File
# Begin Source File

SOURCE=.\style.hpp
# End Source File
# Begin Source File

SOURCE=.\termideal.cpp
# End Source File
# Begin Source File

SOURCE=.\termideal.hpp
# End Source File
# Begin Source File

SOURCE=.\text_io.cpp
# End Source File
# Begin Source File

SOURCE=.\text_io.hpp
# End Source File
# Begin Source File

SOURCE=.\varpower.cpp
# End Source File
# Begin Source File

SOURCE=.\varpower.hpp
# End Source File
# Begin Source File

SOURCE=.\vector.cpp
# End Source File
# Begin Source File

SOURCE=.\vector.hpp
# End Source File
# Begin Source File

SOURCE=.\x_factor.cpp
# End Source File
# Begin Source File

SOURCE=.\x_free.cpp
# End Source File
# Begin Source File

SOURCE=.\x_gb.cpp
# End Source File
# Begin Source File

SOURCE=.\x_mat.cpp
# End Source File
# Begin Source File

SOURCE=.\x_monoid.cpp
# End Source File
# Begin Source File

SOURCE=.\x_monom.cpp
# End Source File
# Begin Source File

SOURCE=.\x_relem.cpp
# End Source File
# Begin Source File

SOURCE=.\x_system.cpp
# End Source File
# Begin Source File

SOURCE=.\Z.cpp
# End Source File
# Begin Source File

SOURCE=.\Z.hpp
# End Source File
# Begin Source File

SOURCE=.\z_mod_p.cpp
# End Source File
# Begin Source File

SOURCE=.\z_mod_p.hpp
# End Source File
# End Target
# End Project
