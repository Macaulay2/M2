#ARING = true
TOPDIR = ../..
include $(TOPDIR)/Makeconf
# MES:
CXXFLAGS = -DGCMALLOC -DDEBUG

%.o : %.cpp
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $< $(OUTPUT_OPTION)

AWK      = awk

ifndef CC
CC       = gcc
endif

ifndef CXX
CXX      = g++ 
endif

ifeq "$(CC)" "cl"
WARNINGS = -W3
else
WARNINGS = -Wno-import -Wchar-subscripts -Wcomment -Wformat -Wimplicit \
	   -Wparentheses -Wreturn-type -Wswitch -Wtrigraphs -Wuninitialized \
	   -Wreorder -Wtemplate-debugging
WARNINGS = -Wall
endif

CPPFLAGS += -I$(INCDIR) #-DNDEBUG  #MES

ifdef ARING
CPPFLAGS += -DARING
endif

# CXXFLAGS += -fhandle-exceptions

######################
# -pg is now handled in ../../Makeconf!
CXXFLAGS += $(DEBUGFLAGS)
######################

ifeq "$(CC)" "cl"
# cl doesn't know about -O3
CXXFLAGS += -O2
else
#CXXFLAGS += -O3 #MES
CXXFLAGS += -g $(WARNINGS)
endif

ifdef SHAREDLIBS
CXXFLAGS += -fPIC
endif

CFLAGS = $(CXXFLAGS)
# ARFLAGS  = r
LIBNAME  = gb

ifeq "$(CC)" "cl"
LIB      = lib$(LIBNAME).a
else
LIB      = $(LIBNAME).lib
endif

PRE.cc   = $(COMPILE.cc) -E 

##################
## Source code ###
##################

CONTAINER = \
	array \
	hashtab \
	queue \
	stack 

INTERFACE = \
	buffer \
	bin_io \
	handles \
	interp \
	object \
	obj_prim \
	text_io

INTERFACE_H = obj_int obj_iarr obj_str obj_ptr

ESTUFF = \
	EZZp \
	Emonorder \
	Emonoid \
	Epoly \
	Ering \
	Evector \
	Efreemod \
	Ematrix \
	Eringmap \
	Eio \
	Ecommands

E_H = \
	Edefs.hpp \
	Ehashtab.hpp \
	Ematrix.hpp \
	Eringmap.hpp \
	Epolywrap.hpp

COMMANDS = \
	x_monoid \
	x_monom \
	x_system \
	x_free \
	x_mat \
	x_relem \
	x_gb \
	freemod2 \
	x_factor \
	gbnod

NAMES = \
	newmonorder \
	serial \
	error \
	ntuple \
	res \
	respoly \
	hilb \
	frac \
	polyring \
	weylalg \
	freemod \
	weylfree \
	matrix \
	sparsemat \
	LLL \
	relem \
	ring \
	vector \
	z_mod_p \
	Z \
	GF \
	schur \
	monideal \
	termideal \
	newspair \
	assprime \
	det \
	pfaff \
	ringmap \
	hermite \
	lattice \
	gauss \
	comb \
	mem \
	int_bag \
	intarray \
	monorder \
	monomial \
	varpower \
	monoid \
	gb \
	spair \
	gbinhom \
	gbbinom \
	sagbi \
	random \
	gbZZ \
	gb2 \
	res2 \
	respoly2

NAMES_H = \
	comp \
	ringelem \
	style \
	classes \
	index \
	gb_comp \
	respair2

C_FILES = mac2

ifdef ARING
#NAMES += ARING/polywrap
NAMES += ARING/poly
endif


##############################
## end of source code files ##
##############################

GENERATED_H = cmdnames cmdinst geovec geores

CC_FILES1 := $(ESTUFF) $(NAMES) $(CONTAINER) $(INTERFACE)
CC_FILES2 := $(COMMANDS)
CC_FILES := $(CC_FILES1) $(CC_FILES2)
FILES := $(CC_FILES) $(C_FILES)
LOFILES1 := $(addsuffix .lo,$(CC_FILES1))
LOFILES2 := $(addsuffix .lo,$(CC_FILES2) $(C_FILES))
LOFILES := $(LOFILES1) $(LOFILES2)
CCFILES := $(addsuffix .cpp,$(CC_FILES))
CFILES := $(addsuffix .c,$(C_FILES))
OFILES := $(addsuffix .o,$(C_FILES) $(CC_FILES))

HHFILES := $(addsuffix .hpp, \
		$(NAMES_H) $(NAMES) \
		$(INTERFACE_H) $(INTERFACE) $(CONTAINER) $(E_H))

OTHERS := Makefile \
	res_aux.cpp res_aux2.cpp geoT.hpp \
	tests misc keep newmonoid.hpp newmonoid.cpp

ALLFILES := $(CCFILES) $(CFILES) $(HHFILES) $(OTHERS) 

###################################################################
## Targets ##
#############
all:: cmdnames.m2 $(addsuffix .hpp, $(GENERATED_H))

ifdef SHAREDLIBS
all:: ../lib/libengine1.so ../lib/libengine2.so
else
all:: $(OFILES)
endif

%.ii: %.cpp
	$(PRE.cc) $< -o $@
%.s: %.cpp
	$(CXX) $(CXXFLAGS) -S $< -o $@

%.lo : %.c  ; $(COMPILE.c)  -fPIC $< $(OUTPUT_OPTION)
%.lo : %.cc ; $(COMPILE.cc) -fPIC $< $(OUTPUT_OPTION)
%.lo : %.cpp; $(COMPILE.cc) -fPIC $< $(OUTPUT_OPTION)
../lib/libengine1.so : $(LOFILES1); $(CC) -shared $^ $(OUTPUT_OPTION)
../lib/libengine2.so : $(LOFILES2); $(CC) -shared $^ $(OUTPUT_OPTION)

geovec.hpp: geoT.hpp
	awk '{sub(/FREEMODULETYPE/, "FreeModule"); sub(/VECTYPE/, "vecterm *"); print }' geoT.hpp >$@

geores.hpp: geoT.hpp
	awk '{sub(/FREEMODULETYPE/, "res2_poly"); sub(/VECTYPE/, "res2term *"); print }' geoT.hpp >$@

#geopoly.hpp: geoT.hpp
#	awk '{sub(/FREEMODULETYPE/, "Ring"); sub(/VECTYPE/, "Nterm *"); print }' geoT.hpp >$@

cmdnames.m2 : misc/cmdnames.input misc/cmdg.awk
	$(AWK) -f misc/cmdg.awk misc/cmdnames.input >cmdnames.m2
cmdnames.hpp : misc/cmdnames.input
	$(AWK) -f misc/cmdh.awk $^ >$@
cmdinst.hpp : misc/cmdnames.input
	$(AWK) -f misc/cmdinst.awk $^ >$@

tags: TAGS

TAGS: $(HFILES) $(CCFILES)
	etags *.hpp *.cpp *.c

clean:
	rm -f *.lo *.o *.dd *.ii $(LIB) allfiles cmdinst.hpp cmdnames.hpp cmdnames.m2
	rm -f TAGS

Makefile-o.depends: Makefile $(HFILES) $(CCFILES) $(CFILES)
	touch $@
	makedepend -o.o -f$@ -Y -I../../include -- $(CFLAGS) -- $(CCFILES) $(CFILES)
Makefile-lo.depends: Makefile $(HFILES) $(CCFILES) $(CFILES)
	touch $@
	makedepend -o.lo -f$@ -Y -I../../include -- $(CFLAGS) -- $(CCFILES) $(CFILES)

include Makefile-o.depends
include Makefile-lo.depends

allfiles: Makefile
	@echo making allfiles
	@echoout '>allfiles' $(ALLFILES)

install:

tar : allfiles
	tar cf e.tar `cat allfiles`

e.tgz : allfiles
	tar zcf e.tgz `cat allfiles`

ifeq "$(CC)" "cl"
x_relem.o : x_relem.cpp
	$(CXX) -c `echo $(CXXFLAGS) | sed 's/-O[0-9] //'` $(CPPFLAGS) $< $(OUTPUT_OPTION)
endif

m2.exe : # used on Wintel

wc ::; wc -l *.hpp *.cpp 

########################
## Obsolete targets ####
########################

ifeq "$(CC)" "cl"
$(LIB): $(OFILES)
	$(AR) $(ARFLAGS) /out:$@ $^	
else
$(LIB): $(LIB)($(OFILES))
	$(RANLIB) $(LIB) || true
endif

%.dd: %.cpp
	@echo remaking $@
	g++ -I$(INCDIR) -MM $(CPPFLAGS) $< | sed 's|$*\.o|& $@|g' > tmp.dd
	mv tmp.dd $@
# we say g++ above instead of $(CXX) because -MM is understood only by g++, or at
# not by cl

#$(addsuffix .dd,$(FILES)) : cmdnames.hpp cmdinst.hpp

#ifndef NODEPENDS
#include $(addsuffix .dd,$(FILES))
#endif

