#ARING = true
TOPDIR = ../..
include $(TOPDIR)/Makeconf

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

CPPFLAGS += -I$(INCDIR) -DNDEBUG

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
CXXFLAGS += -O3 
CXXFLAGS += -g $(WARNINGS)
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

FILES := $(ESTUFF) $(CONTAINER) $(INTERFACE) $(COMMANDS) $(NAMES)
OFILES := $(addsuffix .o,$(FILES)) $(addsuffix .o,$(C_FILES))

CCFILES := $(addsuffix .cpp,$(FILES))

CFILES := $(addsuffix .c,$(C_FILES))

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
all:: cmdnames.m2 $(addsuffix .hpp, $(GENERATED_H)) $(OFILES)

%.ii: %.cpp
	$(PRE.cc) $< -o $@
%.s: %.cpp
	$(CXX) $(CXXFLAGS) -S $< -o $@


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
	rm -f *.o *.dd *.ii $(LIB) allfiles cmdinst.hpp cmdnames.hpp cmdnames.m2
	rm -f TAGS

depend:
	makedepend -Y -I../../include -- $(CFLAGS) -- $(CCFILES)

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

# DO NOT DELETE

EZZp.o: EZZp.hpp Edefs.hpp object.hpp style.hpp ../../include/gmp.h
EZZp.o: classes.hpp error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp
EZZp.o: obj_ptr.hpp
Emonorder.o: Emonorder.hpp Edefs.hpp object.hpp style.hpp ../../include/gmp.h
Emonorder.o: classes.hpp error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp
Emonorder.o: obj_ptr.hpp
Emonoid.o: Emonoid.hpp Edefs.hpp object.hpp style.hpp ../../include/gmp.h
Emonoid.o: classes.hpp error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp
Emonoid.o: obj_ptr.hpp Emonorder.hpp Emontable.hpp text_io.hpp random.hpp
Epoly.o: Epoly.hpp Edefs.hpp object.hpp style.hpp ../../include/gmp.h
Epoly.o: classes.hpp error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp
Epoly.o: obj_ptr.hpp EZZp.hpp
Ering.o: Ering.hpp EZZp.hpp Edefs.hpp object.hpp style.hpp
Ering.o: ../../include/gmp.h classes.hpp error.hpp buffer.hpp mem.hpp
Ering.o: array.hpp intarray.hpp obj_ptr.hpp Emonoid.hpp Emonorder.hpp
Ering.o: Efreemod.hpp Evector.hpp Ematrix.hpp Ehashtab.hpp text_io.hpp
Evector.o: Evector.hpp Ering.hpp EZZp.hpp Edefs.hpp object.hpp style.hpp
Evector.o: ../../include/gmp.h classes.hpp error.hpp buffer.hpp mem.hpp
Evector.o: array.hpp intarray.hpp obj_ptr.hpp Emonoid.hpp Emonorder.hpp
Evector.o: Efreemod.hpp text_io.hpp
Efreemod.o: Efreemod.hpp Edefs.hpp object.hpp style.hpp ../../include/gmp.h
Efreemod.o: classes.hpp error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp
Efreemod.o: obj_ptr.hpp Emonoid.hpp Emonorder.hpp EZZp.hpp Ering.hpp
Efreemod.o: Evector.hpp random.hpp comb.hpp Ematrix.hpp
Ematrix.o: Ematrix.hpp Efreemod.hpp Edefs.hpp object.hpp style.hpp
Ematrix.o: ../../include/gmp.h classes.hpp error.hpp buffer.hpp mem.hpp
Ematrix.o: array.hpp intarray.hpp obj_ptr.hpp Emonoid.hpp Emonorder.hpp
Ematrix.o: EZZp.hpp Ering.hpp Evector.hpp text_io.hpp comb.hpp
Eringmap.o: Eringmap.hpp Evector.hpp Ering.hpp EZZp.hpp Edefs.hpp object.hpp
Eringmap.o: style.hpp ../../include/gmp.h classes.hpp error.hpp buffer.hpp
Eringmap.o: mem.hpp array.hpp intarray.hpp obj_ptr.hpp Emonoid.hpp
Eringmap.o: Emonorder.hpp Efreemod.hpp
Eio.o: Eio.hpp EZZp.hpp Edefs.hpp object.hpp style.hpp ../../include/gmp.h
Eio.o: classes.hpp error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp
Eio.o: obj_ptr.hpp Emonorder.hpp Emonoid.hpp Ering.hpp Efreemod.hpp
Eio.o: Evector.hpp
Ecommands.o: Eio.hpp EZZp.hpp Edefs.hpp object.hpp style.hpp
Ecommands.o: ../../include/gmp.h classes.hpp error.hpp buffer.hpp mem.hpp
Ecommands.o: array.hpp intarray.hpp obj_ptr.hpp Emonorder.hpp Emonoid.hpp
Ecommands.o: Ering.hpp Efreemod.hpp Evector.hpp Ematrix.hpp interp.hpp
Ecommands.o: stack.hpp obj_int.hpp bin_io.hpp text_io.hpp obj_str.hpp
Ecommands.o: obj_iarr.hpp cmdnames.hpp Ehashtab.hpp
array.o: array.hpp style.hpp ../../include/gmp.h classes.hpp error.hpp
array.o: buffer.hpp mem.hpp intarray.hpp object.hpp obj_ptr.hpp queue.hpp
array.o: int_bag.hpp varpower.hpp respoly.hpp monideal.hpp index.hpp ring.hpp
array.o: ringelem.hpp monoid.hpp monorder.hpp Z.hpp res.hpp matrix.hpp
array.o: freemod.hpp vector.hpp relem.hpp polyring.hpp comp.hpp hermite.hpp
array.o: gb_comp.hpp gb.hpp spair.hpp gbZZ.hpp newspair.hpp termideal.hpp
array.o: respoly2.hpp respair2.hpp res2.hpp gbbinom.hpp
hashtab.o: hashtab.hpp style.hpp ../../include/gmp.h classes.hpp error.hpp
hashtab.o: buffer.hpp mem.hpp
queue.o: queue.hpp style.hpp ../../include/gmp.h classes.hpp error.hpp
queue.o: buffer.hpp mem.hpp object.hpp array.hpp intarray.hpp obj_ptr.hpp
queue.o: int_bag.hpp varpower.hpp termideal.hpp ring.hpp ringelem.hpp
queue.o: monoid.hpp monorder.hpp Z.hpp newspair.hpp freemod.hpp monideal.hpp
queue.o: index.hpp polyring.hpp
stack.o: stack.hpp array.hpp style.hpp ../../include/gmp.h classes.hpp
stack.o: error.hpp buffer.hpp mem.hpp object.hpp intarray.hpp obj_ptr.hpp
buffer.o: buffer.hpp
bin_io.o: bin_io.hpp style.hpp ../../include/gmp.h classes.hpp error.hpp
bin_io.o: buffer.hpp mem.hpp interp.hpp stack.hpp array.hpp obj_int.hpp
bin_io.o: object.hpp intarray.hpp obj_ptr.hpp text_io.hpp obj_str.hpp
bin_io.o: obj_iarr.hpp cmdnames.hpp
handles.o: handles.hpp object.hpp style.hpp ../../include/gmp.h classes.hpp
handles.o: error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp
handles.o: hashtab.hpp interp.hpp stack.hpp obj_int.hpp bin_io.hpp
handles.o: text_io.hpp obj_str.hpp obj_iarr.hpp cmdnames.hpp
interp.o: interp.hpp stack.hpp array.hpp style.hpp ../../include/gmp.h
interp.o: classes.hpp error.hpp buffer.hpp mem.hpp obj_int.hpp object.hpp
interp.o: intarray.hpp obj_ptr.hpp bin_io.hpp text_io.hpp obj_str.hpp
interp.o: obj_iarr.hpp cmdnames.hpp handles.hpp hashtab.hpp random.hpp
object.o: object.hpp style.hpp ../../include/gmp.h classes.hpp error.hpp
object.o: buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp monomial.hpp
object.o: varpower.hpp respoly.hpp monideal.hpp queue.hpp index.hpp
object.o: int_bag.hpp ring.hpp ringelem.hpp monoid.hpp monorder.hpp Z.hpp
object.o: res.hpp matrix.hpp freemod.hpp vector.hpp relem.hpp polyring.hpp
object.o: comp.hpp termideal.hpp newspair.hpp ringmap.hpp hermite.hpp
object.o: gb_comp.hpp gauss.hpp hilb.hpp gb.hpp spair.hpp gbinhom.hpp
object.o: gbbinom.hpp gbZZ.hpp sagbi.hpp gb2.hpp respoly2.hpp respair2.hpp
object.o: res2.hpp obj_int.hpp bin_io.hpp text_io.hpp obj_iarr.hpp
object.o: obj_str.hpp handles.hpp hashtab.hpp
obj_prim.o: obj_prim.hpp object.hpp style.hpp ../../include/gmp.h classes.hpp
obj_prim.o: error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp
obj_prim.o: stack.hpp interp.hpp obj_int.hpp bin_io.hpp text_io.hpp
obj_prim.o: obj_str.hpp obj_iarr.hpp cmdnames.hpp
text_io.o: text_io.hpp ../../include/gmp.h buffer.hpp
x_monoid.o: interp.hpp stack.hpp array.hpp style.hpp ../../include/gmp.h
x_monoid.o: classes.hpp error.hpp buffer.hpp mem.hpp obj_int.hpp object.hpp
x_monoid.o: intarray.hpp obj_ptr.hpp bin_io.hpp text_io.hpp obj_str.hpp
x_monoid.o: obj_iarr.hpp cmdnames.hpp monoid.hpp monorder.hpp newmonorder.hpp
x_monom.o: stack.hpp array.hpp style.hpp ../../include/gmp.h classes.hpp
x_monom.o: error.hpp buffer.hpp mem.hpp obj_int.hpp object.hpp intarray.hpp
x_monom.o: obj_ptr.hpp bin_io.hpp text_io.hpp obj_str.hpp obj_iarr.hpp
x_monom.o: interp.hpp cmdnames.hpp monomial.hpp varpower.hpp
x_system.o: mem.hpp style.hpp ../../include/gmp.h classes.hpp error.hpp
x_system.o: buffer.hpp interp.hpp stack.hpp array.hpp obj_int.hpp object.hpp
x_system.o: intarray.hpp obj_ptr.hpp bin_io.hpp text_io.hpp obj_str.hpp
x_system.o: obj_iarr.hpp cmdnames.hpp handles.hpp hashtab.hpp cmdinst.hpp
x_free.o: interp.hpp stack.hpp array.hpp style.hpp ../../include/gmp.h
x_free.o: classes.hpp error.hpp buffer.hpp mem.hpp obj_int.hpp object.hpp
x_free.o: intarray.hpp obj_ptr.hpp bin_io.hpp text_io.hpp obj_str.hpp
x_free.o: obj_iarr.hpp cmdnames.hpp freemod.hpp ring.hpp ringelem.hpp
x_free.o: monoid.hpp monorder.hpp Z.hpp monideal.hpp queue.hpp index.hpp
x_free.o: varpower.hpp int_bag.hpp vector.hpp relem.hpp matrix.hpp
x_mat.o: interp.hpp stack.hpp array.hpp style.hpp ../../include/gmp.h
x_mat.o: classes.hpp error.hpp buffer.hpp mem.hpp obj_int.hpp object.hpp
x_mat.o: intarray.hpp obj_ptr.hpp bin_io.hpp text_io.hpp obj_str.hpp
x_mat.o: obj_iarr.hpp cmdnames.hpp matrix.hpp monoid.hpp monorder.hpp
x_mat.o: freemod.hpp ring.hpp ringelem.hpp Z.hpp monideal.hpp queue.hpp
x_mat.o: index.hpp varpower.hpp int_bag.hpp vector.hpp relem.hpp det.hpp
x_mat.o: comp.hpp comb.hpp pfaff.hpp ringmap.hpp termideal.hpp newspair.hpp
x_mat.o: polyring.hpp monomial.hpp assprime.hpp random.hpp sparsemat.hpp
x_relem.o: interp.hpp stack.hpp array.hpp style.hpp ../../include/gmp.h
x_relem.o: classes.hpp error.hpp buffer.hpp mem.hpp obj_int.hpp object.hpp
x_relem.o: intarray.hpp obj_ptr.hpp bin_io.hpp text_io.hpp obj_str.hpp
x_relem.o: obj_iarr.hpp cmdnames.hpp monoid.hpp monorder.hpp monomial.hpp
x_relem.o: varpower.hpp relem.hpp ring.hpp ringelem.hpp Z.hpp z_mod_p.hpp
x_relem.o: GF.hpp polyring.hpp monideal.hpp queue.hpp index.hpp int_bag.hpp
x_relem.o: matrix.hpp freemod.hpp vector.hpp schur.hpp frac.hpp weylalg.hpp
x_gb.o: interp.hpp stack.hpp array.hpp style.hpp ../../include/gmp.h
x_gb.o: classes.hpp error.hpp buffer.hpp mem.hpp obj_int.hpp object.hpp
x_gb.o: intarray.hpp obj_ptr.hpp bin_io.hpp text_io.hpp obj_str.hpp
x_gb.o: obj_iarr.hpp cmdnames.hpp gb.hpp relem.hpp ring.hpp ringelem.hpp
x_gb.o: monoid.hpp monorder.hpp Z.hpp matrix.hpp freemod.hpp monideal.hpp
x_gb.o: queue.hpp index.hpp varpower.hpp int_bag.hpp vector.hpp polyring.hpp
x_gb.o: comp.hpp gb_comp.hpp spair.hpp gbinhom.hpp gbbinom.hpp gbZZ.hpp
x_gb.o: newspair.hpp termideal.hpp hilb.hpp hermite.hpp gauss.hpp gb2.hpp
x_gb.o: res.hpp respoly.hpp res2.hpp respoly2.hpp respair2.hpp ringmap.hpp
x_gb.o: sagbi.hpp lattice.hpp LLL.hpp sparsemat.hpp
freemod2.o: freemod.hpp ring.hpp ringelem.hpp ../../include/gmp.h monoid.hpp
freemod2.o: monorder.hpp object.hpp style.hpp classes.hpp error.hpp
freemod2.o: buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp Z.hpp
freemod2.o: monideal.hpp queue.hpp index.hpp varpower.hpp int_bag.hpp
freemod2.o: comb.hpp bin_io.hpp polyring.hpp serial.hpp
x_factor.o: interp.hpp stack.hpp array.hpp style.hpp ../../include/gmp.h
x_factor.o: classes.hpp error.hpp buffer.hpp mem.hpp obj_int.hpp object.hpp
x_factor.o: intarray.hpp obj_ptr.hpp bin_io.hpp text_io.hpp obj_str.hpp
x_factor.o: obj_iarr.hpp cmdnames.hpp matrix.hpp monoid.hpp monorder.hpp
x_factor.o: freemod.hpp ring.hpp ringelem.hpp Z.hpp monideal.hpp queue.hpp
x_factor.o: index.hpp varpower.hpp int_bag.hpp vector.hpp relem.hpp
x_factor.o: z_mod_p.hpp frac.hpp ../../include/factor.h
x_factor.o: ../../include/factory.h ../../include/factoryconf.h
x_factor.o: ../../include/templates/array.h ../../include/templates/factor.h
x_factor.o: ../../include/templates/list.h ../../include/templates/matrix.h
x_factor.o: ../../include/templates/functions.h
gbnod.o: style.hpp ../../include/gmp.h classes.hpp error.hpp buffer.hpp
gbnod.o: mem.hpp gb2.hpp object.hpp array.hpp intarray.hpp obj_ptr.hpp
gbnod.o: relem.hpp ring.hpp ringelem.hpp monoid.hpp monorder.hpp Z.hpp
gbnod.o: matrix.hpp freemod.hpp monideal.hpp queue.hpp index.hpp varpower.hpp
gbnod.o: int_bag.hpp vector.hpp polyring.hpp comp.hpp gb_comp.hpp spair.hpp
gbnod.o: hilb.hpp geovec.hpp text_io.hpp
newmonorder.o: newmonorder.hpp object.hpp style.hpp ../../include/gmp.h
newmonorder.o: classes.hpp error.hpp buffer.hpp mem.hpp array.hpp
newmonorder.o: intarray.hpp obj_ptr.hpp
serial.o: serial.hpp object.hpp style.hpp ../../include/gmp.h classes.hpp
serial.o: error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp
ntuple.o: ntuple.hpp style.hpp ../../include/gmp.h classes.hpp error.hpp
ntuple.o: buffer.hpp mem.hpp array.hpp text_io.hpp bin_io.hpp
res.o: respoly.hpp monideal.hpp queue.hpp style.hpp ../../include/gmp.h
res.o: classes.hpp error.hpp buffer.hpp mem.hpp index.hpp varpower.hpp
res.o: intarray.hpp array.hpp object.hpp obj_ptr.hpp int_bag.hpp ring.hpp
res.o: ringelem.hpp monoid.hpp monorder.hpp Z.hpp res.hpp matrix.hpp
res.o: freemod.hpp vector.hpp relem.hpp polyring.hpp comp.hpp text_io.hpp
res.o: res_aux.cpp interp.hpp stack.hpp obj_int.hpp bin_io.hpp obj_str.hpp
res.o: obj_iarr.hpp cmdnames.hpp
respoly.o: respoly.hpp monideal.hpp queue.hpp style.hpp ../../include/gmp.h
respoly.o: classes.hpp error.hpp buffer.hpp mem.hpp index.hpp varpower.hpp
respoly.o: intarray.hpp array.hpp object.hpp obj_ptr.hpp int_bag.hpp ring.hpp
respoly.o: ringelem.hpp monoid.hpp monorder.hpp Z.hpp text_io.hpp
respoly.o: polyring.hpp freemod.hpp
hilb.o: hilb.hpp comp.hpp object.hpp style.hpp ../../include/gmp.h
hilb.o: classes.hpp error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp
hilb.o: obj_ptr.hpp monideal.hpp queue.hpp index.hpp varpower.hpp int_bag.hpp
hilb.o: ring.hpp ringelem.hpp monoid.hpp monorder.hpp Z.hpp matrix.hpp
hilb.o: freemod.hpp vector.hpp relem.hpp polyring.hpp
frac.o: frac.hpp ring.hpp ringelem.hpp ../../include/gmp.h monoid.hpp
frac.o: monorder.hpp object.hpp style.hpp classes.hpp error.hpp buffer.hpp
frac.o: mem.hpp array.hpp intarray.hpp obj_ptr.hpp Z.hpp text_io.hpp
frac.o: bin_io.hpp ringmap.hpp serial.hpp
polyring.o: polyring.hpp ring.hpp ringelem.hpp ../../include/gmp.h monoid.hpp
polyring.o: monorder.hpp object.hpp style.hpp classes.hpp error.hpp
polyring.o: buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp Z.hpp
polyring.o: monideal.hpp queue.hpp index.hpp varpower.hpp int_bag.hpp
polyring.o: text_io.hpp bin_io.hpp ringmap.hpp matrix.hpp freemod.hpp
polyring.o: vector.hpp relem.hpp ntuple.hpp termideal.hpp newspair.hpp
polyring.o: geopoly.hpp serial.hpp
weylalg.o: weylalg.hpp polyring.hpp ring.hpp ringelem.hpp ../../include/gmp.h
weylalg.o: monoid.hpp monorder.hpp object.hpp style.hpp classes.hpp error.hpp
weylalg.o: buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp Z.hpp
weylalg.o: monideal.hpp queue.hpp index.hpp varpower.hpp int_bag.hpp
weylalg.o: geopoly.hpp text_io.hpp weylfree.hpp freemod.hpp
freemod.o: freemod.hpp ring.hpp ringelem.hpp ../../include/gmp.h monoid.hpp
freemod.o: monorder.hpp object.hpp style.hpp classes.hpp error.hpp buffer.hpp
freemod.o: mem.hpp array.hpp intarray.hpp obj_ptr.hpp Z.hpp monideal.hpp
freemod.o: queue.hpp index.hpp varpower.hpp int_bag.hpp comb.hpp text_io.hpp
freemod.o: bin_io.hpp matrix.hpp vector.hpp relem.hpp polyring.hpp
freemod.o: ringmap.hpp ntuple.hpp termideal.hpp newspair.hpp geovec.hpp
weylfree.o: weylfree.hpp weylalg.hpp polyring.hpp ring.hpp ringelem.hpp
weylfree.o: ../../include/gmp.h monoid.hpp monorder.hpp object.hpp style.hpp
weylfree.o: classes.hpp error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp
weylfree.o: obj_ptr.hpp Z.hpp monideal.hpp queue.hpp index.hpp varpower.hpp
weylfree.o: int_bag.hpp freemod.hpp geovec.hpp
matrix.o: style.hpp ../../include/gmp.h classes.hpp error.hpp buffer.hpp
matrix.o: mem.hpp text_io.hpp bin_io.hpp matrix.hpp monoid.hpp monorder.hpp
matrix.o: object.hpp array.hpp intarray.hpp obj_ptr.hpp freemod.hpp ring.hpp
matrix.o: ringelem.hpp Z.hpp monideal.hpp queue.hpp index.hpp varpower.hpp
matrix.o: int_bag.hpp vector.hpp relem.hpp comb.hpp det.hpp comp.hpp
matrix.o: termideal.hpp newspair.hpp polyring.hpp
sparsemat.o: sparsemat.hpp ring.hpp ringelem.hpp ../../include/gmp.h
sparsemat.o: monoid.hpp monorder.hpp object.hpp style.hpp classes.hpp
sparsemat.o: error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp
sparsemat.o: Z.hpp matrix.hpp freemod.hpp monideal.hpp queue.hpp index.hpp
sparsemat.o: varpower.hpp int_bag.hpp vector.hpp relem.hpp text_io.hpp
LLL.o: LLL.hpp sparsemat.hpp ring.hpp ringelem.hpp ../../include/gmp.h
LLL.o: monoid.hpp monorder.hpp object.hpp style.hpp classes.hpp error.hpp
LLL.o: buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp Z.hpp comp.hpp
LLL.o: relem.hpp matrix.hpp freemod.hpp monideal.hpp queue.hpp index.hpp
LLL.o: varpower.hpp int_bag.hpp vector.hpp frac.hpp text_io.hpp
relem.o: relem.hpp ring.hpp ringelem.hpp ../../include/gmp.h monoid.hpp
relem.o: monorder.hpp object.hpp style.hpp classes.hpp error.hpp buffer.hpp
relem.o: mem.hpp array.hpp intarray.hpp obj_ptr.hpp Z.hpp monomial.hpp
relem.o: varpower.hpp frac.hpp
ring.o: ring.hpp ringelem.hpp ../../include/gmp.h monoid.hpp monorder.hpp
ring.o: object.hpp style.hpp classes.hpp error.hpp buffer.hpp mem.hpp
ring.o: array.hpp intarray.hpp obj_ptr.hpp Z.hpp monideal.hpp queue.hpp
ring.o: index.hpp varpower.hpp int_bag.hpp respoly.hpp polyring.hpp
ring.o: freemod.hpp
vector.o: vector.hpp relem.hpp ring.hpp ringelem.hpp ../../include/gmp.h
vector.o: monoid.hpp monorder.hpp object.hpp style.hpp classes.hpp error.hpp
vector.o: buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp Z.hpp
vector.o: freemod.hpp monideal.hpp queue.hpp index.hpp varpower.hpp
vector.o: int_bag.hpp
z_mod_p.o: z_mod_p.hpp ring.hpp ringelem.hpp ../../include/gmp.h monoid.hpp
z_mod_p.o: monorder.hpp object.hpp style.hpp classes.hpp error.hpp buffer.hpp
z_mod_p.o: mem.hpp array.hpp intarray.hpp obj_ptr.hpp Z.hpp text_io.hpp
z_mod_p.o: bin_io.hpp ringmap.hpp random.hpp serial.hpp
Z.o: Z.hpp ring.hpp ringelem.hpp ../../include/gmp.h monoid.hpp monorder.hpp
Z.o: object.hpp style.hpp classes.hpp error.hpp buffer.hpp mem.hpp array.hpp
Z.o: intarray.hpp obj_ptr.hpp text_io.hpp bin_io.hpp relem.hpp ringmap.hpp
Z.o: random.hpp serial.hpp
GF.o: Z.hpp ring.hpp ringelem.hpp ../../include/gmp.h monoid.hpp monorder.hpp
GF.o: object.hpp style.hpp classes.hpp error.hpp buffer.hpp mem.hpp array.hpp
GF.o: intarray.hpp obj_ptr.hpp GF.hpp relem.hpp text_io.hpp bin_io.hpp
GF.o: ringmap.hpp polyring.hpp monideal.hpp queue.hpp index.hpp varpower.hpp
GF.o: int_bag.hpp random.hpp serial.hpp
schur.o: schur.hpp polyring.hpp ring.hpp ringelem.hpp ../../include/gmp.h
schur.o: monoid.hpp monorder.hpp object.hpp style.hpp classes.hpp error.hpp
schur.o: buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp Z.hpp
schur.o: monideal.hpp queue.hpp index.hpp varpower.hpp int_bag.hpp
schur.o: text_io.hpp
monideal.o: monideal.hpp queue.hpp style.hpp ../../include/gmp.h classes.hpp
monideal.o: error.hpp buffer.hpp mem.hpp index.hpp varpower.hpp intarray.hpp
monideal.o: array.hpp object.hpp obj_ptr.hpp int_bag.hpp ring.hpp
monideal.o: ringelem.hpp monoid.hpp monorder.hpp Z.hpp bin_io.hpp text_io.hpp
termideal.o: termideal.hpp queue.hpp style.hpp ../../include/gmp.h
termideal.o: classes.hpp error.hpp buffer.hpp mem.hpp ring.hpp ringelem.hpp
termideal.o: monoid.hpp monorder.hpp object.hpp array.hpp intarray.hpp
termideal.o: obj_ptr.hpp Z.hpp newspair.hpp freemod.hpp monideal.hpp
termideal.o: index.hpp varpower.hpp int_bag.hpp polyring.hpp matrix.hpp
termideal.o: vector.hpp relem.hpp text_io.hpp
newspair.o: newspair.hpp freemod.hpp ring.hpp ringelem.hpp
newspair.o: ../../include/gmp.h monoid.hpp monorder.hpp object.hpp style.hpp
newspair.o: classes.hpp error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp
newspair.o: obj_ptr.hpp Z.hpp monideal.hpp queue.hpp index.hpp varpower.hpp
newspair.o: int_bag.hpp polyring.hpp text_io.hpp
assprime.o: assprime.hpp monideal.hpp queue.hpp style.hpp ../../include/gmp.h
assprime.o: classes.hpp error.hpp buffer.hpp mem.hpp index.hpp varpower.hpp
assprime.o: intarray.hpp array.hpp object.hpp obj_ptr.hpp int_bag.hpp
assprime.o: ring.hpp ringelem.hpp monoid.hpp monorder.hpp Z.hpp
det.o: det.hpp comp.hpp object.hpp style.hpp ../../include/gmp.h classes.hpp
det.o: error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp
det.o: matrix.hpp monoid.hpp monorder.hpp freemod.hpp ring.hpp ringelem.hpp
det.o: Z.hpp monideal.hpp queue.hpp index.hpp varpower.hpp int_bag.hpp
det.o: vector.hpp relem.hpp comb.hpp text_io.hpp bin_io.hpp serial.hpp
pfaff.o: pfaff.hpp comp.hpp object.hpp style.hpp ../../include/gmp.h
pfaff.o: classes.hpp error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp
pfaff.o: obj_ptr.hpp matrix.hpp monoid.hpp monorder.hpp freemod.hpp ring.hpp
pfaff.o: ringelem.hpp Z.hpp monideal.hpp queue.hpp index.hpp varpower.hpp
pfaff.o: int_bag.hpp vector.hpp relem.hpp comb.hpp
ringmap.o: ringmap.hpp ring.hpp ringelem.hpp ../../include/gmp.h monoid.hpp
ringmap.o: monorder.hpp object.hpp style.hpp classes.hpp error.hpp buffer.hpp
ringmap.o: mem.hpp array.hpp intarray.hpp obj_ptr.hpp Z.hpp matrix.hpp
ringmap.o: freemod.hpp monideal.hpp queue.hpp index.hpp varpower.hpp
ringmap.o: int_bag.hpp vector.hpp relem.hpp bin_io.hpp
hermite.o: style.hpp ../../include/gmp.h classes.hpp error.hpp buffer.hpp
hermite.o: mem.hpp hermite.hpp object.hpp array.hpp intarray.hpp obj_ptr.hpp
hermite.o: relem.hpp ring.hpp ringelem.hpp monoid.hpp monorder.hpp Z.hpp
hermite.o: matrix.hpp freemod.hpp monideal.hpp queue.hpp index.hpp
hermite.o: varpower.hpp int_bag.hpp vector.hpp polyring.hpp comp.hpp
hermite.o: gb_comp.hpp text_io.hpp
lattice.o: comp.hpp object.hpp style.hpp ../../include/gmp.h classes.hpp
lattice.o: error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp
lattice.o: lattice.hpp matrix.hpp monoid.hpp monorder.hpp freemod.hpp
lattice.o: ring.hpp ringelem.hpp Z.hpp monideal.hpp queue.hpp index.hpp
lattice.o: varpower.hpp int_bag.hpp vector.hpp relem.hpp sparsemat.hpp
lattice.o: text_io.hpp
gauss.o: style.hpp ../../include/gmp.h classes.hpp error.hpp buffer.hpp
gauss.o: mem.hpp gauss.hpp object.hpp array.hpp intarray.hpp obj_ptr.hpp
gauss.o: relem.hpp ring.hpp ringelem.hpp monoid.hpp monorder.hpp Z.hpp
gauss.o: matrix.hpp freemod.hpp monideal.hpp queue.hpp index.hpp varpower.hpp
gauss.o: int_bag.hpp vector.hpp polyring.hpp comp.hpp gb_comp.hpp text_io.hpp
comb.o: comb.hpp style.hpp ../../include/gmp.h classes.hpp error.hpp
comb.o: buffer.hpp mem.hpp array.hpp text_io.hpp
mem.o: style.hpp ../../include/gmp.h classes.hpp error.hpp buffer.hpp mem.hpp
mem.o: handles.hpp object.hpp array.hpp intarray.hpp obj_ptr.hpp hashtab.hpp
mem.o: text_io.hpp
int_bag.o: int_bag.hpp style.hpp ../../include/gmp.h classes.hpp error.hpp
int_bag.o: buffer.hpp mem.hpp varpower.hpp intarray.hpp array.hpp
intarray.o: intarray.hpp style.hpp ../../include/gmp.h classes.hpp error.hpp
intarray.o: buffer.hpp mem.hpp bin_io.hpp
monorder.o: monorder.hpp object.hpp style.hpp ../../include/gmp.h classes.hpp
monorder.o: error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp
monomial.o: monomial.hpp varpower.hpp intarray.hpp style.hpp
monomial.o: ../../include/gmp.h classes.hpp error.hpp buffer.hpp mem.hpp
monomial.o: array.hpp object.hpp obj_ptr.hpp
varpower.o: varpower.hpp intarray.hpp style.hpp ../../include/gmp.h
varpower.o: classes.hpp error.hpp buffer.hpp mem.hpp array.hpp bin_io.hpp
varpower.o: text_io.hpp
monoid.o: text_io.hpp ../../include/gmp.h buffer.hpp monoid.hpp monorder.hpp
monoid.o: object.hpp style.hpp classes.hpp error.hpp mem.hpp array.hpp
monoid.o: intarray.hpp obj_ptr.hpp varpower.hpp ntuple.hpp
gb.o: style.hpp ../../include/gmp.h classes.hpp error.hpp buffer.hpp mem.hpp
gb.o: gb.hpp object.hpp array.hpp intarray.hpp obj_ptr.hpp relem.hpp ring.hpp
gb.o: ringelem.hpp monoid.hpp monorder.hpp Z.hpp matrix.hpp freemod.hpp
gb.o: monideal.hpp queue.hpp index.hpp varpower.hpp int_bag.hpp vector.hpp
gb.o: polyring.hpp comp.hpp gb_comp.hpp spair.hpp hilb.hpp geovec.hpp
gb.o: text_io.hpp
spair.o: spair.hpp freemod.hpp ring.hpp ringelem.hpp ../../include/gmp.h
spair.o: monoid.hpp monorder.hpp object.hpp style.hpp classes.hpp error.hpp
spair.o: buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp Z.hpp
spair.o: monideal.hpp queue.hpp index.hpp varpower.hpp int_bag.hpp
spair.o: polyring.hpp
gbinhom.o: style.hpp ../../include/gmp.h classes.hpp error.hpp buffer.hpp
gbinhom.o: mem.hpp gbinhom.hpp object.hpp array.hpp intarray.hpp obj_ptr.hpp
gbinhom.o: relem.hpp ring.hpp ringelem.hpp monoid.hpp monorder.hpp Z.hpp
gbinhom.o: matrix.hpp freemod.hpp monideal.hpp queue.hpp index.hpp
gbinhom.o: varpower.hpp int_bag.hpp vector.hpp polyring.hpp comp.hpp
gbinhom.o: gb_comp.hpp spair.hpp geovec.hpp text_io.hpp
gbbinom.o: gbbinom.hpp comp.hpp object.hpp style.hpp ../../include/gmp.h
gbbinom.o: classes.hpp error.hpp buffer.hpp mem.hpp array.hpp intarray.hpp
gbbinom.o: obj_ptr.hpp gb_comp.hpp matrix.hpp monoid.hpp monorder.hpp
gbbinom.o: freemod.hpp ring.hpp ringelem.hpp Z.hpp monideal.hpp queue.hpp
gbbinom.o: index.hpp varpower.hpp int_bag.hpp vector.hpp relem.hpp ntuple.hpp
gbbinom.o: text_io.hpp
sagbi.o: sagbi.hpp matrix.hpp monoid.hpp monorder.hpp object.hpp style.hpp
sagbi.o: ../../include/gmp.h classes.hpp error.hpp buffer.hpp mem.hpp
sagbi.o: array.hpp intarray.hpp obj_ptr.hpp freemod.hpp ring.hpp ringelem.hpp
sagbi.o: Z.hpp monideal.hpp queue.hpp index.hpp varpower.hpp int_bag.hpp
sagbi.o: vector.hpp relem.hpp comp.hpp gb_comp.hpp
random.o: random.hpp Z.hpp ring.hpp ringelem.hpp ../../include/gmp.h
random.o: monoid.hpp monorder.hpp object.hpp style.hpp classes.hpp error.hpp
random.o: buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp relem.hpp
gbZZ.o: style.hpp ../../include/gmp.h classes.hpp error.hpp buffer.hpp
gbZZ.o: mem.hpp gbZZ.hpp object.hpp array.hpp intarray.hpp obj_ptr.hpp
gbZZ.o: relem.hpp ring.hpp ringelem.hpp monoid.hpp monorder.hpp Z.hpp
gbZZ.o: matrix.hpp freemod.hpp monideal.hpp queue.hpp index.hpp varpower.hpp
gbZZ.o: int_bag.hpp vector.hpp polyring.hpp comp.hpp gb_comp.hpp newspair.hpp
gbZZ.o: termideal.hpp geovec.hpp text_io.hpp
gb2.o: gb2.hpp object.hpp style.hpp ../../include/gmp.h classes.hpp error.hpp
gb2.o: buffer.hpp mem.hpp array.hpp intarray.hpp obj_ptr.hpp relem.hpp
gb2.o: ring.hpp ringelem.hpp monoid.hpp monorder.hpp Z.hpp matrix.hpp
gb2.o: freemod.hpp monideal.hpp queue.hpp index.hpp varpower.hpp int_bag.hpp
gb2.o: vector.hpp polyring.hpp comp.hpp gb_comp.hpp spair.hpp hilb.hpp
gb2.o: text_io.hpp interp.hpp stack.hpp obj_int.hpp bin_io.hpp obj_str.hpp
gb2.o: obj_iarr.hpp cmdnames.hpp
res2.o: respoly2.hpp monideal.hpp queue.hpp style.hpp ../../include/gmp.h
res2.o: classes.hpp error.hpp buffer.hpp mem.hpp index.hpp varpower.hpp
res2.o: intarray.hpp array.hpp object.hpp obj_ptr.hpp int_bag.hpp ring.hpp
res2.o: ringelem.hpp monoid.hpp monorder.hpp Z.hpp respair2.hpp res2.hpp
res2.o: matrix.hpp freemod.hpp vector.hpp relem.hpp polyring.hpp comp.hpp
res2.o: geores.hpp res_aux2.cpp interp.hpp stack.hpp obj_int.hpp bin_io.hpp
res2.o: text_io.hpp obj_str.hpp obj_iarr.hpp cmdnames.hpp
respoly2.o: respoly2.hpp monideal.hpp queue.hpp style.hpp ../../include/gmp.h
respoly2.o: classes.hpp error.hpp buffer.hpp mem.hpp index.hpp varpower.hpp
respoly2.o: intarray.hpp array.hpp object.hpp obj_ptr.hpp int_bag.hpp
respoly2.o: ring.hpp ringelem.hpp monoid.hpp monorder.hpp Z.hpp respair2.hpp
respoly2.o: text_io.hpp polyring.hpp freemod.hpp
