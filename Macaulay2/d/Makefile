############################## includes
include ../../Makeconf
############################## main target
all :: compat.h ../bin/Macaulay2
############################## useful targets
all-c-files ::
remove-c-files ::
##############################
.PHONY : all-c-files remove-c-files
############################## files
# the names on these lines should be in sequence from most dependent to
# least dependent so that "make" can remake the *.dep makefiles

PROJECT := 
PROJECT += interpret.d
ifdef MP
PROJECT += mp.d
endif
ifdef includeX11
PROJECT += actorsX.d
endif
PROJECT += actors5.d actors4.d actors3.d actors2.d actors.d
PROJECT += objects.d
PROJECT += structure.d
PROJECT += GC.d
PROJECT += converter.d basic.d binding.d
PROJECT += parser.d lex.d tokens.d
PROJECT += arithmetic.d
PROJECT += err.d
PROJECT += stdiop.d
PROJECT += ctype.d
PROJECT += stdio.d
PROJECT += nets.d
PROJECT += varstrings.d
PROJECT += strings.d 
PROJECT += X.d
PROJECT += GB.d
PROJECT += system.d
PROJECT += C.d
##############################
ifndef NODEPENDS
include $(PROJECT:.d=.dep)
endif
##############################
DNAMES := $(PROJECT)
############################## old files
DNAMES += t.d u.d optim.d actorsX.d
DNAMES += gr.d grx.d
DNAMES += testall.d pp.d redblk.d
############################## more files
WRAPPERS := scclib.c getpagesize.h gc_cpp.cc memdebug.c memdebug.h
#############################################################################
SRCFILES := $(WRAPPERS) $(DNAMES)
SCRIPTS := reverse
WC1FILES := $(SCRIPTS) $(SRCFILES) \
	Makefile abc grxwrap.c g scc.gdb README COPYRIGHT \
	bench sets.m bignum.h bignum.c alloca.h \
	configure remake probe.c sizes.c mp.d
ALLFILES := $(WC1FILES) $(PROJECT:.d=.dep) $(PROJECT:.d=.sig)
############################## rules
SCC1 := ../c/scc1
.SUFFIXES: .d .oo .sig .dep .res .test .m2
.PHONY : clean all tests
%.dep : %.d
	$(SCC1) -nogcc -dep -J. $*.d
	mv $*.dp $*.dep
	../util/update $*.sg $*.sig
interpret.dep : interpret.d
structure.dep : structure.d
varstrings.dep : varstrings.d
arithmetic.dep : arithmetic.d
converter.dep : converter.d
%.c   : %.d
	$(SCC1) -O +gc -J. -noline -nogcc $<
%.oo  : %.d
	$(SCC1) -O +gc -J. -nogcc $<
	$(CC) -o $*.oo $(CPPFLAGS) $(CCFLAGS) -c $*.c
	rm $*.c
%     : %.oo;	$(CC) -o $@ $< $(LDFLAGS) $(LOADLIBES)
%.o   : %.c;	$(CC) -o $@ $< -c $(CPPFLAGS) $(CFLAGS)
############################## flags
CC       := gcc

DEBUGFLAGS := 
# DEBUGFLAGS := -DMEM_DEBUG

STRIPFLAG :=
STRIPFLAG := -s

STRIPCMD := :
STRIPCMD := strip

PURIFYCMD :=
# PURIFYCMD := purify -always-use-cache-dir

CPPFLAGS := -I$(INCDIR) -I. -DGaCo=1 $(DEBUGFLAGS)
# the purpose of the -I. is so scclib.c can find ./alloca.h if it's missing
# from the gcc installation, as it often is

WARNINGS := -Wall -Wshadow -Wcast-qual
CCFLAGS  := -O3 -g
CFLAGS   := $(CCFLAGS) $(WARNINGS)
CXXFLAGS := $(CFLAGS)
LOADLIBES:= 
LDFLAGS  := -L${LIBDIR} $(STRIPFLAG) #-pg
#################################

ifdef includeX11
CPPFLAGS += -DincludeX11
LOADLIBES += -lX11
endif

ifdef MP
CPPFLAGS += -DMP
endif

## we use one of these in scclib.c
# libdbm2.a is our own database manager
# libgdbm.a is the gnu database manager
# libndbm.a is the new database manager
LOADLIBES += ../dbm/libdbm2.a

# hopefully, this will prevent us from getting the buggy version of 
# __underflow
# LOADLIBES += -lc

# ifneq ($(OS),Linux)
LDFLAGS += -static
# endif

#ifeq ($(OS),Linux)
#LOADLIBES += -lieee
#endif

# Messollen's multivariate factoring stuff
ifdef FACTOR
CPPFLAGS += -DFACTOR
endif

# -lsunmath makes suns obey ieee for floating point operations, at
# -liberty is /usr/local/lib/libiberty.a, and it has random() in it
# least under solaris
ifeq ($(OS) $(REL),SunOS 5.4)
LOADLIBES += -lsunmath -liberty
endif

ifeq ($(OS),MS-DOS)
LOADLIBES += -lgpp
else
LOADLIBES += -lg++ -lstdc++
endif

# but on some machines, with non-gnu ld being used, libiostream is
# not used automatically, so put it in anyway.
# LOADLIBES += -liostream

LOADLIBES += -lm

############################## compiling

ifeq "$(OS)" "MS-DOS"
compat.c : ../msdos/compat.c; cp $< $@
compat.h : ../msdos/compat.h; cp $< $@
else
compat.c compat.h : configure; ./configure
endif

scclib.o : ../c/compat.h ../c/compat.c ../../Makeconf.h
memdebug.o scclib.o actors5.oo gc_cpp.o : memdebug.h
allc : $(PROJECT:.d=.c) tmp_init.c
ALLOBJ := $(PROJECT:.d=.oo) scclib.o compat.o gc_cpp.o tmp_init.o memdebug.o

################################# c file production for porting
ALLC := $(PROJECT:.d=.c)
all-c-files :: $(ALLC)
remove-c-files ::; rm -rf $(ALLC)
##############################
tmp_init.c : Makefile ../util/timestmp
	timestmp >>tmp
	@echo "echoout '>>tmp' ..."
	@echoout '>>tmp' $(foreach f, $(PROJECT:.d=), 'void $(f)__prepare();') 
	echoout '>>tmp' 'int main_inits() {'
	@echo "echoout '>>tmp' ..."
	@echoout '>>tmp' $(foreach f, $(PROJECT:.d=), '   $(f)__prepare();')
	echoout '>>tmp' '   return 0;}'
	mv tmp tmp_init.c

.._c_compat.c: ../c/compat.c; cp $^ $@
.._c_compat.h: ../c/compat.h; cp $^ $@

c-port: $(ALLC) tmp_init.c gc_cpp.cc scclib.c memdebug.c memdebug.h compat.c compat.h \
		.._c_compat.c .._c_compat.h
	tar cfz /tmp/c-port.tgz $^

interpret.a : $(ALLOBJ)
	ar rcs $@ $^ tmp_init.o
############################## probe memory for dumpdata
probe : probe.c
	$(CC) -static -I$(INCDIR) -g -o probe probe.c
test-probe : probe
	nm probe |grep -v "gcc2_compiled\|gnu_compiled\0| \." >syms
	probe a b c d >> syms
	sort syms > addresses
	rm syms
############################## miscellaneous

ifdef MP
LIBMP :=	../../lib/libMP.a
endif

../bin/Macaulay2 : $(ALLOBJ) ../e/*.o tmp_init.o \
		../../lib/libgc.a \
		../../lib/libgmp.a \
		$(LIBMP) \
		../../lib/libfac.a \
		../../lib/libcf.a ../../lib/libcfmem.a \
		../../lib/libmpf.a \
		../../lib/libmpz.a \
		../../lib/libmpn.a \
		../../lib/libmpq.a
	rm -f $@
	@ echo 'linking $@ with $(LDFLAGS) $(LOADLIBES)'
	@ time $(PURIFYCMD) $(CC) -o $@ $(LDFLAGS) $^ $(LOADLIBES)
	$(STRIPCMD) $@
all:: TAGS
TAGS: Makefile
	@echo making TAGS
	@echoout -r2 '>TAGS' $(foreach i, $(SRCFILES),  $(i),0)
allfiles: Makefile
	@echo making allfiles
	@echoout '>allfiles.tmp' $(ALLFILES)
	@<allfiles.tmp sort|uniq >allfiles
	@rm allfiles.tmp
wc:
	wc -l $(WC1FILES)
clean :
	rm -f *.log *.sym *.out *.o *.a *.oo *.sig *.dep \
		$(DTESTS:.d=) *_inits.c *.sg *.sgn \
		$(DNAMES:.d=.c) allfiles TAGS \
		core core.*
