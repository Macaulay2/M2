############################## includes
TOPDIR = ../..
include $(TOPDIR)/Makeconf
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
PROJECT += interp.d
PROJECT += mp.d
# PROJECT += actorsX.d
PROJECT += actors5.d actors4.d actors3.d actors2.d actors.d
PROJECT += objects.d
PROJECT += struct.d
PROJECT += GC.d
PROJECT += convertr.d basic.d binding.d
PROJECT += parser.d lex.d tokens.d
PROJECT += arith.d
PROJECT += gmp.d
PROJECT += err.d
PROJECT += stdiop.d
PROJECT += ctype.d
PROJECT += stdio.d
PROJECT += nets.d
PROJECT += varstrin.d
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

SCCFLAGS = -O

ifneq "$(CC)" "gcc"
SCCFLAGS += -nogcc
endif

%.dep : %.d
	$(SCC1) -dep -J. $*.d
	mv $*.dp $*.dep
	../util/update $*.sg $*.sig
%.c   : %.d
	$(SCC1) $(SCCFLAGS) +gc -J. -noline $<
%.oo  : %.d
	$(SCC1) $(SCCFLAGS) +gc -J. $<
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $*.c $(OUTPUT_OPTION)
	rm $*.c
%     : %.oo;	$(CC) $< $(LDFLAGS) $(LDLIBS) $(OUTPUT_OPTION)
%.o   : %.c;	$(CC) $< -c $(CPPFLAGS) $(CFLAGS) $(WARNINGS) $(OUTPUT_OPTION)
############################## flags

PURIFYCMD :=
# PURIFYCMD := purify -always-use-cache-dir

CPPFLAGS := -I$(INCDIR) -I. -DGaCo=1
# the purpose of the -I. is so scclib.c can find ./alloca.h if it's missing
# from the gcc installation, as it often is

ifeq "$(CC)" "cl"
WARNINGS := -W3
else
WARNINGS := -Wall -Wshadow -Wcast-qual
endif

# we can't do this because our manufactured c files have lots of unused labels
# CFLAGS  += -Wall -Wshadow -Wcast-qual

ifeq "$(CC)" "cl"
CFLAGS += -Za -W0
endif

CXXFLAGS += $(WARNINGS)
# LDFLAGS  += $(STRIPFLAG)

ifneq "$(CC)" "cl"
LDFLAGS  += -L${LIBDIR}
endif

ifeq ($(OS),Linux)
## - use this to get a memory map listing from the gnu linker
LDFLAGS  += -Wl,-Map,mapfile
endif

ifeq ($(OS),SunOS)
## - use this to get a memory map listing from the linker
##   if using Sun's ld.  Sigh, it comes to stdout.
# LDFLAGS  += -Wl,-m
endif

#################################

ifdef includeX11
CPPFLAGS += -DincludeX11
LDLIBS += -lX11
endif

ifdef SOCKS
LDLIBS += -lsocks5
endif

## we use one of these in scclib.c
# libdbm2.a is our own database manager
# libgdbm.a is the gnu database manager
# libndbm.a is the new database manager

# hopefully, this will prevent us from getting the buggy version of 
# __underflow
# LDLIBS+= -lc

ifndef NOSTATIC
ifneq "$(CC)" "cl"
LDFLAGS += -static
endif
endif

ifeq ($(OS),Linux)
LDFLAGS += -Wl,-defsym,_DYNAMIC=0
endif

#ifeq ($(OS),Linux)
#LDLIBS += -lieee
#endif

# -lsunmath makes suns obey ieee for floating point operations, at
# -liberty is /usr/local/lib/libiberty.a, and it has random() in it
# least under solaris
ifeq ($(OS) $(REL),SunOS 5.4)
LDLIBS += -lsunmath -liberty
endif

ifeq ($(OS),MS-DOS)
LDLIBS += -lgpp
else

ifneq "$(CC)" "cl"
LDLIBS += -lstdc++
endif

#	We used to -lc in here, but it's for a good reason.
#	Under linux, I have a modern version 2.8.0 of libstdc++ that defines
#	the routine _IO_init which is called by sprintf, indirectly.  My sprintf
#	comes from the old libc, and the _IO_init is incompatible, for some reason.

# 	We used to have -lg++ here, too, (before -lc), but it's obsolete now, and 
#	has been incorporated into libstdc++ with version 2.8.1.1.
endif

# but on some machines, with non-gnu ld being used, libiostream is
# not used automatically, so put it in anyway.
# LDLIBS += -liostream

############################## compiling

ifeq "$(CC)" "cl"
compat.c : ../msdos/compat.c; cp $< $@
compat.h : ../msdos/compat.h; cp $< $@
else
compat.c compat.h : configure; ./configure
endif

M2lib.o scclib.o : ../c/compat.h ../c/compat.c types.h ../../Makeconf.h compat.h memdebug.h
memdebug.o scclib.o actors5.oo gc_cpp.o : memdebug.h
gc_cpp.o : ../../Makeconf.h

allc : $(PROJECT:.d=.c) tmp_init.c

ALLOBJ := $(PROJECT:.d=.oo) M2lib.o scclib.o gc_cpp.o tmp_init.o memdebug.o ../e/*.o

ifneq "$(CC)" "cl"
ALLOBJ += compat.o
endif

ifeq ($(OS),Linux)
# The maintainers of linux' libstdc++ unwisely decided to incorporate
# their own private versions of routines from libc in their library!
# Unforgiveable!
ALLOBJ += putc.o
putc.o : /usr/lib/libc.a; ar x $^ $@
endif

################################# c file production for porting
ALLC := $(PROJECT:.d=.c)
all-c-files :: $(ALLC)
remove-c-files ::; rm -rf $(ALLC)
c-files.tar :: $(ALLC)
	tar cf $@ $(ALLC)
##############################

# UTIL := $(PATHPARENT)util$(PATHSEP)
UTIL := ../util/

tmp_init.o : tmp_init.c
tmp_init.c : Makefile $(TOPDIR)/Makeconf $(PROJECT)
	 $(UTIL)timestmp >tmp
	@ echo "echoout '>>tmp' ..."
	@ $(UTIL)echoout '>>tmp' $(foreach f, $(PROJECT:.d=), 'void $(f)__prepare();') 
	 $(UTIL)echoout '>>tmp' 'char current_date[] = __DATE__;'
	 $(UTIL)echoout '>>tmp' 'char current_time[] = __TIME__;'
	 $(UTIL)echoout '>>tmp' 'int main_inits() {'
	@ echo "echoout '>>tmp' ..."
	@ $(UTIL)echoout '>>tmp' $(foreach f, $(PROJECT:.d=), '   $(f)__prepare();')
	 $(UTIL)echoout '>>tmp' '   return 0;}'
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
	$(CC) -static -I$(INCDIR) probe.c $(OUTPUT_OPTION)
test-probe : probe
	nm probe |grep -v "gcc2_compiled\|gnu_compiled\0| \." >syms
	./probe a b c d >> syms
	sort syms > addresses
	rm syms
############################## miscellaneous



ifeq "$(CC)" "cl"
LIBRARIES += ../dbm/dbm2.lib
LIBRARIES += ../../lib/gc.lib
LIBRARIES += ../../lib/gmp.lib
LIBRARIES += ../../lib/libfac.lib
LIBRARIES += ../../lib/cf.lib
else
LIBRARIES += ../dbm/libdbm2.a
ifdef MP
LIBRARIES += ../../lib/libMP.a
endif
LIBRARIES += ../../lib/libfac.a
LIBRARIES += ../../lib/libcf.a
LIBRARIES += ../../lib/libmpf.a
LIBRARIES += ../../lib/libmpz.a
LIBRARIES += ../../lib/libmpn.a
LIBRARIES += ../../lib/libmpq.a
LIBRARIES += ../../lib/libgmp.a
LIBRARIES += ../../lib/libgc.a
endif

LDLIBS := $(LIBRARIES) $(LDLIBS)

ifeq "$(CC)" "cl"
LINK_OUTPUT_OPTION = -link -out:$@.exe
else
LINK_OUTPUT_OPTION = $(OUTPUT_OPTION)
endif

ifneq "$(CC)" "cl"
LDLIBS += -lm
endif

../bin/Macaulay2 : $(ALLOBJ) $(LIBRARIES)
	rm -f $@
	@ echo 'linking $@ with $(LDFLAGS) $(LDLIBS)'
	time $(PURIFYCMD) $(CC) $(LDFLAGS) $(ALLOBJ) $(LDLIBS) $(LINK_OUTPUT_OPTION)
ifndef CYGWIN32
	$(STRIPCMD) $@
endif

t_main.o : types.h
t : t_main.o gmp.oo stdio.oo strings.oo system.oo varstrin.oo nets.oo scclib.o C.oo t.oo
	$(CC) $(LDFLAGS) $^ $(LDLIBS) $(OUTPUT_OPTION)

all:: TAGS

TAGS: Makefile
	@ echo making TAGS
	@ $(UTIL)echoout -r2 '>TAGS' $(foreach i, $(SRCFILES),  $(i),0)
allfiles: Makefile
	@ echo making allfiles
	@ $(UTIL)echoout '>allfiles.tmp' $(ALLFILES)
	@ <allfiles.tmp sort|uniq >allfiles
	@ rm allfiles.tmp

backup : CVS/Entries
CVS/Entries : $(ALLFILES)
	mount /a.ext2
	tar cfv - $? | (cd /a.ext2; mkdir -p d; cd d; tar xf -)
	umount /a.ext2

wc:
	wc -l $(WC1FILES)
clean :
	rm -f *.log *.sym *.out *.o *.a *.oo *.sig *.dep \
		$(DTESTS:.d=) *_inits.c *.sg *.sgn \
		$(DNAMES:.d=.c) allfiles TAGS \
		core core.* compat.c compat.h c-files.tar tmp_init.c
