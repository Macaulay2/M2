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

# SCCFLAGS += -spincursor

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
%.loo  : %.d
	$(SCC1) $(SCCFLAGS) +gc -J. $<
	$(CC) $(CPPFLAGS) $(CFLAGS) -fPIC -c $*.c $(OUTPUT_OPTION)
	rm $*.c
%     : %.oo;	$(LINK.o)                $^ $(LOADLIBES) $(LDLIBS) $(OUTPUT_OPTION)
%.o   : %.c;	$(COMPILE.c) $(WARNINGS) $<                        $(OUTPUT_OPTION)
%.lo  : %.c;    $(COMPILE.c)  -fPIC      $<                        $(OUTPUT_OPTION)
%.lo  : %.cc;   $(COMPILE.cc) -fPIC      $<			   $(OUTPUT_OPTION)
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

ifneq "$(CC)" "cl"
LDLIBS  += -L${LIBDIR}
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

ifndef SHAREDLIBS
LDFLAGS += -static
endif

ifeq ($(OS),Linux)

# this is for gc somehow, see the end of gc/config.h
LDFLAGS += -Wl,-defsym,_DYNAMIC=0

# This next bit is really needed only for gnu libc6 (glibc-2.0.6)
# Without it, it does dynamic linking at run time and breaks our dumpdata scheme
# Don't link statically, so that these libraries actually get loaded at run time.
# If we must link statically, we should find out how to link all members.
# LDLIBS += -lnss_compat -lnss_db -lnss_dns -lnss_nis

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

ALLOBJ :=
MISCO := M2lib.o scclib.o gc_cpp.o tmp_init.o memdebug.o

ifdef SHAREDLIBS
LOADLIBES += -L../lib
LDFLAGS += -rdynamic
LIBRARYFILES += ../lib/libengine1.so
LIBRARYOPTIONS += -lengine1
LIBRARYFILES += ../lib/libengine2.so
LIBRARYOPTIONS += -lengine2
LIBRARYFILES += ../lib/libinterpreter.so
LIBRARYOPTIONS += -linterpreter
../lib/libinterpreter.so : $(PROJECT:.d=.loo) $(MISCO:.o=.lo)
	$(CC) -shared $^ $(OUTPUT_OPTION)
else
ALLOBJ += $(wildcard ../e/*.o)
ALLOBJ += $(PROJECT:.d=.oo) 
ALLOBJ += $(MISCO)
endif

ifeq ($(OS),Linux)
ifndef NODUMPDATA
LOADLIBES += -L../dumpdata
LIBRARYFILES += ../dumpdata/libdump.a
LIBRARYOPTIONS += -ldump
endif
endif

ifneq "$(CC)" "cl"
ALLOBJ += compat.o
endif

ifeq ($(OS),Linux)
# The maintainers of linux' libstdc++ unwisely decided to incorporate
# their own private versions of routines from libc in their library!
# Unforgiveable, since the symbols defined differ (of course).
ALLOBJ += putc.o
putc.o : /usr/lib/libc.a; ar x $^ $@
endif

ifeq ($(OS),Linux)
ALLOBJ += malloc.o
# LDFLAGS += -rdynamic
malloc.o : /usr/lib/libc.a; ar x $^ $@
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

ifdef MP
M2lib.o : mpversion.h
mpversion.h : ../../mp/MP/MP_Config.h
	grep MP_VERSION $< >$@
endif

###################### libraries

ifeq "$(CC)" "cl"
LIBRARYFILES += ../dbm/dbm2.lib
LIBRARYOPTIONS += ../dbm/dbm2.lib
LIBRARYFILES += ../../lib/gc.lib
LIBRARYOPTIONS += ../../lib/gc.lib
LIBRARYFILES += ../../lib/gmp.lib
LIBRARYOPTIONS += ../../lib/gmp.lib
LIBRARYFILES += ../../lib/libfac.lib
LIBRARYOPTIONS += ../../lib/libfac.lib
LIBRARYFILES += ../../lib/cf.lib
LIBRARYOPTIONS += ../../lib/cf.lib
else
LOADLIBES += -L../dbm
LIBRARYFILES += ../dbm/libdbm2.a
LIBRARYOPTIONS += -ldbm2
ifdef MP
LIBRARYFILES += ../../lib/libMP.a
LIBRARYOPTIONS += ../../lib/libMP.a
endif
ifdef SHAREDLIBS
LIBRARYFILES += ../lib/libfac.so
LIBRARYOPTIONS += -lfac
LIBRARYFILES += ../lib/libcf.so
LIBRARYOPTIONS += -lcf
else
LIBRARYFILES += ../../lib/libfac.a
LIBRARYOPTIONS += ../../lib/libfac.a
LIBRARYFILES += ../../lib/libcf.a
LIBRARYOPTIONS += ../../lib/libcf.a
endif
LIBRARYFILES += ../../lib/libgmp.a
LIBRARYOPTIONS += ../../lib/libgmp.a
LIBRARYFILES += ../../lib/libgc.a
LIBRARYOPTIONS += ../../lib/libgc.a
endif

LDLIBS := $(LIBRARYOPTIONS) $(LDLIBS)

######################

ifeq "$(CC)" "cl"
LINK_OUTPUT_OPTION = -link -out:$@.exe
else
LINK_OUTPUT_OPTION = $(OUTPUT_OPTION)
endif

ifneq "$(CC)" "cl"
LDLIBS += -lm
endif

../bin/Macaulay2 : $(ALLOBJ) $(LIBRARYFILES)
	rm -f $@
	time $(PURIFYCMD) $(CC) $(LDFLAGS) $(ALLOBJ) $(LOADLIBES) $(LDLIBS) $(LINK_OUTPUT_OPTION)
ifndef CYGWIN32
	$(STRIPCMD) $@
endif

t_main.o : types.h
t : t_main.o gmp.oo stdio.oo strings.oo system.oo varstrin.oo nets.oo scclib.o C.oo t.oo
	$(CC) $(LDFLAGS) $^ $(LDLIBS) $(OUTPUT_OPTION)

all:: TAGS

TAGS: Makefile
	@ echo making TAGS
	@ $(UTIL)echoout -r2 '>TAGS' $(foreach i, $(SRCFILES) M2lib.c types.h,  $(i),0)
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
