default : phase1

TOPDIR = ../..
include $(TOPDIR)/Makeconf

export M2HOME := $(shell cd ..; pwd)

all::

##############################
.SUFFIXES: .m2
.PHONY: clean all install phase1 tests
# CFLAGS =
# LDFLAGS = -s
# LOADLIBES =
DUMPSEQUENCE = $(shell cat dumpseq)
DOCSEQUENCE = $(shell cat docseq)

MADEGFILES := gbdoc.m2 gbfunctions.m2 loads.m2 docloads.m2 tutorials.m2 cmdnames.m2

export LD_LIBRARY_PATH=../lib

ifdef MP
mpcodes.m2: ../../include/MP.h mpcodes.sed
	sed -f mpcodes.sed $< > $@
all:: mpcodes.m2
MADEGFILES += mpcodes.m2
endif

cmdnames.m2 : ../e/cmdnames.m2; cp $< $@

TUTORIALS := $(wildcard ../tutorial/final/*.out)

DUMPEDGFILES := $(DUMPSEQUENCE) $(DOCSEQUENCE) setup.m2 $(MADEGFILES) $(TUTORIALS)

ifdef DUMPDATA
DUMPEDGFILES := $(DUMPEDGFILES) dumpdata.m2
endif

NONDOCGFILES = setup.m2 $(DUMPSEQUENCE) dumpdata.m2 eg.m2 sagbi.m2 makeM2.m2

wc:
	wc -l $(DOCSEQUENCE)
	wc -l $(NONDOCGFILES)

GFILES = $(NONDOCGFILES) $(DOCSEQUENCE)
SCRIPTS = ../bin/M2

ifeq "$(OS)" "MS-DOS"
SCRIPTS = ../bin/M2.bat ../bin/M2.arg
endif

ifeq "$(OS)" "Windows-95-98-NT"
SCRIPTS = ../bin/M2.bat ../bin/M2
endif


loads.m2 : dumpseq loads.awk
	@echo making $@
	@awk -f loads.awk <$< >$@

docloads.m2 : docseq loads.awk
	@echo making $@
	@awk -f loads.awk <$< >$@

#version.m2 : $(TOPDIR)/Makeconf ../../Makeconf-m2
#	../../Makeconf-m2 >version.tmp
#	mv version.tmp $@

tutorials.m2 :
	@ echo making tutorials.m2
	@ ls ../tutorial/final/*.out |awk '{printf "load \"%s\"\n", $$0}' >$@

gbdoc.m2 : ../e/misc/cmdnames.input gbdoc.awk
	awk -f gbdoc.awk ../e/misc/cmdnames.input >tmp
	mv tmp $@
gbfunctions.m2 :  ../e/misc/cmdnames.input gbfuns.awk
	awk -f gbfuns.awk ../e/misc/cmdnames.input >tmp
	mv tmp $@
all:: TAGS
TAGS: Makefile dumpseq
	@echo making TAGS
	@../util/echoout '>TAGS' $(foreach i, \
			      $(GFILES) $(MADEGFILES) \
			      $(wildcard ../html/*.m2 ../test/*.m2 ../book/*.m2 \
				../basictests/*.m2 ../emacs/*.m2 \
				../tutorial/prelim/*.m2 ../tutorial/final/*.m2 \
				../emacs/emacs.hlp \
				../packages/*.m2 \
				../*/Makefile* \
			        ../experiments/*.m2 \
			        ../slides/*.m2 \
			        ../slides/lib/*.m2 \
				../Vasconcelos-appendix/eg.m2 \
				../Vasconcelos-appendix/appendix.tex \
				../Vasconcelos-appendix/cohomology.tex \
				../Vasconcelos-appendix/elementary.tex \
				../Vasconcelos-appendix/introduction.tex \
				../examples/*.m2 ../schubert/*.m2 \
				../CHANGES), \
			       $(i),0)

all :: phase1

phase1 :: $(DUMPEDGFILES)

ifdef DUMPDATA
ARGS := -q -silent '-eloaddata("../cache/Macaulay2-$(ARCH)-data")' -- '-erunStartFunctions()' 
phase1 :: ../cache/Macaulay2-$(ARCH)-data
../cache/Macaulay2-$(ARCH)-data : $(DUMPEDGFILES) ../bin/Macaulay2
	rm -f ../cache/Macaulay2-$(ARCH)-data
	time ../bin/Macaulay2 -q -silent -tty '-ephase=1' setup.m2 dumpdata.m2 '-edump()'
else
ARGS := -silent setup.m2
endif

phase1 :: ../cache/Macaulay2-made
../cache/Macaulay2-made : $(DUMPEDGFILES) ../bin/Macaulay2$(EXESUFFIX)
	touch $@

phase1 :: $(SCRIPTS)
ifeq "$(CC)" "cl"
$(SCRIPTS) : ../util/setup.exe
	cd ..; util/setup
else
$(SCRIPTS) : makeM2.m2 ../../Makeconf.h
	rm -f $(SCRIPTS)
	../bin/Macaulay2 $(ARGS) makeM2.m2 '-eexit(0)'
endif

../cache/Macaulay2-pre : $(DUMPEDGFILES) ../bin/Macaulay2$(EXESUFFIX)
	rm -f ../cache/Macaulay2-tmp
	../bin/Macaulay2 -q -silent '-ephase=2' setup.m2 '-eexit(0)'
	mv ../cache/Macaulay2-tmp ../cache/Macaulay2-pre

backup : CVS/Entries
CVS/Entries : $(GFILES)
	mount /a.ext2
	tar cfv - $? | (cd /a.ext2; mkdir -p m2; cd m2; tar xf -)
	umount /a.ext2

profile : gmon.out
	gprof ../bin/Macaulay2 >profile
clean ::; rm -f profile

ifneq "$(CC)" "cl"
all :: bare
endif

INPUTS := ../tmp/Examples/sample  ../tmp/Examples/expression
../tmp/Examples/sample: sample; cp $< $@
../tmp/Examples/expression: expression; cp $< $@

all :: ../cache/Macaulay2-doc
../cache/Macaulay2-doc : ../cache/Macaulay2-pre $(MADEGFILES) $(INPUTS)
	$(MAKE) -k -C ../tmp/Examples -f ../../m2/Makefile.egs
	rm -f ../cache/Macaulay2-tmp
	../bin/Macaulay2 -q -silent '-ephase=4' setup.m2 '-eexit(0)'
	mv ../cache/Macaulay2-tmp ../cache/Macaulay2-doc

test : test.m2 phase1 $(SCRIPTS)
	M2 '-e errorDepth 0' -q test.m2 '-e << "--test succeeded" << endl' '-e exit 0'

tests:: $(SCRIPTS)
	 $(MAKE) -k -C ../tmp/Tests -f ../../m2/Makefile.tests

tests:: test1 test2
test1:
	echo 2+3 | M2
	touch $@
test2: ../util/screen
	echo 2+3 | ../util/screen M2
	touch $@

gdb : ; gdb ../bin/Macaulay2

clean::
	rm -rf ../cache/Macaulay2-$(ARCH)-data ../cache/Macaulay2-doc \
		../tmp/Examples ../tmp/Tests *.obj *.exe bare \
		TAGS ../bin/M2 runexample \
		temp.data allfiles distfiles \
		gbdoc.m2 gbfunctions.m2 loads.m2 docloads.m2 tutorials.m2 cmdnames.m2 \
		mpcodes.m2 tutorials.m2 version.m2 core

