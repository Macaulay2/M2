TOPDIR = ../..
include $(TOPDIR)/Makeconf

all::

##############################
.SUFFIXES: .m2
.PHONY: clean all install phase1 tests
CFLAGS =
LDFLAGS = -s
LOADLIBES =
DUMPSEQUENCE = $(shell cat dumpseq)

MADEGFILES := gbdoc.m2 gbfunctions.m2 loads.m2 tutorials.m2 cmdnames.m2

ifdef MP
mpcodes.m2: ../../include/MP.h mpcodes.sed
	sed -f mpcodes.sed $< > $@
all:: mpcodes.m2
MADEGFILES += mpcodes.m2
endif

cmdnames.m2 : ../e/cmdnames.m2; cp $< $@

TUTORIALS := $(wildcard ../tutorial/final/*.out)

DUMPEDGFILES := $(DUMPSEQUENCE) setup.m2 $(MADEGFILES) $(TUTORIALS)

ifdef DUMPDATA
DUMPEDGFILES := $(DUMPEDGFILES) dumpdata.m2
endif

GFILES = $(DUMPSEQUENCE) setup.m2 dumpdata.m2 eg.m2 sagbi.m2 makeM2.m2

##############################

SCRIPTS = ../bin/M2

ifeq "$(OS)" "MS-DOS"
SCRIPTS = ../bin/M2.bat ../bin/M2.arg
endif

ifeq "$(OS)" "CYGWIN32-95"
SCRIPTS = ../bin/M2.bat
endif

wc:; wc -l $(GFILES)

loads.m2 : dumpseq loads.awk
	@echo making loads.m2
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
TAGS: Makefile
	@echo making TAGS
	@../util/echoout '>TAGS' $(foreach i, \
			      $(GFILES) $(MADEGFILES) ../CHANGES \
			      $(wildcard ../html/*.m2 ../test/*.m2 ../book/*.m2 \
				../basictests/*.m2 ../emacs/*.m2 \
				../tutorial/prelim/*.m2 ../tutorial/final/*.m2 \
				../emacs/emacs.hlp \
			        ../experiments/*.m2 \
			        ../slides/*.m2 \
				../Vasconcelos-appendix/eg.m2 \
				../Vasconcelos-appendix/appendix.tex \
				../Vasconcelos-appendix/cohomology.tex \
				../Vasconcelos-appendix/elementary.tex \
				../Vasconcelos-appendix/introduction.tex \
				../examples/*.m2 ../schubert/*.m2), \
			       $(i),0)

all :: phase1

phase1 :: $(DUMPEDGFILES)

ifdef DUMPDATA
ARGS := -silent '-eloaddata("../cache/Macaulay2-$(ARCH).data")' -- '-erunStartFunctions()' 
phase1 :: ../cache/Macaulay2-$(ARCH).data
../cache/Macaulay2-$(ARCH).data : $(DUMPEDGFILES) ../bin/Macaulay2
	rm -f ../cache/Macaulay2-$(ARCH).data
	../bin/Macaulay2 -silent -tty '-ephase=1' setup.m2 dumpdata.m2 '-eexit(0)'
else
ARGS := -silent setup.m2
endif

phase1 :: ../cache/Macaulay2.made
../cache/Macaulay2.made : $(DUMPEDGFILES) ../bin/Macaulay2$(EXESUFFIX)
	touch $@

all:: $(SCRIPTS)
$(SCRIPTS) : makeM2.m2
	rm -f $(SCRIPTS)
	../bin/Macaulay2 $(ARGS) makeM2.m2 '-eexit(0)'

../cache/Macaulay2.pre : $(DUMPEDGFILES) ../bin/Macaulay2$(EXESUFFIX)
	rm -f ../cache/Macaulay2.tmp
	../bin/Macaulay2 -silent '-ephase=2' setup.m2 '-eexit(0)'
	mv ../cache/Macaulay2.tmp ../cache/Macaulay2.pre

backup : CVS/Entries
CVS/Entries : $(GFILES)
	tar cfzv /f/dan/backups/m2-$$$$.tgz $?

all :: bare

all :: ../cache/Macaulay2.doc
../cache/Macaulay2.doc : ../cache/Macaulay2.pre $(MADEGFILES)
	$(MAKE) -k -C ../tmp/Examples -f ../../m2/Makefile.egs
	rm -f ../cache/Macaulay2.tmp
	../bin/Macaulay2 -silent '-ephase=4' setup.m2 '-eexit(0)'
	mv ../cache/Macaulay2.tmp ../cache/Macaulay2.doc

tests: $(SCRIPTS)
	 $(MAKE) -k -C ../tmp/Tests -f ../../m2/Makefile.tests

clean:
	rm -rf ../cache/Macaulay2-$(ARCH).data ../cache/Macaulay2.doc \
		../tmp/Examples ../tmp/Tests \
		TAGS ../bin/M2 runexample \
		temp.data allfiles distfiles \
		gbdoc.m2 gbfunctions.m2 loads.m2 tutorials.m2 cmdnames.m2 \
		mpcodes.m2 tutorials.m2 version.m2 core

