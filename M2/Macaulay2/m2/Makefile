include ../../Makeconf

all::

##############################
.SUFFIXES: .m2
.PHONY: clean all install
CFLAGS =
LDFLAGS = -s
DUMPSEQUENCE = $(shell cat dumpsequence)

MADEGFILES := gbdoc.m2 gbfunctions.m2 loads.m2 tutorials.m2 cmdnames.m2

ifdef MP
mpcodes.m2: ../../include/MP.h mpcodes.sed
	sed -f mpcodes.sed $< > $@
all:: mpcodes.m2
MADEGFILES += mpcodes.m2
endif

cmdnames.m2 : ../e/cmdnames.m2; cp $< $@

DUMPEDGFILES = $(DUMPSEQUENCE) setup.m2 dumpdata.m2 $(MADEGFILES)

GFILES = $(DUMPSEQUENCE) setup.m2 dumpdata.m2 eg.m2 sagbi.m2 makeM2.m2

##############################

ifeq "$(OS)" "MS-DOS"
SCRIPTS = ../bin/M2.bat ../bin/M2.arg
else
SCRIPTS = ../bin/M2
endif

wc:		;			wc -l $(GFILES)

loads.m2 : dumpsequence
	@echo making loads.m2
	@../util/echoout '>loads.m2' $(foreach i, $(DUMPSEQUENCE), 'load "$(i)"')

version.m2 : ../../Makeconf ../../Makeconf-m2 ../bin/Macaulay2 $(DUMPEDGFILES)
	../../Makeconf-m2 >version.tmp
	mv version.tmp $@

TUTORIALS = $(wildcard ../tutorial/final/*.out)
tutorials.m2 : Makefile
	@ echo making tutorials.m2
	@../util/echoout '>tutorials.m2' $(foreach i, $(TUTORIALS), 'load "$(i)"')

gbdoc.m2 : ../e/misc/cmdnames.input gbdoc.awk
	awk -f gbdoc.awk ../e/misc/cmdnames.input >tmp
	mv tmp $@
gbfunctions.m2 :  ../e/misc/cmdnames.input gbfunctions.awk
	awk -f gbfunctions.awk ../e/misc/cmdnames.input >tmp
	mv tmp $@
all:: TAGS
TAGS: Makefile
	@echo making TAGS
	@../util/echoout '>TAGS' $(foreach i, \
			      $(GFILES) ../CHANGES \
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
all:: $(SCRIPTS)
$(SCRIPTS) : makeM2.m2 ../cache/Macaulay2-$(ARCH).data
	rm -f $(SCRIPTS)
	../bin/Macaulay2 -silent '-eloaddata("../cache/Macaulay2-$(ARCH).data")' \
			-- '-erunStartFunctions()' makeM2.m2 '-eexit(0)'

all:: ../cache/Macaulay2-$(ARCH).data
../cache/Macaulay2-$(ARCH).data : $(DUMPEDGFILES) \
			version.m2 ../bin/Macaulay2
	rm -f ../cache/Macaulay2-$(ARCH).data
	../bin/Macaulay2 -silent -tty '-ephase=1' setup.m2 dumpdata.m2 '-eexit(0)'


../cache/Macaulay2.pre : ../cache/Macaulay2-$(ARCH).data \
			$(MADEGFILES) version.m2
	rm -f ../cache/Macaulay2.tmp
	../bin/Macaulay2 -silent '-ephase=2' setup.m2 '-eexit(0)'
	mv ../cache/Macaulay2.tmp ../cache/Macaulay2.pre

all :: ../cache/Macaulay2.doc
../cache/Macaulay2.doc : ../cache/Macaulay2.pre \
			../cache/Macaulay2-$(ARCH).data $(MADEGFILES)
	$(MAKE) -k -C ../tmp/Examples -f ../../m2/Makefile.egs
	rm -f ../cache/Macaulay2.tmp
	../bin/Macaulay2 -silent '-ephase=4' setup.m2 '-eexit(0)'
	mv ../cache/Macaulay2.tmp ../cache/Macaulay2.doc

scriptde.tests: scriptde/test.okay scriptde/testsubring.okay

tests: ../cache/Macaulay2-$(ARCH).data $(SCRIPTS)
	 $(MAKE) -k -C ../tmp/Tests -f ../../m2/Makefile.tests

clean:
	rm -rf ../cache/Macaulay2-$(ARCH).data ../cache/Macaulay2.doc \
		../tmp/Examples ../tmp/Tests \
		TAGS ../bin/M2 runexample \
		temp.data allfiles distfiles \
		gbdoc.m2 gbfunctions.m2 loads.m2 tutorials.m2 \
		mpcodes.m2 tutorials.m2 version.m2
