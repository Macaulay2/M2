# reminder - this file must run with non-gnu make programs
# sigh, but I can't make it

include ../Makefile.develop
##############################
.SUFFIXES: .m2
.PHONY: clean all install
CFLAGS =
LDFLAGS = -s
OPTIONS :=
DUMPSEQUENCE = \
	command.m2 \
	name.m2 \
	html.m2 \
	document.m2 \
	classes.m2 \
	option.m2 \
	methods.m2 \
	profile.m2 \
	underscore.m2 \
	autoload.m2 \
	equal.m2 \
	convert.m2 \
	system.m2 \
	debugging.m2 \
	remember.m2 \
	set.m2 \
	handles.m2 gbinterface.m2 \
	fold.m2 \
	max.m2 \
	sort.m2 \
	structure.m2 \
	functions.m2 \
	files.m2 \
	combinatorics.m2 \
	lists.m2 \
	nets.m2 \
	expressions.m2 \
	peek.m2 \
	printing.m2 \
	gateway.m2 \
	numbers.m2 \
	rings.m2 \
	enginering.m2 \
	integers.m2 \
	rationals.m2 \
	reals.m2 \
	complex.m2 \
	quotient.m2 \
	product.m2 \
	monoids.m2 \
	powers.m2 \
	orderedmonoidrings.m2 \
	schur.m2 \
	variables.m2 \
	ofcm.m2 \
	tables.m2 \
	modules.m2 \
	matrix.m2 \
	quotring.m2 \
	multilin.m2 \
	genmat.m2 \
	modules2.m2 \
	gb.m2 \
	matrix2.m2 \
	colon.m2 \
	galois.m2 \
	ringmap.m2 \
        scriptde/newring.m2 \
        scriptde/subring.m2 \
	ext.m2 \
	tor.m2 \
	monideal.m2 \
	duals.m2 \
	res.m2 \
	chaincomplexes.m2 \
	radical.m2 \
	factor.m2 \
	doc.m2 \
	macaulay.m2 \
	integrate.m2 \
	http.m2 \
        $(OPTIONS) \
	browse.m2 \
	examples.m2 \
        monomcurve.m2 \
	fano.m2 \
	code.m2 \
	obsolete.m2 \
	indeterminates.m2  # this one always last

MADEGFILES = gbdoc.m2 gbfunctions.m2 loads.m2 tutorials.m2

DUMPEDGFILES = $(DUMPSEQUENCE) \
		setup.m2 dumpdata.m2 \
		$(MADEGFILES) gbdoc.m2 gbfunctions.m2

DISTGFILES = setup.m2 $(DUMPSEQUENCE) makeM2.m2 dumpdata.m2 eg.m2

OTHERDIST = Makefile Makefile.egs gbdoc.awk gbfunctions.awk \
		runexample.c .gdbinit

DISTFILES = $(DISTGFILES) $(OTHERDIST) $(MADEGFILES) version.m2

GFILES = $(DISTGFILES)
# polynomial.m2 was just before ofcm.m2 and after orderedmonoidrings.m2
# ordered-monomials.m2 was after groups.m2 and before powers.m2

OTHER = COPYRIGHT image.mcly Makefile.tests mpcodes.sed scriptde/* makeVersion.awk
ALLFILES = $(OTHER) $(OTHERDIST) $(GFILES)

ifdef MP
MPSTUFF = mpcodes.m2
else
MPSTUFF = 
endif

ifeq "$(OS)" "MS-DOS"
SCRIPTS = ../bin/M2.bat ../bin/M2.arg
else
SCRIPTS = ../bin/M2
endif

all:		Examples Tests ../bin/Macaulay2-$(ARCH).data $(SCRIPTS) $(MPSTUFF)
install:	all ../bin/Macaulay2.doc
Examples:;	mkdir Examples
Tests:;		mkdir Tests

allfiles:	Makefile
	@echo making allfiles
	@../bin/echoout '>tmp' $(ALLFILES)
	@sort <tmp |uniq >allfiles
	@rm tmp
distfiles:	Makefile
	@echo making distfiles
	@../bin/echoout '>distfiles' $(DISTFILES)
wc:		;			wc -l $(DISTGFILES)

loads.m2 : Makefile
	@echo making loads.m2
	@../bin/echoout '>loads.m2' $(foreach i, $(DUMPSEQUENCE), 'load "$(i)"')

version.m2 : ../Makefile.develop makeVersion.awk ../bin/Macaulay2 $(DUMPEDGFILES)
	gawk -f makeVersion.awk </dev/null >version.tmp
	mv version.tmp $@

TUTORIALS = $(wildcard ../tutorial/final/*.out)
tutorials.m2 : Makefile
	@ echo making tutorials.m2
	@../bin/echoout '>tutorials.m2' $(foreach i, $(TUTORIALS), 'load "$(i)"')

gbdoc.m2 : ../e/misc/cmdnames.input gbdoc.awk
	awk -f gbdoc.awk ../e/misc/cmdnames.input >tmp
	mv tmp $@
gbfunctions.m2 :  ../e/misc/cmdnames.input gbfunctions.awk
	awk -f gbfunctions.awk ../e/misc/cmdnames.input >tmp
	mv tmp $@
TAGS: Makefile
	@echo making TAGS
	@../bin/echoout '>TAGS' $(foreach i, \
			      $(GFILES) ../CHANGES \
			      $(wildcard ../html/*.m2 ../test/*.m2 ../book/*.m2 \
				../basictests/*.m2 ../emacs/*.m2 \
				../tutorial/prelim/*.m2 ../tutorial/final/*.m2 \
				../emacs/emacs.hlp \
			        ../experiments/*.m2 \
			        ../slides/*.m2 \
				../Vasconcelos-appendix/*.tex \
				../examples/*.m2 ../schubert/*.m2), \
			       $(i),0)
$(SCRIPTS) : makeM2.m2
	rm -f $(SCRIPTS)
	../bin/Macaulay2 -silent '-eloaddata("../bin/Macaulay2-$(ARCH).data")' \
			-- '-erunStartFunctions()' makeM2.m2 '-eexit(0)'

../bin/Macaulay2-$(ARCH).data : $(DUMPEDGFILES) version.m2 ../bin/Macaulay2
	rm -f ../bin/Macaulay2-$(ARCH).data
	../bin/Macaulay2 -silent -tty '-ephase=1' setup.m2 dumpdata.m2 '-eexit(0)'

../bin/Macaulay2.pre : ../bin/Macaulay2-$(ARCH).data $(MADEGFILES) version.m2
	rm -rf ../bin/Macaulay2.pre
	../bin/Macaulay2 -silent '-ephase=2' setup.m2 '-eexit(0)'

../bin/Macaulay2.doc : ../bin/Macaulay2.pre ../bin/Macaulay2-$(ARCH).data $(MADEGFILES)
	-$(MAKE) -k -C Examples -f ../Makefile.egs
	rm -f ../bin/Macaulay2.doc
	../bin/Macaulay2 -silent '-ephase=4' setup.m2 '-eexit(0)'

scriptde.tests: scriptde/test.okay scriptde/testsubring.okay

tests: ../bin/Macaulay2-$(ARCH).data $(SCRIPTS)
	- $(MAKE) -k -C Tests -f ../Makefile.tests

clean:
	rm -rf ../bin/Macaulay2-$(ARCH).data ../bin/Macaulay2.doc Examples Tests \
		TAGS ../bin/M2 gbdoc.m2 gbfunctions.m2 runexample temp.data

ifdef MP
mpcodes.m2: ../../include/MP.h \
		mpcodes.sed
	sed -f mpcodes.sed $< > $@

endif
