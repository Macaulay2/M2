# SHELL := bash
# export SHELL
################################# targets to use
default : all install tests
doc : html book
everything : default doc
dists : dist bookdist devdist srcdist
################################# set up
all :: initialize
include Makefile.develop
# MAKEFLAGS += --no-print-directory
.PHONY: default install tests all allfiles clean dirs misc \
		initialize doc html book exe zip
ROOT := $(shell basename `pwd`)
################################# announce
all :: misc
misc ::
	@echo 'making Macaulay 2 version $(VERSION)'
	@echo '  for $(SYSTEM)'
	@echo '  in $(shell pwd)'
	@echo '  with LIBDIR = $(LIBDIR)'
	@echo '  and  INCDIR = $(INCDIR)'
################################# version
version : Makefile.develop
	bin/echoout '>$@' 'version ${VERSION} for ${SYSTEM}' 
version.gen : Makefile.develop
	bin/echoout '>$@' 'version ${VERSION}'
version.date : Makefile.develop
	bin/echoout '>$@' '${VERSIONDATE}'
misc :: version version.gen version.date
################################# bin
bin :; mkdir $@
clean ::; rm -rf bin
initialize :: bin
################################# tgz
tgz :; mkdir $@
clean ::; rm -rf tgz
initialize :: tgz
################################# util
initialize ::; $(MAKE) -C util all
################################# c
all ::; $(MAKE) -C c all
allfiles ::; $(MAKE) NODEPENDS=1 -C c allfiles
clean ::; $(MAKE) NODEPENDS=1 -C c clean
wc ::;  $(MAKE) -C c NODEPENDS=1 wc
################################# e
all ::; $(MAKE) -C e all
allfiles ::; $(MAKE) NODEPENDS=1 -C e allfiles
clean ::; $(MAKE) NODEPENDS=1 -C e clean
wc ::;  $(MAKE) -C e NODEPENDS=1 wc
################################# dbm
all ::; $(MAKE) -C dbm all
install ::; $(MAKE) -C dbm install
tests ::; -$(MAKE) -C dbm tests
allfiles ::; $(MAKE) NODEPENDS=1 -C dbm allfiles
clean ::; $(MAKE) NODEPENDS=1 -C dbm clean
uninstall ::; $(MAKE) -C dbm uninstall
wc ::;  $(MAKE) -C dbm NODEPENDS=1 wc
################################# d
all ::; bin/restart d/restart.tmp $(MAKE) -C d all
allfiles ::; $(MAKE) NODEPENDS=1 -C d allfiles
clean ::; $(MAKE) NODEPENDS=1 -C d clean
uninstall ::; $(MAKE) -C d uninstall
wc ::;  $(MAKE) -C d NODEPENDS=1 wc
################################# basictests
allfiles ::; $(MAKE) -C basictests allfiles
all ::; -$(MAKE) -C basictests -k
clean ::; $(MAKE) -C basictests clean
################################# thread
allfiles.dev ::; $(MAKE) NODEPENDS=1 -C thread allfiles
################################# tutorial
#### keep this before 'm2' so the final/*.out files get made
all::; $(MAKE) -C tutorial all
allfiles ::; $(MAKE) -C tutorial allfiles
################################# m2
all ::; $(MAKE) -C m2 all
install ::; $(MAKE) -C m2 install
allfiles ::; $(MAKE) -C m2 allfiles
distfiles ::; $(MAKE) -C m2 distfiles
tests ::;  $(MAKE) -C m2 tests
clean ::; $(MAKE) -C m2 clean
wc ::;  $(MAKE) -C m2 wc
################################# schubert
tests ::; -$(MAKE) -C schubert tests
################################# slides
allfiles ::; $(MAKE) NODEPENDS=1 -C slides allfiles
tests ::; -$(MAKE) -k -C slides
################################# test
allfiles ::; $(MAKE) -C test allfiles
tests ::; -$(MAKE) -C test -k
clean ::; $(MAKE) -C test clean
################################# screen
allfiles.dev ::; $(MAKE) -C screen allfiles
################################# emacs
install ::; $(MAKE) -C emacs
allfiles ::; $(MAKE) -C emacs allfiles
################################# html
html ::; $(MAKE) -C html install
clean ::; $(MAKE) -C html clean
allfiles ::; $(MAKE) -C html allfiles
################################# book
book ::;  $(MAKE) -C book install
allfiles ::; $(MAKE) -C book allfiles
distfiles ::; $(MAKE) -C book distfiles
clean ::; $(MAKE) -C book clean
################################# appendix to Vasconcelos' book
allfiles ::; $(MAKE) -C Vasconcelos-appendix allfiles
tests ::; $(MAKE) -C Vasconcelos-appendix
################################# socket
allfiles.dev ::; $(MAKE) -C socket allfiles
################################# Mathematica
allfiles.dev ::; $(MAKE) -C Mathematica allfiles
clean ::; if [ -d Mathematica ]; then $(MAKE) -C Mathematica clean; fi

################################# limited source distribution for porting
SRCDIST := tgz/Macaulay2-${VERSION}-src-limited.tgz
srcdist : ${SRCDIST}
SRCDISTFILES = Makefile.develop util licenses install.bat include msdos \
	Makefile README CHANGES setup \
	$(shell sed 's-^-c/-' <c/allfiles) \
	$(shell sed 's-^-d/-' <d/allfiles) \
	$(shell sed 's-^-dbm/-' <dbm/allfiles) \
	$(shell sed 's-^-m2/-' <m2/allfiles) \
	$(shell sed 's-^-e/-' <e/allfiles) \
	$(shell sed 's-^-emacs/-' <emacs/allfiles) \
	$(shell sed 's-^-html/-' <html/allfiles) \
	$(shell sed 's-^-test/-' <test/allfiles) \
	$(shell sed 's=^=Vasconcelos-appendix/=' <Vasconcelos-appendix/allfiles) \
	$(shell sed 's-^-basictests/-' <basictests/allfiles) \
	$(shell sed 's-^-book/-' <book/allfiles) \
	$(shell sed 's-^-tutorial/-' <tutorial/allfiles)
${SRCDIST} : allfiles
	tar -c -z -f $@ -C .. $(addprefix $(ROOT)/, $(SRCDISTFILES))

################################# source distribution for developers
DEVDIST := tgz/Macaulay2-${VERSION}-src-dev.tgz
devdist : allfiles allfiles.dev ${DEVDIST}
DEVDISTFILES = $(SRCDISTFILES) \
	schubert experiments docs eisenbud gmp-macros benchmarks benchmarks2 \
	mike examples \
	$(shell sed 's-^-slides/-' <slides/allfiles) \
	$(shell sed 's-^-screen/-' <screen/allfiles) \
	$(shell sed 's-^-thread/-' <thread/allfiles) \
	$(shell sed 's-^-socket/-' <socket/allfiles) \
	$(shell sed 's-^-Mathematica/-' <Mathematica/allfiles)
DEVLABEL := -V "Macaulay 2 source for developers, ${VERSION}, `date`"
${DEVDIST} : allfiles allfiles.dev
	tar -c -z -f $@ $(DEVLABEL) -C .. $(addprefix $(ROOT)/, $(DEVDISTFILES))
floppy  : allfiles allfiles.dev
	@echo making floppy
	@tar -c -z -f /dev/floppy \
		--label "Macaulay2 $(VERSION) sources backup by $(USER) on `hostname`, `date`" \
		$(DEVDISTFILES)
################################# binary distributions
# we use .tar.gz as extension, because Netscape doesn't recognize .tgz

ifeq ($(OS),MS-DOS)
EXE := bin/Macaulay2.exe
SETUP := setup.bat
DIST := tgz/Macaulay2-$(DESC).exe
else
EXE := bin/Macaulay2
SETUP := setup
DIST := tgz/Macaulay2-$(DESC).tar.gz
endif

dist : allfiles tgz all distfiles misc html $(SETUP) ${DIST}
setup.bat : msdos/setup.bat; cp $^ $@

DISTRIBUTIONFILES = $(EXE) \
	$(shell sed 's-^-book/-' <book/distfiles) \
	bin/Macaulay2.doc $(SETUP) \
	$(wildcard tutorial/final/*.out) \
	bin/echoout bin/restart bin/timestmp bin/update licenses \
	$(shell sed 's=^=Vasconcelos-appendix/=' <Vasconcelos-appendix/allfiles) \
	$(shell sed 's-^-emacs/-' <emacs/allfiles) emacs/M2-symbols.el \
	m2/loads.m2 $(shell sed 's-^-m2/-' <m2/distfiles) \
	e/misc/cmdnames.input \
	e/cmdnames.m2 \
	util html README CHANGES version version.gen version.date
DISTLABEL := -V "Macaulay 2, ${DESC}, `date`"
tgz/Macaulay2-$(DESC).tar.gz : distfiles bin/Macaulay2.doc $(EXE)
	chmod a+rX $(DISTRIBUTIONFILES)
	tar cf - -C .. $(addprefix $(ROOT)/,$(DISTRIBUTIONFILES)) |gzip >$@
exe : tgz/Macaulay2-$(DESC).exe
zip : tgz/Macaulay2-$(DESC).zip
tgz/Macaulay2-$(DESC).zip :
	bin/echoout '>files.tmp' $(DISTRIBUTIONFILES)
	rm -f $@
	winzip32 -a -r -p $@ @files.tmp
tgz/Macaulay2-$(DESC).exe : tgz/Macaulay2-$(DESC).zip
	winzipse $< -d C:/Macaulay2-$(VERSION) -m msdos/M2.diz -overwrite
################################# book distribution
BOOK  := tgz/M2book-$(VERSION).dvi.gz
HBOOK := tgz/M2hbook-$(VERSION).dvi.gz
bookdist : allfiles tgz ${BOOK} ${HBOOK}
${BOOK}  : book/M2book.dvi;	gzip <$< >$@
${HBOOK} : book/M2hbook.dvi;	gzip <$< >$@
