# SHELL := bash
# export SHELL
############################### targets to use
all ::				# everything needed for the distribution
tests ::
doc : html book Vasconcelos-appendix
################################# prologue
all :: initialize config
TOPDIR = ..
include $(TOPDIR)/Makeconf
.PHONY: default tests all clean dirs misc config \
		initialize doc html book exe zip
ROOT := $(shell basename `pwd`)
clean::; rm -f core
################################# version
version : ../Makeconf
	bin/echoout '>$@' 'version ${VERSION} for ${SYSTEM}' 
version.gen : ../Makeconf
	bin/echoout '>$@' 'version ${VERSION}'
misc :: version version.gen
clean :: ; rm -f version version.gen
################################# bin
bin :; mkdir $@
clean ::; rm -rf bin
initialize :: bin
################################# cache
initialize :: cache
cache :; mkdir $@
clean ::; rm -rf cache
################################# lib
ifdef SHAREDLIBS
all :: lib
lib :; mkdir $@
endif
clean ::; rm -rf lib
################################# tmp
tmp :; mkdir $@
tmp/Examples :; mkdir $@
tmp/Tests :; mkdir $@
clean ::; rm -rf tmp
initialize :: tmp tmp/Tests tmp/Examples
################################# util
initialize ::; $(MAKE) -C util all
clean ::; $(MAKE) NODEPENDS=1 -C util clean
################################# c
all ::; $(MAKE) -C c all
clean ::; $(MAKE) NODEPENDS=1 -C c clean
wc ::;  $(MAKE) -C c NODEPENDS=1 wc
################################# e
all ::; $(MAKE) -C e all
clean ::; $(MAKE) NODEPENDS=1 -C e clean
wc ::;  $(MAKE) -C e NODEPENDS=1 wc
################################# dbm
all ::; $(MAKE) -C dbm all
tests ::; $(MAKE) -C dbm tests
clean ::; $(MAKE) NODEPENDS=1 -C dbm clean
wc ::;  $(MAKE) -C dbm NODEPENDS=1 wc
################################# d
config ::; $(MAKE) NODEPENDS=1 -C d compat.h
all ::; util/restart d/restart.tmp $(MAKE) -C d all
clean ::; $(MAKE) NODEPENDS=1 -C d clean
wc ::;  $(MAKE) -C d NODEPENDS=1 wc
################################# basictests
all ::; $(MAKE) -C basictests -k
clean ::; $(MAKE) -C basictests clean
################################# thread
clean ::; if [ -d thread ]; then $(MAKE) -C thread clean; fi
################################# tutorial
#### keep this before 'm2' so the final/*.out files get made
all::; $(MAKE) -C tutorial all
clean::; $(MAKE) -C tutorial clean
################################# m2
all ::; $(MAKE) -C m2 all
tests ::;  $(MAKE) -C m2 tests
clean ::; $(MAKE) -C m2 clean
wc ::;  $(MAKE) -C m2 wc
################################# test
tests ::; $(MAKE) -C test -k
clean ::; $(MAKE) -C test clean
################################# schubert
tests ::; if [ -d schubert ]; then $(MAKE) -C schubert tests; fi
################################# setup
all :: setup
ifdef DUMPDATA
setup : setup.dumpdata ../Makeconf.lcl
	cp $< $@
else
setup : setup.nodumpdata ../Makeconf.lcl
	cp $< $@
endif
################################# slides
clean ::; if [ -d slides ]; then $(MAKE) -k -C slides clean; fi
tests ::; if [ -d slides ]; then $(MAKE) -k -C slides; fi
################################# html
html ::; $(MAKE) -C html all
clean ::; $(MAKE) -C html clean
################################# book
book ::;  $(MAKE) -C book all
clean ::; $(MAKE) -C book clean
################################# emacs
all ::;  $(MAKE) -C emacs all
clean ::; $(MAKE) -C emacs clean
################################# appendix to Vasconcelos' book
Vasconcelos-appendix ::; $(MAKE) -C Vasconcelos-appendix all
clean ::; $(MAKE) -C Vasconcelos-appendix clean
################################# socket
clean ::; if [ -d socket ]; then $(MAKE) -C socket clean; fi
################################# Mathematica
clean ::; if [ -d Mathematica ]; then $(MAKE) -C Mathematica clean; fi
################################# backup
backup ::; $(MAKE) -C m2 backup
backup ::; if [ -d docs ]; then $(MAKE) -C docs backup; fi
backup ::; if [ -d experiments ]; then $(MAKE) -C experiments backup; fi
################################# tar manufactured files
manufactured.tgz :
	tar cfz $@ \
		e/geovec.hpp e/geores.hpp e/cmdnames.m2 e/cmdnames.hpp e/cmdinst.hpp \
		m2/tutorials.m2 tutorial/final/*.out \
		m2/gbdoc.m2 m2/gbfunctions.m2
