# SHELL := bash
# export SHELL
############################### targets to use
all ::				# everything needed for the distribution
tests ::
doc:: html
doc:: book
# doc:: ComputationsBook
ifndef CYGWIN32
doc:: Vasconcelos-appendix
endif
################################# prologue
justM2 all :: initialize config
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
justM2 all :: lib
lib :; mkdir $@
endif
clean ::; rm -rf lib
################################# tmp
clean ::; rm -rf tmp
################################# util
initialize ::; $(MAKE) -C util all
clean ::; $(MAKE) NODEPENDS=1 -C util clean
wc ::; $(MAKE) NODEPENDS=1 -C util wc
################################# c
justM2 all ::; $(MAKE) -C c all
clean ::; $(MAKE) NODEPENDS=1 -C c clean
wc ::;  $(MAKE) -C c NODEPENDS=1 wc
################################# c2
all ::; $(MAKE) NODEPENDS=1 -C c2 targettypes.h
clean ::; $(MAKE) NODEPENDS=1 -C c2 clean
################################# e
justM2 all ::; $(MAKE) -C e all
tests ::; $(MAKE) -C e tests
clean ::; $(MAKE) NODEPENDS=1 -C e clean
wc ::;  $(MAKE) -C e NODEPENDS=1 wc
################################# dbm
justM2 all ::; $(MAKE) -C dbm all
tests ::; $(MAKE) -C dbm tests
clean ::; $(MAKE) NODEPENDS=1 -C dbm clean
wc ::;  $(MAKE) -C dbm NODEPENDS=1 wc
################################# dumpdata
ifndef NODUMPDATA
ifdef NEWDUMPDATA
justM2 all ::; $(MAKE) -C dumpdata all
tests ::; $(MAKE) -C dumpdata tests
endif
endif
clean ::; $(MAKE) NODEPENDS=1 -C dumpdata clean
wc ::;  $(MAKE) -C dumpdata NODEPENDS=1 wc
################################# d
config ::; $(MAKE) NODEPENDS=1 -C d compat.h
justM2 all ::; util/restart d/restart.tmp $(MAKE) -C d all
clean ::; $(MAKE) NODEPENDS=1 -C d clean
wc ::;  $(MAKE) -C d NODEPENDS=1 wc
################################# basictests
all ::; $(MAKE) -C basictests -k
clean ::; $(MAKE) -C basictests clean
################################# thread
clean ::; if [ -d thread ]; then $(MAKE) -C thread clean; fi
################################# tutorial
#### keep this before 'm2' so the final/*.out files get made
justM2 all::; $(MAKE) -C tutorial all
clean::; $(MAKE) -C tutorial clean
################################# m2
all ::; $(MAKE) -C m2 all
justM2 ::; $(MAKE) -C m2 default
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
setup : setup.template ../Makeconf.lcl
	sed -e 's/%DUMPDATA%/true/' <$< >$@
	chmod a+x $@
else
setup : setup.template ../Makeconf.lcl
	sed -e 's/%DUMPDATA%/false/' <$< >$@
	chmod a+x $@
endif
################################# experiments
clean ::; if [ -d experiments ]; then $(MAKE) -C experiments clean; fi
all   ::; if [ -d experiments ]; then $(MAKE) -C experiments all; fi
################################# slides
clean ::; if [ -d slides ]; then $(MAKE) -k -C slides clean; fi
tests ::; if [ -d slides ]; then $(MAKE) -k -C slides; fi
################################# html
html ::; $(MAKE) -C html all
clean ::; $(MAKE) -C html clean
################################# book
book ::;  $(MAKE) -C book all
clean ::; $(MAKE) -C book clean
################################# ComputationsBook
ComputationsBook ::; if [ -d ComputationsBook ]; then $(MAKE) -C ComputationsBook all; fi
clean ::; if [ -d ComputationsBook ]; then $(MAKE) -C ComputationsBook clean; fi
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
