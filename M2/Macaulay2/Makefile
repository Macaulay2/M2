# SHELL := bash
# export SHELL
############################### targets to use
all ::				# everything needed for the distribution
tests ::
doc : html book Vasconcelos-appendix
################################# setup
all :: initialize
include ../Makeconf
.PHONY: default tests all clean dirs misc \
		initialize doc html book exe zip
ROOT := $(shell basename `pwd`)
################################# version
version : ../Makeconf
	bin/echoout '>$@' 'version ${VERSION} for ${SYSTEM}' 
version.gen : ../Makeconf
	bin/echoout '>$@' 'version ${VERSION}'
misc :: version version.gen
clean :: ; rm version version.gen
################################# bin
bin :; mkdir $@
clean ::; rm -rf bin
initialize :: bin
################################# cache
initialize :: cache
cache :; mkdir $@
clean ::; rm -rf cache
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
all ::; util/restart d/restart.tmp $(MAKE) -C d all
clean ::; $(MAKE) NODEPENDS=1 -C d clean
wc ::;  $(MAKE) -C d NODEPENDS=1 wc
################################# basictests
all ::; $(MAKE) -C basictests -k
clean ::; $(MAKE) -C basictests clean
################################# thread
clean ::; $(MAKE) -C thread clean
################################# tutorial
#### keep this before 'm2' so the final/*.out files get made
all::; $(MAKE) -C tutorial all
clean::; $(MAKE) -C tutorial clean
################################# test
tests ::; $(MAKE) -C test -k
clean ::; $(MAKE) -C test clean
################################# m2
all ::; $(MAKE) -C m2 all
tests ::;  $(MAKE) -C m2 tests
clean ::; $(MAKE) -C m2 clean
wc ::;  $(MAKE) -C m2 wc
################################# schubert
tests ::; $(MAKE) -C schubert tests
################################# slides
clean ::; -$(MAKE) -k -C slides clean
tests ::; $(MAKE) -k -C slides
################################# html
html ::; $(MAKE) -C html all
clean ::; $(MAKE) -C html clean
################################# book
book ::;  $(MAKE) -C book all
clean ::; $(MAKE) -C book clean
################################# appendix to Vasconcelos' book
Vasconcelos-appendix ::; $(MAKE) -C Vasconcelos-appendix all
################################# socket
clean ::; $(MAKE) -C socket clean
################################# Mathematica
clean ::; if [ -d Mathematica ]; then $(MAKE) -C Mathematica clean; fi
