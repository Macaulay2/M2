TOPDIR = ../..
include $(TOPDIR)/Makeconf

all:: M2book.dvi
M2book.tex : ../cache/Macaulay2.doc book.m2 booktex.m2
	../bin/M2 -q book.m2 '-e exit 0'
	mv M2book.tmp M2book.tex
clean :; rm -rf M2book.tex M2book.dvi M2book.log
