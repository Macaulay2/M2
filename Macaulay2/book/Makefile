TOPDIR = ../..
include $(TOPDIR)/Makeconf

DVIPSFLAGS = -z
TEX=latex

%.bux : %.tex
	$(TEX) '\scrollmode \input $<'
	makeindex $*
	mv $*.dvi $*-prelim.dvi
	cp $*.aux $*.bux
.PRECIOUS : %.bux
%.bbl : %.bux
	cp $*.bux $*.aux
	bibtex $*
%.dvi : %.tex
%.dvi : %.bbl
	$(TEX) '\scrollmode \input $*'
	makeindex $*
	if grep -s "Rerun to get cross-references right." $*.log; \
	then $(TEX) '\scrollmode \input $*'; \
	     makeindex $* ; \
	fi
%.ps : %.dvi
	dvips $(DVIPSFLAGS) $< $(OUTPUT_OPTION)
	rm -f body.tmp head.tmp

%.pdf : %.tex
	pdf$(TEX) '\scrollmode \input $*'
	makeindex $*

all:: M2book.tex

M2book.bbl : papers.bib

ifndef CYGWIN32
all:: M2book.dvi M2book.pdf
endif

M2book.tex : ../cache/Macaulay2-doc book.m2 booktex.m2
	../bin/M2 -q book.m2 '-e exit 0'
	mv M2book.tmp M2book.tex

clean :; rm -rf M2book.tex *.dvi *.log *.aux *.out
