TOPDIR = ../..
include $(TOPDIR)/Makeconf

BOOKNAME = Macaulay2-$(VERSION)-book

DVIPSFLAGS = -z
TEX=latex

%.bux : %.tex
	$(TEX) $*
	makeindex $*.idx
	mv $*.dvi $*-prelim.dvi
	cp $*.aux $*.bux
.PRECIOUS : %.bux
%.bbl : %.bux
	cp $*.bux $*.aux
	bibtex $*
%.dvi : %.tex
%.dvi : %.bbl
	$(TEX) $*
	makeindex $*.idx
	if grep -s "Rerun to get cross-references right." $*.log; \
	then $(TEX) $*; \
	     makeindex $*.idx ; \
	fi
%.ps : %.dvi
	dvips $(DVIPSFLAGS) $< $(OUTPUT_OPTION)
	rm -f body.tmp head.tmp

%.pdf : %.tex
	pdf$(TEX) $*
	makeindex $*.idx
	pdf$(TEX) $*

all:: $(BOOKNAME).tex

$(BOOKNAME).bbl : papers.bib

ifndef CYGWIN32
all:: $(BOOKNAME).dvi $(BOOKNAME).ps $(BOOKNAME).pdf
endif

## for M2book.pdf put these lines in texmf.cnf
# main_memory = 1263000
# hash_extra = 15000
# pool_size = 300000
# max_strings = 30000

$(BOOKNAME).tex : ../cache/Macaulay2-doc book.m2 booktex.m2
	../bin/M2 -q book.m2 '-e exit 0'
	mv M2book.tmp $(BOOKNAME).tex

clean :; rm -rf .tex *.dvi *.log *.aux *.out *.tmp *.pdf *.ps \
	*.bbl *.blg *.ilg *.ind *.toc *.idx *.bux

