NAME := TEMPLATE

EPS  := $(wildcard *.eps *.mps)
EPSU := $(EPSU:.eps=.epsu)
EPSU := $(wildcard *.eps)

all : process merge $(NAME)-m2.pdf
dvi : $(NAME)-m2.dvi $(EPSU)
pdf : $(NAME)-m2.pdf
ps  : $(NAME)-m2.ps

.PRECIOUS : %-m2.tex %.split %.out %.m2
DVIPSFLAGS = -z
CFLAGS = -Wall -g
HEADERS = array.mps utils.mps sub.mps
INPUTS = 
TEX = latex
%-m2.tex : %.tex %.out
	./merge $*.tex $*.out >$*-m2.tex.tmp
	mv $*-m2.tex.tmp $*-m2.tex
%.out : %.m2
	@if [ -f $*.out ] && cmp -s $*.m2 $*.old ; \
	 then touch $*.out ; \
	 else echo "running M2 on $*.m2"; \
	   M2 --silent --stop <$*.m2 >$*.out.tmp \
		&& \
	   sed -e '/^$$/N' -e 's/^\n*\(ii*[1-9][0-9]* : \)/\1/' <$*.out.tmp >$*.out && \
	   cp $*.m2 $*.old ; \
	 fi
%.m2 : %.tex
	./process $< >$@.tmp
	mv $@.tmp $@

$(NAME).tgz : Makefile $(NAME).tex $(EPS) $(INPUTS) papers.bib
	tar cfz $(NAME).tgz $^

bak : $(NAME).tgz
	rcp $(NAME).tgz osmium:

%.epsu : %.eps $(HEADERS)
	cat $^ >$@

%.bux : %.tex
	$(TEX) '\scrollmode \input $<'
	rm -f $*.dvi
	cp $*.aux $*.bux
.PRECIOUS : %.bux
%.bbl : %.bux
	cp $*.bux $*.aux
	bibtex $*
%.dvi : %.tex
%.dvi : %.bbl
	$(TEX) '\scrollmode \input $*'
	if grep -s "Rerun to get cross-references right." $*.log; \
	then $(TEX) '\scrollmode \input $*'; \
	fi
%.ps : %.dvi
	dvips $(DVIPSFLAGS) $< $(OUTPUT_OPTION)
	rm -f body.tmp head.tmp
%.pdf : %.dvi
	dvipdfm $<

clean :
	rm -f   *.dvi *.numtex *.log *.xyc *.aux *.nums *.blg *.tmp *.bbl \
		*.ps *.pdf *.bux *.old *.out *-m2.tex $(NAME).m2 merge process
$(NAME).ps : $(EPSU)
$(NAME).dvi $(NAME).bux : $(INPUTS)
$(NAME).bbl : papers.bib
print : $(NAME).ps
	lpr $<
