include ../../Makeconf

FILES := Makefile tutbook.m2 titlepage.tex \
	final/--INDEX final/*.m2 prelim/*.m2 translate.c
TUTORIALS := $(wildcard final/*.m2)
OUTPUTS := $(TUTORIALS:.m2=.out)
%.out : %.m2; ./translate <$*.m2 >$*.out
all : $(OUTPUTS)

tutbook.dvi : tutbook.tex ../book/macros.tex titlepage.tex
	tex $<
$(OUTPUTS) : translate
tutbook.tex : ../bin/Macaulay2.doc tutbook.m2 translate $(OUTPUTS)
	../bin/M2 '-e errorDepth 0' '-e writeExamples=true' tutbook.m2 '-e exit 0'
	$(MAKE) -k -C Examples -f ../../m2/Makefile.egs
	../bin/M2 -e'readExamples=true' tutbook.m2 '-e exit 0'

allfiles : Makefile prelim/*.m2 final/*.m2
	../bin/echoout $(FILES) >allfiles

clean :
	rm -rf *.dvi *.log allfiles translate final/*.out

