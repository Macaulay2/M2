include ../../Makeconf
FILES = Makefile booktex.m2 book.m2 hypertex.m2 README
all: ../tgz/M2hbook.dvi.gz ../tgz/M2book.dvi.gz
../tgz/M2hbook.dvi.gz : M2hbook.dvi.gz ; gzip <$< >$@
../tgz/M2book.dvi.gz : M2book.dvi.gz ; gzip <$< >$@
M2book.tex : ../bin/Macaulay2.doc book.m2 booktex.m2
	../bin/M2 book.m2 '-e exit 0'
M2book.dvi : M2book.tex
	tex $<
M2hbook.tex : ../bin/Macaulay2.doc book.m2 booktex.m2 hypertex.m2
	../bin/M2 hypertex.m2 book.m2 '-e exit 0'
M2hbook.dvi : M2hbook.tex
	tex $<
allfiles : Makefile; echoout $(FILES) >allfiles
distfiles: allfiles
	cp allfiles distfiles
clean :; rm -rf M2book.tex M2book.dvi M2book.log \
		M2hbook.tex M2hbook.dvi M2hbook.log
view : M2hbook.dvi; xhdvi -hl blue -bd red -cr orange M2hbook.dvi
