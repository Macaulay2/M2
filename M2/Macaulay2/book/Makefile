include ../../Makeconf
all: M2hbook.dvi M2book.dvi
M2book.tex : ../cache/Macaulay2.doc book.m2 booktex.m2
	../bin/M2 '-ehypertex=false' book.m2 '-e exit 0'
M2book.dvi : M2book.tex
	tex $<
M2hbook.tex : ../cache/Macaulay2.doc book.m2 booktex.m2
	../bin/M2 '-ehypertex=true' book.m2 '-e exit 0'
M2hbook.dvi : M2hbook.tex
	tex $<
clean :; rm -rf M2book.tex M2book.dvi M2book.log \
		M2hbook.tex M2hbook.dvi M2hbook.log
view : M2hbook.dvi; xhdvi -hl blue -bd red -cr orange M2hbook.dvi
