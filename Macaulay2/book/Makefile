TOPDIR = ../..
include $(TOPDIR)/Makeconf

all:: M2hbook.dvi M2book.dvi
M2book.tex : ../cache/Macaulay2.doc book.m2 booktex.m2
	../bin/M2 '-ehypertex=false' book.m2 '-e exit 0'
	mv M2book.tmp M2book.tex
M2book.dvi : M2book.tex
	tex $<
M2hbook.tex : ../cache/Macaulay2.doc book.m2 booktex.m2
	../bin/M2 '-ehypertex=true' book.m2 '-e exit 0'
	mv M2hbook.tmp M2hbook.tex
M2hbook.dvi : M2hbook.tex
	tex $<
clean :; rm -rf M2book.tex M2book.dvi M2book.log \
		M2hbook.tex M2hbook.dvi M2hbook.log
view : M2hbook.dvi; xhdvi -hl blue -bd red -cr orange M2hbook.dvi
split :
	dvips -p   1 -l 100 M2book -o M2book1.ps
	dvips -p 101 -l 200 M2book -o M2book2.ps
	dvips -p 201 -l 300 M2book -o M2book3.ps
	dvips -p 301        M2book -o M2book4.ps
