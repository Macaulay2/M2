TOPDIR = ../..
include $(TOPDIR)/Makeconf


all :: index.html
index.html : html.m2 ../cache/Macaulay2-made ../cache/Macaulay2-doc
	rm -f ./*.html
	../bin/M2 -q html.m2 '-eexit(0)'
view :
	netscape index.html &
clean :
	rm -f ./*.html make-html
allfiles : *.jpg *.gif html.m2 Makefile
	echo $^ |tr ' ' \\012 >allfiles
