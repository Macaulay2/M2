include ../../Makeconf

all : index.html
index.html : ../bin/Macaulay2-$(ARCH).data html.m2
	rm -f ./*.html
	../bin/M2 html.m2 '-eexit(0)'
view :
	netscape index.html &
clean :
	rm -f ./*.html make-html
allfiles : *.jpg *.gif html.m2 Makefile
	echo $^ |tr ' ' \\012 >allfiles
