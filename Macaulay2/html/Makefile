TOPDIR = ../..
include $(TOPDIR)/Makeconf


all :: up.gif next.gif previous.gif top.gif index.gif index.html

# for gimp to find this *.scm file, put
# (script-fu-path "${gimp_dir}/scripts:${gimp_data_dir}/scripts:/home/geometry/dan/src/M2/Macaulay2/html")
# into ~/.gimp/gimprc

up.gif : Macaulay2-button.scm; gimp -b "(Macaulay2-button \"Up\" \"up.gif\")"  -b '(gimp-quit 0)'
next.gif : Macaulay2-button.scm; gimp -b "(Macaulay2-button \"Next\" \"next.gif\")"  -b '(gimp-quit 0)'
previous.gif : Macaulay2-button.scm; gimp -b "(Macaulay2-button \"Previous\" \"previous.gif\")"  -b '(gimp-quit 0)'
index.gif : Macaulay2-button.scm; gimp -b "(Macaulay2-button \"Index\" \"index.gif\")"  -b '(gimp-quit 0)'
top.gif : Macaulay2-button.scm; gimp -b "(Macaulay2-button \"Top\" \"top.gif\")"  -b '(gimp-quit 0)'

index.html : html.m2 ../cache/Macaulay2-made ../cache/Macaulay2-doc
	rm -f ./*.html
	../bin/M2 -q html.m2 '-eexit(0)'
view :
	netscape index.html &
clean :
	rm -f ./*.html make-html
allfiles : *.jpg *.gif html.m2 Makefile
	echo $^ |tr ' ' \\012 >allfiles
