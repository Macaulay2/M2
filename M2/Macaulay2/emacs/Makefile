TOPDIR = ../..
include $(TOPDIR)/Makeconf


ELFILES = M2-mode.el M2.el
ALLFILES = makesyms.m2 Makefile $(ELFILES) emacs.hlp emacs.m2

all:: M2-symbols.el

M2-symbols.el : ../cache/Macaulay2.doc makesyms.m2
	../bin/M2 -silent makesyms.m2 '-e exit 0'

all:: emacs.hlp
emacs.hlp: ../cache/Macaulay2.doc makehlp.m2
	../bin/M2 -silent makehlp.m2 '-e exit 0'

all:: emacs.m2
emacs.m2: ../cache/Macaulay2.doc makem2.m2
	../bin/M2 -silent makem2.m2 '-e exit 0'

allfiles : Makefile; 
	echo $(ALLFILES) | tr ' ' '\012' >allfiles

clean:
	rm -f M2-symbols.el

