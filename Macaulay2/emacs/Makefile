include ../Makefile.develop

ELFILES = M2-mode.el M2.el
ALLFILES = makesyms.m2 Makefile $(ELFILES) emacs.hlp emacs.m2

all: M2-symbols.el

M2-symbols.el : ../bin/Macaulay2-$(ARCH).data makesyms.m2
	../bin/M2 makesyms.m2 '-e exit 0'

allfiles : Makefile; 
	echo $(ALLFILES) | tr ' ' '\012' >allfiles
