# this makefile is readable only by the gnu make program
############################## main target
CC=gcc
all : scc1
include ../../Makeconf
############################################
%.c        : %.y
	$(YACC) $(YFLAGS) $^
	mv y.tab.c $*.c
############################################################
.PHONY : clean all install
# YACC := yacc
YACC := bison -o y.tab.c
YFLAGS := -vdlt
LOADLIBES := -lm -L${LIBDIR} -lgc
##############################
CNAMES := scc1.c readfile.c error.c dictionary.c list.c cprint.c \
	  type.c chk.c compat.c
HFILES := $(CNAMES:.c=.h) scc.h grammar.h
OFILES := $(CNAMES:.c=.o) grammar.o
SRCFILES := $(HFILES) $(CNAMES) grammar.y
OTHERS := makefile.dos scc.template Makefile README COPYRIGHT
ALLFILES := $(SRCFILES) $(OTHERS) grammar.c
OPTIMIZE := -Winline -finline-functions -O2
INCLUDES := -I${INCDIR}
ifeq "$(OS)" "MSDOS"
  WARNINGS := -Wall -Wshadow -Wcast-qual -Wtraditional
else
  WARNINGS := -Wall -Wshadow -Wcast-qual -Wtraditional -ansi
endif
GCFLAGS  := -g -O $(INCLUDES)
CFLAGS   := $(GCFLAGS) $(WARNINGS)
############################## compiling
scc1 : $(OFILES)
$(OFILES) : $(HFILES)
grammar.o : grammar.c;	 	$(CC) $(GCFLAGS) -c $<
############################## installation 
#scc : scc.template Makefile
#	rm -f $@
#	sed -e s@--LIBDIR--@.@g \
#	    -e s@--SCC1--@../c/scc1@g \
#	    -e s@--MMGR--@.@g \
#	    <$< >$@
#	chmod a+x $@
############################## miscellaneous
allfiles: Makefile;		echo $(ALLFILES) |tr ' ' \\012 >allfiles
TAGS: $(SRCFILES);		etags $(SRCFILES)
prn :;				enscript -2r $(SRCFILES)
wc:;				wc -l $(ALLFILES)
############################## cleaning
# we purposely don't delete grammar.c so it can be used for porting
clean :
	rm -f *.o scc1 scc.a grammar.output grammar.tab.h \
		allfiles scc.install TAGS scc core
