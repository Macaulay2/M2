TOPDIR = ../..
include $(TOPDIR)/Makeconf
CFLAGS := -Wall -g
LDFLAGS := 

all :: check
	check dump "hi there"
	check load "look!"
check : dumpdata.o check.o
dumpdata.o check.o : dumpdata.h

clean :
	rm -f *.o check core check-data
