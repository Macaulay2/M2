TOPDIR = ../..
include $(TOPDIR)/Makeconf
CFLAGS += -Wall -g

all :: check
	check
check : dumpdata.o check.o
dumpdata.o check.o : dumpdata.h

clean :
	rm -f *.o check core check-data
