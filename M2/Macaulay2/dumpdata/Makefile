TOPDIR = ../..
include $(TOPDIR)/Makeconf
CFLAGS += -Wall -g
CPPFLAGS += -I$(INCDIR)
LDFLAGS += -L$(LIBDIR)
LOADLIBES += -lgc

all :: check
	check
check : dumpdata.o check.o
dumpdata.o check.o : dumpdata.h

