default: all2 tests2
TOPDIR = ../..
include $(TOPDIR)/Makeconf
CFLAGS := -Wall -g
LDFLAGS := 
check: check.o libdump.a
map.o: map.h file.h std.h warning.h
dumpdata.o: dumpdata.h map.h warning.h std.h
check.o: dumpdata.h std.h
file.o: std.h
warning.o: warning.h std.h
gdb: check; gdb check
TAGS:; etags *.h *.c
clean:; rm -f *.o check core check-data *.a libdump.a
wc:; wc -l *.c *.h
all2: check
libdump.a : libdump.a(file.o warning.o map.o dumpdata.o)
	ranlib libdump.a
tests2: check
	check dump "hi there"
	check load "look!"
ifdef NODUMPDATA
all::
tests:
else
all:: all2
tests: tests2
endif
