TOPDIR = ../..
include $(TOPDIR)/Makeconf
# LDFLAGS =
# CFLAGS =

CPPFLAGS += -I../include
EXES := echoout timestmp restart update

all:: $(EXES)
update.o : restart.h
clean :; rm -f $(EXES) *.o $(EXES:=.exe) $(EXES:=.obj)
ifeq "$(CC)" "cl"
all:: setup
	cp setup.exe ..
endif