include ../../Makeconf
CPPFLAGS += -I../include
EXES = echoout timestmp restart update
all : $(EXES)
update.o : restart.h
clean :; rm -r $(EXES)
