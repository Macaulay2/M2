include ../../Makeconf
CPPFLAGS += -I../include

all : ../bin/echoout ../bin/timestmp ../bin/restart ../bin/update

../bin/echoout : echoout.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $< -o $@
../bin/timestmp : timestmp.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $< -o $@
../bin/restart : restart.c restart.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $< -o $@
../bin/update : update.c restart.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $< -o $@
