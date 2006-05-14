.PHONY : all

all: configure monitor include/config.h.in.stamp
	@echo 'Makefile: now run "configure" from a temporary build directory and start "make" there'

configure : configure.ac config/files # aclocal.m4
	autoconf

# autoheader's job is to make include/config.h, but if there are no changes, it doesn't touch it, 
# which means "make" will keep trying
include/config.h.in.stamp : configure.ac # aclocal.m4
	autoheader
	touch "$@"

monitor:
	[ -f include/config.h.in ] || rm -f include/config.h.in.stamp

# Local Variables:
# mode: Makefile
# compile-command: "make -f Makefile"
# End:
