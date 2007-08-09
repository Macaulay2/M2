.PHONY : all monitor

all: configure monitor include/config.h.in.stamp
	@echo 'Makefile: now run "configure" from a temporary build directory and start "make" there'
	@echo 'Makefile: ... or run "make reconfigure" there and then start "make"'

configure : configure.ac config/files # aclocal.m4
	@ (set -x ; autoconf) || echo "a Makefile for compiling a new version of autoconf is available in ./libraries/autoconf" >&2

# autoheader's job is to make include/config.h, but if there are no changes, it doesn't touch it, 
# which means "make" will keep trying
include/config.h.in.stamp : configure.ac # aclocal.m4
	autoheader
	touch "$@"

monitor:
	@[ -f include/config.h.in ] || (set -x ; rm -f include/config.h.in.stamp)

# Local Variables:
# mode: Makefile
# compile-command: "make -f Makefile"
# End:
