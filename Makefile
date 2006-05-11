.PHONY : all

all: configure include/config.h.in
	@echo 'Makefile: $^: up to date'
	@echo 'Makefile: run "configure" from a build directory and start "make" there'

configure : configure.ac config/files # aclocal.m4
	autoconf

include/config.h.in : configure.ac # aclocal.m4
	autoheader

# Local Variables:
# mode: Makefile
# compile-command: "make "
# End:
