.PHONY : all monitor

all: check-autoconf configure monitor include/config.h.in.stamp
	@echo '--The configure script in this directory has been prepared.'
	@echo '--To configure and build Macaulay 2:'
	@echo '--'
	@echo '--   mkdir BUILD/normal'
	@echo '--   cd BUILD/normal'
	@echo '--   ../../configure --prefix=/usr/local'
	@echo '--   make install'
	@echo '--'
	@echo '--To reconfigure and try again:'
	@echo '--'
	@echo '--   cd BUILD/normal'
	@echo '--   make reconfigure install'
	@echo '--'
	@echo '--To see all the options to "configure":'
	@echo '--'
	@echo '--   ./configure --help'

MAJOR = 2
MINOR = 60
# version 2.60 was the first one with the macro AC_PROG_MKDIR_P
AUTOCONF_VERSION = $(MAJOR).$(MINOR)

PATH := autoconf/final/bin:$(PATH)
export PATH

get-autoconf :
	$(MAKE) -C autoconf
	$(MAKE)

rm-autoconf :; $(MAKE) -C autoconf clean

autoconf-absent : autoconf-absent-msg autoconf-msg 
autoconf-absent-msg :
	@ echo "=============================================================================" >&2
	@ echo "Error: the autoconf program is not installed on your system." >&2

autoconf-old : autoconf-old-msg autoconf-msg 
autoconf-old-msg :
	@ echo "=============================================================================" >&2
	@ echo "error: the autoconf program installed on your system (`type autoconf`) is too old." >&2

autoconf-msg :
	@ echo "" >&2
	@ echo "Please install or update autoconf: the version (`autoconf -V | sed -e '1s/.*\([1-9][0-9]*\..*\)/\1/' -e '2,$$d'`) should be at least $(AUTOCONF_VERSION)." >&2
	@ echo "" >&2
	@ echo "Alternatively, type" >&2
	@ echo "" >&2
	@ echo "    make get-autoconf" >&2
	@ echo "" >&2
	@ echo "for a recent version to be downloaded and compiled automatically." >&2
	@ echo "=============================================================================" >&2
	@ false

check-autoconf :
	@ type autoconf >/dev/null || $(MAKE) autoconf-absent
	@ test `autoconf -V | sed -e '1s/.* \([1-9][0-9]*\)\..*/\1/' -e '2,$$d'` -gt $(MAJOR) \
	  || \
	    test `autoconf -V | sed -e '1s/.* \([1-9][0-9]*\)\..*/\1/' -e '2,$$d'`  =  $(MAJOR) \
	    && \
	    test `autoconf -V | sed -e '1s/.*\.\([1-9][0-9]*\).*/\1/' -e '2,$$d'`   -ge $(MINOR) || $(MAKE) autoconf-old

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
