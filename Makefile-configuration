# -*- Mode: Makefile -*-

## to build:
## the usual way:
##      ./configure --prefix=/usr/local ...
##      make 
##      make install
## or
##	make configure.options
##	select options in the file configure.options, and add some of your own
##	make
##      make install

## to rebuild configure script without using the makefile:
##	autoconf
##	autoheader


TARGETS = all install dist check clean distclean uninstall doc port justM2 testport FACTORY LIBFAC GMP GC

#############################################################################
ifdef XYZZY
  If you get an error message on this line, it is because you are not using
  the 'gnu' version of the 'make' program.  The source code for it is
  available at ftp://ftp.gnu.org/pub/gnu/make.
endif
#############################################################################

default : all doc

include configure.options
configure.options :
	echo "# -*- Mode: Makefile -*-" > $@
	echo "# CONFIGURE_ENVIRON = " >>$@
	echo "# CONFIGURE_OPTIONS = " >>$@
	echo "# CONFIGURE_OPTIONS += --disable-dumpdata" >>$@
	echo "# CONFIGURE_OPTIONS += --enable-dumpdata=old" >>$@
	echo "# CONFIGURE_OPTIONS += --with-socks" >>$@
	echo "# CONFIGURE_OPTIONS += --enable-shared" >>$@
	echo "# CONFIGURE_OPTIONS += --disable-strip" >>$@
	echo "# CONFIGURE_OPTIONS += --enable-profile" >>$@
	echo "# CONFIGURE_OPTIONS += --disable-optimize" >>$@
	echo "# CONFIGURE_OPTIONS += --enable-debug" >>$@
	echo "# CONFIGURE_OPTIONS += --enable-static" >>$@
	echo "# CONFIGURE_OPTIONS += --enable-memdebug" >>$@
	echo "# CONFIGURE_OPTIONS += --enable-verbose" >>$@
	echo "# CONFIGURE_OPTIONS += --disable-gc-for-new" >>$@

export CONFIGURED_FILES := $(shell cat config.files)
% : %.in config.status
	./config.status $@
#############################################################################

configure : configure.ac aclocal.m4
	autoconf
include/config.h.in : configure.ac aclocal.m4
	autoheader
	touch $@

config.status : configure configure.options version
	unset CONFIG_SITE; \
	$(CONFIGURE_ENVIRON) ./configure $(CONFIGURE_OPTIONS) --no-create --cache-file=config.cache

include/config.h : config.status include/config.h.in
	./config.status $@
	touch $@

configuration : include/config.h $(CONFIGURED_FILES)

$(TARGETS) :: configuration
	$(MAKE) $@ -f Makefile-run
