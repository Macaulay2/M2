# -*- Mode: Makefile -*-

## to build:
## the usual way:
##      ./configure --prefix=/usr/local ...
##      make 
##      make install
## or
##	make Makefile.configure
##	select overrides in the file Makefile.overrides, and add some of your own
##	make
##      make install

## to rebuild configure script without using the makefile:
##	autoconf
##	autoheader

#############################################################################

default : all

include Makefile.overrides
Makefile.overrides :
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
	echo "# CONFIGURE_OPTIONS += --without-factory" >>$@
	echo "# CONFIGURE_OPTIONS += --with-gmp" >>$@
	echo "# CONFIGURE_OPTIONS += --with-gc" >>$@

# this list is the same as the one in aclocal.m4
export CONFIGURED_FILES := \
	config.Makefile \
	Makefile-run \
	Macaulay2/Makefile \
	Macaulay2/m2/Makefile \
	Macaulay2/basictests/Makefile \
	Macaulay2/book/Makefile \
	Macaulay2/c/Makefile \
	Macaulay2/c2/Makefile \
	Macaulay2/d/Makefile \
	Macaulay2/dbm/Makefile \
	Macaulay2/dumpdata/Makefile \
	Macaulay2/e/Makefile \
	Macaulay2/emacs/Makefile \
	Macaulay2/html/Makefile \
	Macaulay2/packages/Makefile \
	Macaulay2/test/Makefile \
	Macaulay2/tutorial/Makefile \
	Macaulay2/setup \
	Macaulay2/util/Makefile
#############################################################################

stage1 : configure include/config.h.in
configure : configure.in aclocal.m4
	autoconf
include/config.h.in : configure.in aclocal.m4
	autoheader
	touch $@

stage2 : include/config.h
config.status : configure Makefile.overrides
	unset CONFIG_SITE; \
	$(CONFIGURE_ENVIRON) ./configure $(CONFIGURE_OPTIONS) --no-create --cache-file=config.cache

$(CONFIGURED_FILES) include/config.h : $(CONFIGURED_FILES:=.in) config.status include/config.h.in
	./config.status
	touch include/config.h

TARGETS = all install dist check clean distclean uninstall doc port justM2 testport \
	FACTORY LIBFAC GMP GC

$(TARGETS) :: Makefile-run include/config.h
	$(MAKE) $@ -f Makefile-run

