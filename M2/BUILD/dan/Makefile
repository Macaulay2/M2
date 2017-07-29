# this makefile is the start of a generic makefile that always works

# we will distribute the result, so prevent aggressive optimizations based on the current architecture:
CONFIGOPTIONS += --build=$(shell ../../config/config.guess)

# detect the OS flavor
DEBIAN := $(shell if [ -d /var/lib/dpkg ] ; then echo yes ; else echo no; fi)
RPM := $(shell if [ -d /var/lib/yum ] ; then echo yes ; else echo no; fi)
MACOSX := $(shell if [ `uname -s` = Darwin ] ; then echo yes ; else echo no; fi)

DIRECTORY ?= $(shell hostname)

BRANCH := $(shell git branch --contains | grep '^\* ' | sed -e s=^..== -e s=/=.=g )
DIRECTORY := $(DIRECTORY)-$(BRANCH)

ifeq ($(DEBUG),yes)
CONFIGOPTIONS += --enable-debug --enable-memdebug --disable-optimize
DIRECTORY := $(DIRECTORY).debug
else
CONFIGOPTIONS += --enable-tarfile
CONFIGOPTIONS += --enable-strip
ifeq ($(DEBIAN),yes)
CONFIGOPTIONS += --enable-deb
endif
ifeq ($(RPM),yes)
CONFIGOPTIONS += --enable-rpm
endif
ifeq ($(MACOSX),yes)
CONFIGOPTIONS += --enable-dmg
endif
endif

include Makefile.include

# use "gmake" instead of "make", if present, because /usr/bin/make under Mac OS X is old and buggy:
# Local Variables:
# mode: makefile-gmake
# compile-command: "MAKE=$(if [ $(type -t gmake) = file ] ; then echo gmake ; else echo make ; fi) ; set -x ; time $MAKE "
# End:
