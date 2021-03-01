# This makefile is the start of a generic makefile that works on all systems

#############################################################################
# Variables that could be overridden on the command line or in a makefile that includes this one:
#  CC
#  CXX
#  FC
#  AR
# configure script options:
#  OPTIONS
# libraries to build:
#  BUILDLIBS
# build directory name suffix
#  DIRSUFF

# careful: "make" might set CC incorrectly -- the usual values are CC=cc CXX=g++

#############################################################################
# Example:
#    time make SIGN=no DEBUG=no CC=gcc-8 CXX=g++-8 FC=gfortran-8 DIRSUFF=-gcc8
#############################################################################

# We may distribute the result, so prevent aggressive optimizations based on the current architecture:
CONFIGOPTIONS += --build=$(shell ../../config/config.guess)

# detect the OS flavor
DEBIAN := $(shell if [ -d /var/lib/dpkg ] ; then echo yes ; else echo no; fi)
RPM := $(shell if [ -d /var/lib/yum -o -d /var/lib/dnf ] ; then echo yes ; else echo no; fi)
MACOSX := $(shell if [ `uname -s` = Darwin ] ; then echo yes ; else echo no; fi)

ifeq ($(MACOSX),yes)
# if we don't do this, we may get an absolute path in currentLayout:
#    i8 : currentLayout#"factory gftables"
#    o8 = /usr/local/Cellar/singular/4.1.3p2_2/share/factory/
BUILDLIBS += gftables
endif

DIRECTORY ?= $(shell hostname)$(DIRSUFF)

BRANCH := $(shell git branch --contains | grep '^\* ' | sed -e s=^..== -e s=/=.=g )
DIRECTORY := $(DIRECTORY)-$(BRANCH)

ifeq ($(DEV),yes)
CONFIGOPTIONS += --enable-development
endif

ifeq ($(DEBUG),yes)
CONFIGOPTIONS += --enable-debug --enable-memdebug --disable-optimize
DIRECTORY := $(DIRECTORY).debug
else
CONFIGOPTIONS += --enable-strip
ifeq ($(DEBIAN),yes)
CONFIGOPTIONS += --enable-deb
else
ifeq ($(RPM),yes)
CONFIGOPTIONS += --enable-rpm
else
ifeq ($(MACOSX),yes)
# if we make a *.dmg file, we don't want to include any shared libraries in it (so we can pass the gatekeeper, eventually)
CONFIGOPTIONS += --enable-dmg --disable-shared
else
CONFIGOPTIONS += --enable-tarfile
endif
endif
endif
endif

CONFIGOPTIONS += --enable-build-libraries="$(BUILDLIBS)"

# add the extra options last
CONFIGOPTIONS += $(OPTIONS)

ifneq ($(VERBOSE),)
# make FLINT compilation verbose:
AT=
QUIET_CC=
QUIET_CXX=
QUIET_AR=
endif

$(foreach O, $(OPTLIBS),			\
	$(eval CPPFLAGS += -I$O/include)	\
	$(eval LDFLAGS += -L$O/lib))

# ifeq ($(shell uname -m),i686)
# CONFIGOPTIONS += --with-mpir-config-options="ABI=32 --build=i686-pc-linux-gnu"
# endif

include Makefile.include

test :
	: $(FC) $(CC) $(CXX) $(AR)
	: $(MACOSX) $(CONFIGOPTIONS) $(RPM) $(DEBIAn)

# Local Variables:
# mode: makefile-gmake
# compile-command: "time gmake SIGN=no DEBUG=no DEV=no "
# End:
