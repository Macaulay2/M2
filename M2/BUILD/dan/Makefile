# This makefile is the start of a generic makefile that works on all systems

#############################################################################
# Variables to override on the command line:

# set the compilers to use, and clear any values set by "make", which can be wrong:
CC  =
CXX =
FC  =

# configure script options:
OPTIONS =

# libraries to build:
BUILDLIBS = 

# build directory name suffix
DIRSUFF =

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
OPTLIBS += /usr/local/opt/readline
endif

DIRECTORY ?= $(shell hostname)$(DIRSUFF)

BRANCH := $(shell git branch --contains | grep '^\* ' | sed -e s=^..== -e s=/=.=g )
DIRECTORY := $(DIRECTORY)-$(BRANCH)

ifeq ($(DEBUG),yes)
CONFIGOPTIONS += --enable-debug --enable-memdebug --disable-optimize
DIRECTORY := $(DIRECTORY).debug
else
CONFIGOPTIONS += --enable-tarfile
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

include Makefile.include

test :
	: $(FC) $(CC) $(CXX)

# Local Variables:
# mode: makefile-gmake
# compile-command: "time make SIGN=no DEBUG=no "
# End:
