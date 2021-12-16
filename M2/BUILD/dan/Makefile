# This makefile is the start of a generic makefile that works on all systems

#############################################################################
# Variables that could be overridden on the command line or in a makefile that includes this one:
#  CC
#  CXX
FC = gfortran
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

# detect the OS flavor
DEBIAN := $(shell if [ -d /var/lib/dpkg ] ; then echo yes ; else echo no; fi)
RPM := $(shell if [ -d /var/lib/yum -o -d /var/lib/dnf ] ; then echo yes ; else echo no; fi)
MACOSX := $(shell if [ `uname -s` = Darwin ] ; then echo yes ; else echo no; fi)

# temporary (2021-11-17) : to get 2.8.4 built by us, avoiding a bug in 2.8.3
BUILDLIBS += flint

# to get the Python package working
# for Mac OS X:
ifeq ($(MACOSX),yes)
# CONFIGOPTIONS += --with-python=3.9
# LDFLAGS  += -L/usr/local/opt/python@3.9/Frameworks/Python.framework/Versions/3.9/lib/python3.9/config-3.9-darwin
# CPPFLAGS += -I/usr/local/opt/python@3.9/Frameworks/Python.framework/Versions/3.9/include/python3.9
else 
CONFIGOPTIONS += --with-python
endif

# We may distribute the result, so prevent aggressive optimizations based on the current architecture:
CONFIGOPTIONS += --build=$(shell ../../config/config.guess)

ifeq ($(MACOSX),yes)
# # if we don't do this, we may get an absolute path in currentLayout:
# #    i8 : currentLayout#"factory gftables"
# #    o8 = /usr/local/Cellar/singular/4.1.3p2_2/share/factory/
# BUILDLIBS += gftables nauty factory csdp 4ti2 gfan normaliz topcom
# on the other hand, we can move the *.dylib files out of the way

# we need an old version of tbb
# brew install tbb@2020
LDFLAGS  += "-L/usr/local/opt/tbb@2020/lib"
CPPFLAGS += "-I/usr/local/opt/tbb@2020/include"

endif

ifeq ($(DEBIAN),yes)
# cohomcalg, as provided in a debian package, has a bug.  See https://github.com/Macaulay2/M2/issues/2158
BUILDLIBS += cohomcalg
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
CONFIGOPTIONS += --enable-dmg
else
# CONFIGOPTIONS += --enable-tarfile
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
