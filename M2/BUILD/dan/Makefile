# This makefile is the start of a generic makefile that always works

# To set the compilers override these variables.
# (We don't want to use CC, CXX, and FC, because "make" may set them)
_CC  =
_CXX =
_FC  =

# We may distribute the result, so prevent aggressive optimizations based on the current architecture:
CONFIGOPTIONS += --build=$(shell ../../config/config.guess)

# detect the OS flavor
DEBIAN := $(shell if [ -d /var/lib/dpkg ] ; then echo yes ; else echo no; fi)
RPM := $(shell if [ -d /var/lib/yum ] ; then echo yes ; else echo no; fi)
MACOSX := $(shell if [ `uname -s` = Darwin ] ; then echo yes ; else echo no; fi)

BUILDLIBS = 

ifeq ($(MACOSX),yes)
OPTLIBS += /usr/local/opt/readline
endif

DIRECTORY ?= $(shell hostname)

BRANCH := $(shell git branch --contains | grep '^\* ' | sed -e s=^..== -e s=/=.=g )
DIRECTORY := $(DIRECTORY)-$(BRANCH)

ifeq ($(DEBUG),yes)
CONFIGOPTIONS += --enable-debug --enable-memdebug --disable-optimize
DIRECTORY := $(DIRECTORY).debug
else
SIGN=yes
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

CONFIGOPTIONS += --enable-build-libraries="$(BUILDLIBS)"

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

# Local Variables:
# mode: makefile-gmake
# compile-command: "time make "
# End:
