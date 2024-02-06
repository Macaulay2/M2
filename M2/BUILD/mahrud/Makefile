# This Makefile uses a pre-existing CMake build so that
# the autotools build doesn't have to build any libraries

# Set the path to the CMake build directory here
CMAKE_PREFIX_PATH       = $(shell pwd)/../build
# Or the brew prefix
export HOMEBREW_PREFIX  = $(shell brew --prefix)
CMAKE_PREFIX_PATH       = $(HOMEBREW_PREFIX)

# Set the exec_prefix here in order to also find the programs
# TODO: perhaps store the distribution architecture in a file
ARCH                    = $(shell basename $(CMAKE_PREFIX_PATH)/usr-dist/x*)
PROGRAMSDIR             = usr-dist/$(ARCH)/libexec/Macaulay2/bin
BINDIR                  = $(shell [ -e $(CMAKE_PREFIX_PATH)/$(PROGRAMSDIR) ] && echo $(PROGRAMSDIR) || echo bin)

export       CPPFLAGS = -I$(CMAKE_PREFIX_PATH)/include
export        LDFLAGS = -L$(CMAKE_PREFIX_PATH)/lib
export PKG_CONFIG_PATH := $(CMAKE_PREFIX_PATH)/lib/pkgconfig:$(PKG_CONFIG_PATH)
export LD_LIBRARY_PATH := $(CMAKE_PREFIX_PATH)/lib:$(LD_LIBRARY_PATH)
export            PATH := $(CMAKE_PREFIX_PATH)/$(BINDIR):$(PATH)

autotools-test-build:
	echo $(PATH)
	make -j4 -C ../.. && \
	mkdir -p build && cd build && \
	../../../configure --prefix=/usr --enable-download && \
	make -j4 -C Macaulay2

macos-prep-brew:
	xcode-select -p >/dev/null 2>&1 || xcode-select --install
	brew update
	brew upgrade
	brew tap macaulay2/tap
	brew install autoconf automake bison ccache cmake ctags libtool ninja pkg-config
	brew install --only-dependencies macaulay2/tap/M2

# HOMEBREW_DEPS=`brew deps --1 --include-optional macaulay2/tap/M2 | tr '\n' ';'`
# OPT_PREFIX=$HOMEBREW_PREFIX/opt/${HOMEBREW_DEPS//;/;$HOMEBREW_PREFIX/opt/}

HOMEBREW_DEPS           = $(shell brew deps --1 --include-optional macaulay2/tap/M2 | tr '\n' ' ')
OPT_PREFIX              = $(subst $() $(),;,$(HOMEBREW_DEPS:%=$(HOMEBREW_PREFIX)/opt/%))

cmake-build-with-brew:
	cmake -GNinja -S../.. -Bcmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH="$(OPT_PREFIX)"
	cmake --build cmake --target build-libraries build-programs
	cmake --build cmake --target install-packages
	cmake --build cmake --target M2-emacs

#TODO: can we skip building factory? use FTABLESDIR=/usr/share/factory/ 
