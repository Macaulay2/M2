# This Makefile uses a pre-existing CMake build so that
# the autotools build doesn't have to build any libraries

# Set the path to the CMake build directory here
CMAKE_BUILD_DIR       = $(shell pwd)/../build

# Set the exec_prefix here in order to also find the programs
# TODO: perhaps store the distribution architecture in a file
ARCH                  = $(shell basename $(CMAKE_BUILD_DIR)/usr-dist/x*)

export CPPFLAGS       = -I$(CMAKE_BUILD_DIR)/usr-host/include
export LDFLAGS        = -L$(CMAKE_BUILD_DIR)/usr-host/lib
export PATH            := $(CMAKE_BUILD_DIR)/usr-dist/$(ARCH)/libexec/Macaulay2/bin:$(PATH)
export PKG_CONFIG_PATH := $(CMAKE_BUILD_DIR)/usr-host/lib/pkgconfig:$(PKG_CONFIG_PATH)

all:
	make -j4 -C ../.. && \
	mkdir -p build && cd build && \
	../../../configure --prefix=/usr --enable-download && \
	make -j4 -C Macaulay2

#TODO: can we skip building factory? use FTABLESDIR=/usr/share/factory/ 
