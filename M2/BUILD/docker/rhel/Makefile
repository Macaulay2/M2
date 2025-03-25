include ../Makefile

## Parameters
DISTRIBUTION = rockylinux
RELEASE = 9
BUILD = autotools
BUILD_ARGS = --build-arg DISTRIBUTION=$(DISTRIBUTION) \
	--build-arg RELEASE=$(RELEASE)
TAG = m2-$(BUILD)-$(DISTRIBUTION)-$(RELEASE)-build
BUILD_DIR = M2/BUILD/build-docker

## Script for building Macaulay2 (autotools)
define M2_BUILD_SCRIPT_autotools
set -xe

git config --global --add safe.directory $(M2_HOME)/M2
mkdir -p M2/$(BUILD_DIR)
cd M2/$(BUILD_DIR)
$(M2_HOME)/M2/M2/autogen.sh
$(M2_HOME)/M2/M2/configure --with-system-gc --enable-download --enable-rpm \
	--prefix=/usr
make
endef
export M2_BUILD_SCRIPT_autotools

###############################################################################
# Build targets

build: build-image
	docker run $(VOLUME) -it --entrypoint="" $(TAG) \
		bash -c "$$M2_BUILD_SCRIPT_$(BUILD)"
