include ../Makefile

## Parameters
DISTRIBUTION = rockylinux
RELEASE = 8
BUILD = autotools
TAG = m2-$(BUILD)-$(DISTRIBUTION)-$(RELEASE)-build
BUILD_DIR = M2/BUILD/build-docker

## Script for building Macaulay2 (autotools)
define M2_BUILD_SCRIPT_autotools
set -xe

git config --global --add safe.directory $(M2_HOME)/M2
make -C M2/M2
mkdir -p M2/$(BUILD_DIR)
cd M2/$(BUILD_DIR)
$(M2_HOME)/M2/M2/configure --with-system-gc --with-python --enable-download
make
endef
export M2_BUILD_SCRIPT_autotools

###############################################################################
# Build targets

build: build-image
	docker run $(VOLUME) -it --entrypoint="" $(TAG) \
		bash -c "$$M2_BUILD_SCRIPT_$(BUILD)"
