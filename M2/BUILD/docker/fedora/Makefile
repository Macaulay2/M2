include ../ubuntu/Makefile

## Parameters
RELEASE = latest
BUILD_ARGS = --build-arg RELEASE=$(RELEASE)
TAG = m2-fedora-$(RELEASE)-build
BUILD_DIR = M2/BUILD/build-docker
BUILD_OPT = -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr

rpm:;       docker run $(VOLUME) -it --entrypoint "" $(TAG) bash -c "cd M2/$(BUILD_DIR); cpack -G RPM"

rpmlint:;   docker run $(VOLUME) -it --entrypoint "" $(TAG) bash -c "rpmlint M2/$(BUILD_DIR)/Macaulay2-*.rpm"

## autotools build
## TODO: generalize this to other distros, move to toplevel Makefile

CONFIG_OPT = --with-system-gc --with-system-memtailor --with-system-mathic \
	--with-system-mathicgb --enable-download --enable-rpm --prefix=/usr

define M2_BUILD_SCRIPT_autotools
set -xe

mkdir -p M2/$(BUILD_DIR)
cd M2/$(BUILD_DIR)
$(M2_HOME)/M2/M2/autogen.sh
$(M2_HOME)/M2/M2/configure $(CONFIG_OPT)
make
endef
export M2_BUILD_SCRIPT_autotools

build-autotools: build-image
	docker run $(VOLUME) -it --entrypoint="" $(TAG) \
		bash -c "$$M2_BUILD_SCRIPT_autotools"
