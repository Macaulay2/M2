include ../ubuntu/Makefile
include ../autotools/Makefile

## Parameters
RELEASE = latest
BUILD_ARGS = --build-arg RELEASE=$(RELEASE)
TAG = m2-fedora-$(RELEASE)-build
BUILD_DIR = M2/BUILD/build-docker
BUILD_OPT = -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr

rpm:;       docker run $(VOLUME) -it --entrypoint "" $(TAG) bash -c "cd M2/$(BUILD_DIR); cpack -G RPM"

rpmlint:;   docker run $(VOLUME) -it --entrypoint "" $(TAG) bash -c "rpmlint M2/$(BUILD_DIR)/Macaulay2-*.rpm"

CONFIG_OPT = --with-system-libs --enable-download --enable-rpm --prefix=/usr
