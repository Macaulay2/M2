include ../ubuntu/Makefile

## Parameters
TAG = m2-fedora-build
BUILD_DIR = M2/BUILD/build-docker
BUILD_OPT = -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr

rpm:;       docker run $(VOLUME) -it --entrypoint "" $(TAG) bash -c "cd $(BUILD_DIR); cpack -G RPM"

rpmlint:;   docker run $(VOLUME) -it --entrypoint "" $(TAG) bash -c "rpmlint $(BUILD_DIR)/Macaulay2-*.rpm"
