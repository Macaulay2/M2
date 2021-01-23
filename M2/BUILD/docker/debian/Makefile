include ../ubuntu/Makefile

## Parameters
TAG = m2-debian-build
BUILD_DIR = M2/BUILD/build-docker
BUILD_OPT = -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr

deb:;       docker run $(VOLUME) -it --entrypoint "" $(TAG) bash -c "cd M2/$(BUILD_DIR); cpack -G DEB"

lintian:;   docker run $(VOLUME) -it --entrypoint "" $(TAG) bash -c "lintian -I -E --pedantic M2/$(BUILD_DIR)/Macaulay2-*.deb"
