include ../ubuntu/Makefile

## Parameters
TAG = m2-debian-build
BUILD_DIR = M2/BUILD/build-docker
BUILD_OPT = -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr

deb:;       docker run $(VOLUME) -it --entrypoint "" $(TAG) bash -c "cd $(BUILD_DIR); cpack -G DEB"

lintian:;   docker run $(VOLUME) -it --entrypoint "" $(TAG) bash -c "lintian -I -E --pedantic $(BUILD_DIR)/Macaulay2-*.deb"
