include ../ubuntu/Makefile

## Parameters
TAG = m2-fedora-build
STORAGE = `pwd`/../storage
M2_REPO = `pwd`/../../../..
M2_HOME = /home/macaulay
VOLUME = --volume $(STORAGE):$(M2_HOME) --volume $(M2_REPO):$(M2_HOME)/M2
BUILD_DIR = M2/M2/BUILD/build-docker
BUILD_OPT = -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr -DUSING_MPIR=OFF

rpm:;       docker run $(VOLUME) -it --entrypoint "" $(TAG) bash -c "cd $(BUILD_DIR); cpack -G RPM"

rpmlint:;   docker run $(VOLUME) -it --entrypoint "" $(TAG) bash -c "rpmlint $(BUILD_DIR)/Macaulay2-*.rpm"
