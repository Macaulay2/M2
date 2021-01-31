include ../Makefile

## Parameters
TAG = m2-github-actions-ubuntu-20.04-clang-10
BUILD_DIR = M2/BUILD/build-actions
BUILD_OPT = -DBLA_VENDOR=Generic -DBUILD_LIBRARIES="Givaro;FFLAS_FFPACK"

## Script for building Macaulay2
define M2_BUILD_SCRIPT
set -xe

export PATH=/usr/lib/ccache:$$PATH
export CC=`which clang-10`
export CXX=`which clang++-10`
export LIBRARY_PATH=`llvm-config-10 --libdir`

#make -j -C M2/M2/BUILD/mahrud

## Full build
mkdir -p M2/$(BUILD_DIR); cd M2/$(BUILD_DIR)
cmake -S../.. -B. -GNinja $(BUILD_OPT)
cmake --build . --target build-libraries # build-programs
cmake --build . --target M2-core
ninja M2-unit-tests
ctest -R unit-tests
#./M2 --silent -q -e "debug Core; stacktrace(); exit 0"
endef
export M2_BUILD_SCRIPT

###############################################################################

build: build-image
	docker run $(VOLUME) -it --entrypoint="" $(TAG) bash -c "$$M2_BUILD_SCRIPT"

# For anything else, run it in the build directory within the container
%:;	docker run $(VOLUME) -it --entrypoint="" $(TAG) cmake --build $(M2_HOME)/M2/$(BUILD_DIR) --target $@
