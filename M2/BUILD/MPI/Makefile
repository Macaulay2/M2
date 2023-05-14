.PHONY: always
always:

BRANCH := $(shell git branch --contains | grep '^\* ' | sed -e s=^..== -e s=/=.=g )

#######################
## cmake build files ##
#######################

auto-ubuntu:
	echo "git branch is " $(BRANCH)
	mkdir -p builds.tmp/$@
	cd builds.tmp/$@; ../../../../configure \
		CC=mpicc CXX=mpic++ \
		--with-MPI \
		--prefix="`pwd`/installed" \
                --disable-strip \
                --enable-download
auto-ubuntu-debug:
	echo "git branch is " $(BRANCH)
	mkdir -p builds.tmp/$@
	cd builds.tmp/$@; ../../../../configure \
		CC=mpicc CXX=mpic++ \
		--with-MPI \
		--prefix="`pwd`/installed" \
                --disable-strip \
                --enable-download \
		--enable-debug \
		--disable-optimize
cmake-ubuntu:
	echo "git branch is " $(BRANCH)
	mkdir -p builds.tmp/$@
	cd builds.tmp/$@; cmake \
		-GNinja \
		-DCMAKE_C_COMPILER=mpicc \
		-DCMAKE_CXX_COMPILER=mpic++ \
		-DWITH_MPI=ON\
		-DCMAKE_BUILD_TYPE=RelWithDebInfo \
		-DBUILD_TESTING=on \
		-DBUILD_DOCS=on \
		../../../..

cmake-ubuntu-no-WITH_MPI:
	echo "git branch is " $(BRANCH)
	mkdir -p builds.tmp/$@
	cd builds.tmp/$@; cmake \
		-GNinja \
		-DCMAKE_C_COMPILER=mpicc \
		-DCMAKE_CXX_COMPILER=mpic++ \
		-DCMAKE_BUILD_TYPE=RelWithDebInfo \
		-DBUILD_TESTING=on \
		-DBUILD_DOCS=on \
		../../../..
