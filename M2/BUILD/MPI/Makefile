.PHONY: always
always:

BRANCH := $(shell git branch --contains | grep '^\* ' | sed -e s=^..== -e s=/=.=g )

#######################
## cmake build files ##
#######################

cmake-appleclang:
	echo "git branch is " $(BRANCH)
	mkdir -p builds.tmp/cmake-appleclang
	cd builds.tmp/cmake-appleclang; cmake \
		-GNinja \
		-DCMAKE_C_COMPILER=mpicc \
		-DCMAKE_CXX_COMPILER=mpiCC \
		-DCMAKE_BUILD_TYPE=RelWithDebInfo \
		-DBUILD_TESTING=on \
		-DBUILD_DOCS=on \
		../../../..
#		-DLINTING=on

