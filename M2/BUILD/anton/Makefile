.PHONY: always
always:

BRANCH := $(shell git branch --contains | grep '^\* ' | sed -e s=^..== -e s=/=.=g )

cmake:
	echo "git branch is " $(BRANCH)
	mkdir -p builds.tmp/$@
	cd builds.tmp/$@; cmake \
		-GNinja \
		-DCMAKE_BUILD_TYPE=RelWithDebInfo \
		-DBUILD_TESTING=on \
		-DBUILD_DOCS=on \
		../../../..
#		-DLINTING=on

auto:
	echo "git branch is " $(BRANCH)
	mkdir -p builds.tmp/$@
	cd builds.tmp/$@; ../../../../configure \
	        --prefix="`pwd`/installed" \
                --disable-strip \
                --enable-download

