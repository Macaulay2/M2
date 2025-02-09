.PHONY: always
always:

BRANCH := $(shell git branch --contains | grep '^\* ' | sed -e s=^..== -e s=/=.=g )


cmake:
	echo "git branch is " $(BRANCH)
	mkdir -p builds.tmp/$@
	cd builds.tmp/$@; cmake \
		-GNinja \
		-DCMAKE_BUILD_TYPE=RelWithDebInfo \
		-DCMAKE_PREFIX_PATH="`brew --prefix libffi`;`brew --prefix`;`brew --prefix libomp`" \
		-DBUILD_NATIVE=off \
		-DCMAKE_INSTALL_PREFIX=`pwd`/installed \
		-DBUILD_TESTING=on \
		-DBUILD_DOCS=off \
		../../../..

auto:
	echo "git branch is " $(BRANCH)
	mkdir -p builds.tmp/$@
	cd builds.tmp/$@; ../../../../configure \
	        --prefix="`pwd`/installed" \
                --disable-strip \
                --enable-download

auto-debug: 
	echo "git branch is " $(BRANCH)
	mkdir -p builds.tmp/$@
	cd builds.tmp/$@; ../../../../configure \
	        --prefix="`pwd`/installed" \
		CPPFLAGS="-DENGINE_DEBUG_" \
		CXXFLAGS=" -U__GNUC_STDC_INLINE__" \
                --disable-strip \
		--enable-debug \
		--disable-optimize \
                --enable-download
auto-debug-optimize: 
	echo "git branch is " $(BRANCH)
	mkdir -p builds.tmp/$@
	cd builds.tmp/$@; ../../../../configure \
	        --prefix="`pwd`/installed" \
		CPPFLAGS="-DENGINE_DEBUG_" \
		CXXFLAGS=" -U__GNUC_STDC_INLINE__" \
                --disable-strip \
		--enable-debug \
		--enable-optimize \
                --enable-download
