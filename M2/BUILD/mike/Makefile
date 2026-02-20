.PHONY: always
always:

BRANCH := $(shell git branch --contains | grep '^\* ' | sed -e s=^..== -e s=/=.=g )

BREWPREFIX := `brew --prefix`

cmake-appleclang:
	echo "git branch is " $(BRANCH)
	mkdir -p builds.tmp/cmake-appleclang
	cd builds.tmp/cmake-appleclang; cmake \
		-GNinja \
		-DCMAKE_BUILD_TYPE=RelWithDebInfo \
		-DCMAKE_PREFIX_PATH="/opt/homebrew/opt/mpi;/opt/homebrew/opt/bdw-gc;/opt/homebrew/opt/boost;/opt/homebrew/opt/cohomcalg;/opt/homebrew/opt/csdp;/opt/homebrew/opt/eigen;/opt/homebrew/opt/factory;/opt/homebrew/opt/fflas-ffpack;/opt/homebrew/opt/flint;/opt/homebrew/opt/fourtitwo;/opt/homebrew/opt/frobby;/opt/homebrew/opt/gdbm;/opt/homebrew/opt/gfan;/opt/homebrew/opt/givaro;/opt/homebrew/opt/gmp;/opt/homebrew/opt/libffi;/opt/homebrew/opt/libomp;/opt/homebrew/opt/lrs;/opt/homebrew/opt/mpfi;/opt/homebrew/opt/mpfr;/opt/homebrew/opt/mpsolve;/opt/homebrew/opt/msolve;/opt/homebrew/opt/nauty;/opt/homebrew/opt/node;/opt/homebrew/opt/normaliz;/opt/homebrew/opt/ntl;/opt/homebrew/opt/python;/opt/homebrew/opt/readline;/opt/homebrew/opt/tbb;/opt/homebrew/opt/topcom;/opt/homebrew/opt/" \
		-DBUILD_NATIVE=off \
		-DCMAKE_INSTALL_PREFIX=`pwd`/installed \
		-DBUILD_TESTING=on \
		-DBUILD_DOCS=on \
		-DCMAKE_EXPORT_COMPILE_COMMANDS=true \
		../../../..

cmake-debug-appleclang:
	echo "git branch is " $(BRANCH)
	mkdir -p builds.tmp/cmake-debug-appleclang
	cd builds.tmp/cmake-debug-appleclang; cmake \
		-GNinja \
		-DCMAKE_BUILD_TYPE=Debug \
		-DCMAKE_PREFIX_PATH="/opt/homebrew/opt/mpi;/opt/homebrew/opt/bdw-gc;/opt/homebrew/opt/boost;/opt/homebrew/opt/cohomcalg;/opt/homebrew/opt/csdp;/opt/homebrew/opt/eigen;/opt/homebrew/opt/factory;/opt/homebrew/opt/fflas-ffpack;/opt/homebrew/opt/flint;/opt/homebrew/opt/fourtitwo;/opt/homebrew/opt/frobby;/opt/homebrew/opt/gdbm;/opt/homebrew/opt/gfan;/opt/homebrew/opt/givaro;/opt/homebrew/opt/gmp;/opt/homebrew/opt/libffi;/opt/homebrew/opt/libomp;/opt/homebrew/opt/lrs;/opt/homebrew/opt/mpfi;/opt/homebrew/opt/mpfr;/opt/homebrew/opt/mpsolve;/opt/homebrew/opt/msolve;/opt/homebrew/opt/nauty;/opt/homebrew/opt/node;/opt/homebrew/opt/normaliz;/opt/homebrew/opt/ntl;/opt/homebrew/opt/python;/opt/homebrew/opt/readline;/opt/homebrew/opt/tbb;/opt/homebrew/opt/topcom;/opt/homebrew/opt/" \
		-DBUILD_NATIVE=off \
		-DCMAKE_INSTALL_PREFIX=`pwd`/installed \
		-DBUILD_TESTING=on \
		-DBUILD_DOCS=off \
		-DCMAKE_EXPORT_COMPILE_COMMANDS=true \
		../../../..

# this one currently fails.  can't build lrslib.  gmp and/or openmp problem?
arm64-appleclang : always
	mkdir -p builds.tmp/arm64-appleclang
	cd builds.tmp/arm64-appleclang; ../../../../configure \
	--enable-dmg \
	--enable-download --with-system-gc \
	--with-system-memtailor --with-system-mathic \
	--with-system-mathicgb \
	LDFLAGS="-L`brew --prefix`/lib  \
		-L`brew --prefix factory`/lib \
		-L`brew --prefix libomp`/lib  \
		-L`brew --prefix readline`/lib \
		-L`brew --prefix python`/Frameworks/Python.framework/Versions/3.13/lib" \
	CPPFLAGS="-I`brew --prefix`/include   \
	    -I`brew --prefix`/include/cddlib    \
		-I`brew --prefix factory`/include   \
		-I`brew --prefix libomp`/include    \
		-I`brew --prefix readline`/include" \
	F77="gfortran-15" \
	PKG_CONFIG_PATH="`brew --prefix factory`/lib/pkgconfig"

arm64-debug-appleclang : always
	mkdir -p builds.tmp/arm64-debug-appleclang
	cd builds.tmp/arm64-debug-appleclang; ../../../../configure \
		--disable-strip \
		--enable-debug \
		--enable-memdebug \
		--disable-optimize \
		--enable-download --with-system-gc \
		--with-system-memtailor --with-system-mathic \
		--with-system-mathicgb \
		LDFLAGS="-L`brew --prefix`/lib  \
			-L`brew --prefix factory`/lib \
			-L`brew --prefix libomp`/lib  \
			-L`brew --prefix readline`/lib \
			-L`brew --prefix python`/Frameworks/Python.framework/Versions/3.13/lib" \
		CPPFLAGS="-I`brew --prefix`/include   \
		    -I`brew --prefix`/include/cddlib    \
			-I`brew --prefix factory`/include   \
			-I`brew --prefix libomp`/include    \
			-I`brew --prefix readline`/include" \
		F77="gfortran-15" \
		PKG_CONFIG_PATH="`brew --prefix factory`/lib/pkgconfig"

# todo: add profiling builds
