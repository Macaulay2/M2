.PHONY: always

always:

# For icc compiler: set:
#    CC="icc" CXX="icpc"

#		--enable-scscp

################
## mac os x ####
################
darwin64-gcc6 : always
	mkdir -p builds.tmp/darwin64-gcc6
	cd builds.tmp/darwin64-gcc6; ../../../../configure \
		CC=gcc-6 CXX=g++-6 \
		CPPFLAGS="-I/usr/local/include -I/usr/local/opt/readline/include" \
		LDFLAGS="-L/usr/local/lib -L/usr/local/opt/readline/lib" \
		--build=x86_64-apple-darwin \
		--prefix="`pwd`/installed" \
		--disable-strip \
		--enable-dmg \
		--enable-download \
		--enable-build-libraries="gc mpir flint pari ntl"

darwin64-gcc5 : always
	mkdir -p builds.tmp/darwin64-gcc5
	cd builds.tmp/darwin64-gcc5; ../../../../configure \
		CC=gcc-5 CXX=g++-5 \
		CPPFLAGS="-I/usr/local/include -I/usr/local/opt/readline/include" \
		LDFLAGS="-L/usr/local/lib -L/usr/local/opt/readline/lib" \
		--build=x86_64-apple-darwin \
		--prefix="`pwd`/installed" \
		--disable-strip \
		--enable-dmg \
		--enable-download \
		--enable-build-libraries="gc mpir flint pari ntl"

darwin64-gcc4.9 : always
	mkdir -p builds.tmp/darwin64-gcc4.9
	cd builds.tmp/darwin64-gcc4.9; ../../../../configure \
		CC="gcc-4.9 -m64" CXX="g++-4.9 -m64" \
		--build=x86_64-apple-darwin \
		--prefix="`pwd`/installed" \
		--disable-strip \
		--enable-dmg \
		--enable-download \
		--enable-build-libraries="gc mpir flint pari ntl"

#		CPPFLAGS="-I/usr/local/include -I/usr/local/opt/readline/include" \
#		LDFLAGS="-L/usr/local/lib -L/usr/local/opt/readline/lib" \

#  CXX="clang++-3.8 -stdlib=libc++"
#  CXXFLAGS="$CXXFLAGS -nostdinc++ -I/usr/local/opt/llvm38/lib/llvm-3.8/include/c++/v1"
#  LDFLAGS="$LDFLAGS -L/usr/local/opt/llvm38/lib/llvm-3.8/lib"

darwin64-clang3.8 : always
	mkdir -p builds.tmp/darwin64-clang3.8
	cd builds.tmp/darwin64-clang3.8; ../../../../configure \
		CXX="clang++-3.8 -stdlib=libc++" \
		CC="clang-3.8 -stdlib=libc++" \
		--build=x86_64-apple-darwin \
		--prefix="`pwd`/installed" \
		--disable-strip \
		--enable-dmg \
		--enable-download \
		--enable-build-libraries="gc mpir flint pari ntl"


darwin64-debug : always
	mkdir -p builds.tmp/darwin64-debug
	cd builds.tmp/darwin64-debug; ../../../../configure \
		CC="gcc-4.9 -m64" CXX="g++-4.9 -m64" \
		CPPFLAGS="-I/usr/local/include -I/usr/local/opt/readline/include -DENGINE_DEBUG_" \
		LDFLAGS="-L/usr/local/lib -L/usr/local/opt/readline/lib" \
		CXXFLAGS=" -U__GNUC_STDC_INLINE__" \
		--prefix="`pwd`/installed" \
		--build=x86_64-apple-darwin \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc mpir flint pari ntl"

darwin64-profile : always
	mkdir -p builds.tmp/darwin64-profile
	cd builds.tmp/darwin64-profile; ../../../../configure \
		CC="gcc-4.9 -m64" CXX="g++-4.9 -m64" \
		CPPFLAGS="-I/usr/local/include -I/usr/local/opt/readline/include" \
		LDFLAGS="-L/usr/local/lib -L/usr/local/opt/readline/lib" \
		--build=x86_64-apple-darwin \
		--prefix="`pwd`/installed" \
		--enable-profile \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc mpir flint ntl"

darwin64-clang : always
	mkdir -p builds.tmp/darwin64-clang
	cd builds.tmp/darwin64-clang; ../../../../configure \
		CC="clang -m64" CXX="clang++ -m64" \
		--disable-gfan \
		--disable-frobby \
		CPPFLAGS="-I/usr/local/include -I/usr/local/opt/readline/include" \
		LDFLAGS="-L/usr/local/lib -L/usr/local/opt/readline/lib" \
		--build=x86_64-apple-darwin \
		--prefix="`pwd`/installed" \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc mpir flint pari ntl"

darwin64-debug-clang : always
	mkdir -p builds.tmp/darwin64-debug-clang
	cd builds.tmp/darwin64-debug-clang; ../../../../configure \
		CC="clang -m64" CXX="clang++ -m64" \
		--disable-gfan \
		--disable-frobby \
		CPPFLAGS="-I/usr/local/include -I/usr/local/opt/readline/include -DENGINE_DEBUG_" \
		LDFLAGS="-L/usr/local/lib -L/usr/local/opt/readline/lib" \
		CXXFLAGS=" -U__GNUC_STDC_INLINE__" \
		--prefix="`pwd`/installed" \
		--build=x86_64-apple-darwin \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc mpir flint pari ntl"

###################
## linux ##########
###################
linux-opt64 : always
	mkdir -p builds.tmp/opt64
	cd builds.tmp/opt64; ../../../../configure \
		--prefix="`pwd`/installed" \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc mpir flint pari ntl"

linux-debug64 : always
	mkdir -p builds.tmp/debug64
	cd builds.tmp/debug64; ../../../../configure \
		--prefix="`pwd`/installed" \
		CPPFLAGS="-DENGINE_DEBUG_" \
		CXXFLAGS=" -U__GNUC_STDC_INLINE__" \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc mpir flint pari ntl"

linux-profile64 : always
	mkdir -p builds.tmp/profile64
	cd builds.tmp/profile64; ../../../../configure \
		--prefix="`pwd`/installed" \
		--disable-strip \
		--enable-profile \
		--enable-download \
		--enable-build-libraries="gc mpir flint pari ntl"

# make sure that CC and CXX are set in environment variables
#		CC="gcc -m64" CXX="g++ -m64" \

#		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory frobby pari"


#		CC="gcc -m64" CXX="g++ -m64" \
#		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory frobby pari"
#       --enable-fplll
#		--enable-linbox
# for adding tbb in for mathicgb:
#		LDFLAGS="-L/usr/local/lib -ltbb"

#               CPPFLAGS="-I/usr/local/include" LDFLAGS="-L/usr/local/lib -ltbb" \


######## below this line is older ######################

dan-ubuntu64: always
	mkdir -p dan-ubuntu64
	cd dan-ubuntu64; ../../../configure \
		--prefix="`pwd`/sandbox" \
                --enable-encap \
	        --enable-download \
                --disable-common-staging-area \
                --with-mpir-config-options="ABI=64"

#		CFLAGS="-fno-strict-aliasing" CXXFLAGS="-fno-strict-aliasing" \

#		--enable-python
# LDFLAGS=-shared-libgcc

# can we compile sage in?
debug32 : always
	mkdir -p debug32
	cd debug32; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		LDFLAGS="-L/Users/mike/local/software/sage/sage-3.4/local/lib" \
		CPPFLAGS="-I/Users/mike/local/software/sage/sage-3.4/local/include" \
		--enable-python --with-libpython=/Users/mike/local/software/sage/sage-3.4/local/lib/python2.5/config/libpython2.5.a \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby"

# icc versions
icc-darwin :
	mkdir -p icc-darwin
	cd icc-darwin; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-dmg \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  pari"

# These are for use on linux/ubuntu/redhat

opt : always
	mkdir -p opt
	cd opt;	../../../configure \
		--prefix="`pwd`/installed" \
		--enable-tarfile \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  lapack frobby pari"

opt64-ubuntu : always
	mkdir -p opt
	cd opt;	../../../configure \
		--prefix="`pwd`/installed" \
                --with-mpir-config-options="ABI=64" \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  lapack frobby pari"


  # is this comment for icc?
  # ntl hangs on ./MakeDesc, so workaround:
  # cd libraries/ntl/build/ntl-5.5.2/src/
  # make CFLAGS=-g  ?? Is this needed anymore??

debug : always
	mkdir -p debug
	cd debug; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  lapack frobby pari"

profile : always
	mkdir -p profile
	cd profile; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-profile \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  lapack frobby pari"

profile64-ubuntu : always
	mkdir -p profile
	cd profile; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-profile \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  lapack frobby pari"

upload : always
	scp darwin64/*.dmg u00.math.uiuc.edu:/home/html/www/Macaulay2/Uploads

