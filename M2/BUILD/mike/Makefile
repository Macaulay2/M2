.PHONY: always

always:

################
## mac os x ####
################
darwin64 : always
	mkdir -p builds.tmp/darwin64
	cd builds.tmp/darwin64; ../../../../configure \
		--prefix="`pwd`/installed" \
		--disable-ntl-wizard \
		--disable-strip \
		--enable-dmg \
		--enable-download

# make sure that CC and CXX are set in environment variables
#		CC="gcc -m64" CXX="g++ -m64" \

#		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory frobby pari"

debug64 : always
	mkdir -p builds.tmp/debug64
	cd builds.tmp/debug64; ../../../../configure \
		--prefix="`pwd`/installed" \
		CPPFLAGS="-DENGINE_DEBUG_" \
		CXXFLAGS=" -U__GNUC_STDC_INLINE__" \
		--disable-ntl-wizard \
		--enable-debug \
		--disable-optimize \
		--enable-download

#		CC="gcc -m64" CXX="g++ -m64" \
#		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

# the following has all optional libs desired for linalg merge
opt64-linalg : always
	mkdir -p builds.tmp/opt64-linalg
	cd builds.tmp/opt64-linalg; ../../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--disable-ntl-wizard \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-fplll \
		--disable-strip \
		--enable-download \
		--enable-dmg

opt64-mathic : always
	mkdir -p builds.tmp/opt64-mathic
	cd builds.tmp/opt64-mathic; ../../../../configure \
		--prefix="`pwd`/installed" \
		--disable-ntl-wizard \
		--disable-strip \
		--enable-download \
		--enable-dmg

#		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory frobby pari"

# the following has all optional libs of worth
opt64-all : always
	mkdir -p builds.tmp/opt64-all
	cd builds.tmp/opt64-all; ../../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--disable-ntl-wizard \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-fplll \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory frobby pari"

#		--enable-linbox \

# for adding tbb in for mathicgb:
#		LDFLAGS="-L/usr/local/lib -ltbb" \

debug64-all : always
	mkdir -p builds.tmp/debug64-all
	cd builds.tmp/debug64-all; ../../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		CPPFLAGS="-DENGINE_DEBUG_" \
		CXXFLAGS=" -U__GNUC_STDC_INLINE__" \
		--disable-ntl-wizard \
		--disable-strip \
		--enable-debug \
		--disable-optimize \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

#		--enable-linbox \

debug64-linalg : always
	mkdir -p builds.tmp/debug64-linalg
	cd builds.tmp/debug64-linalg; ../../../../configure \
		--prefix="`pwd`/installed" \
		CPPFLAGS="-DENGINE_DEBUG_" \
		CXXFLAGS=" -U__GNUC_STDC_INLINE__" \
		--disable-strip \
		--enable-debug \
		--disable-optimize \
		--disable-ntl-wizard \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-fplll \
		--enable-download

#		CC="gcc -m64" CXX="g++ -m64" \


####################
## clang versions ##
####################
opt64-clang : always
	mkdir -p builds.tmp/opt64-clang
	cd builds.tmp/opt64-clang; ../../../../configure \
		--prefix="`pwd`/installed" \
		CC="clang -m64" CXX="clang++ -m64" \
		--disable-gfan \
		--disable-frobby \
		--disable-ntl-wizard \
		--disable-strip \
		--enable-dmg \
		--enable-download \


#		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

debug64-clang : always
	mkdir -p builds.tmp/debug64-clang
	cd builds.tmp/debug64-clang; ../../../../configure \
		--prefix="`pwd`/installed" \
		CC="clang -m64" CXX="clang++ -m64" \
		CPPFLAGS="-DENGINE_DEBUG" \
		--disable-gfan \
		--disable-frobby \
		--disable-ntl-wizard \
		--enable-debug \
		--disable-optimize \
		--enable-download \

#		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

# the following has mathic
opt64-clang-mathic : always
	mkdir -p builds.tmp/opt64-clang-mathic
	cd builds.tmp/opt64-clang-mathic; ../../../../configure \
		--prefix="`pwd`/installed" \
		CC="clang -m64" CXX="clang++ -m64" \
		--disable-gfan \
		--disable-frobby \
		--disable-ntl-wizard \
		--disable-strip \
		--enable-download \

# the following has all optional libs of worth
opt64-clang-all : always
	mkdir -p builds.tmp/opt64-clang-all
	cd builds.tmp/opt64-clang-all; ../../../../configure \
		--prefix="`pwd`/installed" \
		CC="clang -m64" CXX="clang++ -m64" \
		--disable-gfan \
		--disable-frobby \
		--disable-ntl-wizard \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--disable-strip \
		--enable-download \

#		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

debug64-clang-all : always
	mkdir -p builds.tmp/debug64-all
	cd builds.tmp/debug64-all; ../../../../configure \
		--prefix="`pwd`/installed" \
		CC="clang -m64" CXX="clang++ -m64" \
		CPPFLAGS="-DENGINE_DEBUG" \
		--disable-strip \
		--enable-debug \
		--disable-optimize \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

##################
## linux #########
##################
linux-opt64 : always
	mkdir -p builds.tmp/opt64
	cd builds.tmp/opt64; ../../../../configure \
		--prefix="`pwd`/installed" \
		--disable-ntl-wizard \
		--disable-strip \
		--enable-download

#		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"
# 		CC="gcc -m64" CXX="g++ -m64" \

linux-debug64 : always
	mkdir -p builds.tmp/debug64
	cd builds.tmp/debug64; ../../../../configure \
		--prefix="`pwd`/installed" \
		CPPFLAGS="-DENGINE_DEBUG_" \
		CXXFLAGS=" -U__GNUC_STDC_INLINE__" \
		--enable-debug \
		--disable-ntl-wizard \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

linux-profile64 : always
	mkdir -p builds.tmp/profile64
	cd builds.tmp/profile64; ../../../../configure \
		--prefix="`pwd`/installed" \
		--disable-ntl-wizard \
		--disable-strip \
		--enable-profile \
		--enable-download

linux-opt64-mathic : always
	mkdir -p builds.tmp/opt64-mathic
	cd builds.tmp/opt64-mathic; ../../../../configure \
	    --prefix="`pwd`/installed" \
	    --disable-strip \
	    --enable-download

linux-debug64-mathic : always
	mkdir -p builds.tmp/debug64-mathic
	cd builds.tmp/debug64-mathic; ../../../../configure \
		--prefix="`pwd`/installed" \
		CPPFLAGS="-DENGINE_DEBUG_" \
		CXXFLAGS=" -U__GNUC_STDC_INLINE__" \
		--enable-debug \
		--disable-ntl-wizard \
		--disable-optimize \
		--enable-download \

#               CPPFLAGS="-I/usr/local/include" LDFLAGS="-L/usr/local/lib -ltbb" \

linux-opt64-linalg : always
	mkdir -p builds.tmp/opt64-linalg
	cd builds.tmp/opt64-linalg; ../../../../configure \
		--prefix="`pwd`/installed" \
		--disable-ntl-wizard \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--enable-fplll \
		--disable-strip \
		--enable-download

linux-opt64-all : always
	mkdir -p builds.tmp/opt64-all
	cd builds.tmp/opt64-all; ../../../../configure \
		--prefix="`pwd`/installed" \
		--disable-ntl-wizard \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

linux-debug64-all : always
	mkdir -p builds.tmp/debug64-all
	cd builds.tmp/debug64-all; ../../../../configure \
		--prefix="`pwd`/installed" \
		CPPFLAGS="-DENGINE_DEBUG_" \
		CXXFLAGS=" -U__GNUC_STDC_INLINE__" \
		--disable-ntl-wizard \
		--enable-debug \
		--disable-optimize \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

linux-debug64-linalg : always
	mkdir -p builds.tmp/debug64-linalg
	cd builds.tmp/debug64-linalg; ../../../../configure \
		--prefix="`pwd`/installed" \
		CPPFLAGS="-DENGINE_DEBUG_" \
		CXXFLAGS=" -U__GNUC_STDC_INLINE__" \
		--disable-ntl-wizard \
		--enable-debug \
		--disable-optimize \
		--enable-fflas-ffpack \
		--enable-givaro \
		--disable-strip \
		--enable-download

#		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

######## below this line is older ######################
sandboxed : always
	mkdir -p sandbox64
	cd sandbox64; ../../../configure \
		--prefix="`pwd`/sandbox" \
		--enable-download



dan-ubuntu64: always
	mkdir -p dan-ubuntu64
	cd dan-ubuntu64; ../../../configure \
		--prefix="`pwd`/sandbox" \
                --enable-encap \
	        --enable-download \
                --disable-common-staging-area \
                --with-mpir-config-options="ABI=64"

opt64 : always
	mkdir -p opt64
	cd opt64; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc-4.2 -m64" CXX="g++-4.2 -m64" \
		--disable-strip \
		--enable-download \
		--enable-dmg \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

opt64-clang-old : always
	mkdir -p opt64-clang
	cd opt64-clang; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="clang -m64" CXX="clang++ -m64" \
		--enable-dmg \
		--disable-frobby \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  pari"

#		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

#		CFLAGS="-fno-strict-aliasing" CXXFLAGS="-fno-strict-aliasing" \

opt64-givaro : always
	mkdir -p opt64-givaro
	cd opt64-givaro; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc-4.2 -m64" CXX="g++-4.2 -m64" \
		--enable-fflas-ffpack \
		--enable-givaro \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

opt64-mathic-older : always
	mkdir -p opt64-mathic
	cd opt64-mathic; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		CPPFLAGS="-I/usr/local/include" LDFLAGS="-L/usr/local/lib -ltbb" \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

debug64-mathic : always
	mkdir -p debug64-mathic
	cd debug64-mathic; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		CPPFLAGS="-I/usr/local/include" LDFLAGS="-L/usr/local/lib -ltbb" \
		CPPFLAGS="-DENGINE_DEBUG" \
		--enable-debug \
		--disable-optimize \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

opt64-clang-linbox : always
	mkdir -p opt64-clang-linbox
	cd opt64-clang-linbox; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="clang -m64" CXX="clang++ -m64" \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

debug64-linbox : always
	mkdir -p debug64-linbox
	cd debug64-linbox; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--enable-debug \
		--disable-optimize \
		CPPFLAGS="-DENGINE_DEBUG" \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

opt64-fplll : always
	mkdir -p opt64-fplll
	cd opt64-fplll; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--enable-fplll \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

opt64-flint : always
	mkdir -p opt64-flint
	cd opt64-flint; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

#		CC="gcc-4.2 -m64" CXX="g++-4.2 -m64" 

debug64-linbox-only : always
	mkdir -p debug64-linbox
	cd debug64-linbox; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

debug64-givaro : always
	mkdir -p debug64-givaro
	cd debug64-givaro; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc-4.2 -m64" CXX="g++-4.2 -m64" \
		CPPFLAGS="-DENGINE_DEBUG" \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

debug64-old : always
	mkdir -p debug64
	cd debug64; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		CPPFLAGS="-DENGINE_DEBUG" \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

debug64nofrobby : always
	mkdir -p debug64
	cd debug64; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--with-mpir-config-options="ABI=64" \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--disable-frobby \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  pari"

profile64 : always
	mkdir -p profile64
	cd profile64; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--enable-profile \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

tiger32 : always
	mkdir -p tiger32
	cd tiger32; LDFLAGS=-static-libgcc ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-dmg \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

darwin32 : always
	mkdir -p darwin32
	cd darwin32;  ../../../configure \
		CC="gcc -m32" CXX="g++ -m32" \
		--prefix="`pwd`/installed" \
		--enable-dmg \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"
#		--enable-python
# LDFLAGS=-shared-libgcc



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

profile32 : always
	mkdir -p profile32
	cd profile32; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-profile \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

# icc versions
icc-darwin :
	mkdir -p icc-darwin
	cd icc-darwin; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="icc" CXX="icpc" \
		--enable-scscp \
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

ubuntu64-givaro : always
	mkdir -p opt64-givaro
	cd opt64-givaro; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-fflas-ffpack \
		--enable-givaro \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

ubuntu64-linbox : always
	mkdir -p opt64-linbox
	cd opt64-linbox; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

ubuntu64-linbox-debug : always
	mkdir -p debug64-linbox
	cd debug64-linbox; ../../../configure \
		--prefix="`pwd`/installed" \
		CPPFLAGS="-DENGINE_DEBUG" \
		--enable-debug \
		--disable-optimize \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

ubuntu64-linbox-profile : always
	mkdir -p profile64-linbox
	cd profile64-linbox; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-profile \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  frobby pari"

icc-ubuntu64 :
	mkdir -p icc-opt
	cd icc-opt; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="icc" CXX="icpc" \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  pari"
  # ntl hangs on ./MakeDesc, so workaround:
  # cd libraries/ntl/build/ntl-5.5.2/src/
  # make CFLAGS=-g
icc-debug : always
	mkdir -p icc-debug
	cd icc-debug; ../../../configure \
		CC="icc" CXX="icpc" \
		--prefix="`pwd`/installed" \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory  lapack frobby pari"


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
	scp opt32/*.dmg u00.math.uiuc.edu:/home/html/www/Macaulay2/Uploads
