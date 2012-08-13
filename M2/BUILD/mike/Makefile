# checking out the source code:
#
#      svn co svn://Macaulay2.math.uiuc.edu/Macaulay2/trunk/M2 M2

.PHONY: always

always:

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
                --build=x86_64-unknown-linux-gnu \
                --with-mpir-config-options="ABI=64"

opt64 : always
	mkdir -p opt64
	cd opt64; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc-4.2 -m64" CXX="g++-4.2 -m64" \
		--build=x86_64-apple-darwin \
		--disable-strip \
		--enable-download \
		--enable-dmg \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac frobby pari"

opt64-givaro : always
	mkdir -p opt64-givaro
	cd opt64-givaro; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc-4.2 -m64" CXX="g++-4.2 -m64" \
		--build=x86_64-apple-darwin \
		--enable-fflas-ffpack \
		--enable-givaro \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac frobby pari"

opt64-linbox : always
	mkdir -p opt64-linbox
	cd opt64-linbox; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc-4.2 -m64" CXX="g++-4.2 -m64" \
		--build=x86_64-apple-darwin \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--enable-memtailor \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac frobby pari"

debug64-linbox : always
	mkdir -p debug64-linbox
	cd debug64-linbox; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc-4.2 -m64" CXX="g++-4.2 -m64" \
		--build=x86_64-apple-darwin \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-linbox \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac frobby pari"

debug64-givaro : always
	mkdir -p debug64-givaro
	cd debug64-givaro; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc-4.2 -m64" CXX="g++-4.2 -m64" \
		CPPFLAGS="-DENGINE_DEBUG" \
		--build=x86_64-apple-darwin \
		--enable-fflas-ffpack \
		--enable-givaro \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac frobby pari"

debug64 : always
	mkdir -p debug64
	cd debug64; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc-4.2 -m64" CXX="g++-4.2 -m64" \
		--build=x86_64-apple-darwin \
		CPPFLAGS="-DENGINE_DEBUG" \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac frobby pari"

debug64nofrobby : always
	mkdir -p debug64
	cd debug64; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--with-mpir-config-options="ABI=64" \
		--build=x86_64-apple-darwin \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--disable-frobby \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac pari"

profile64 : always
	mkdir -p profile64
	cd profile64; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--build=x86_64-apple-darwin \
		--enable-profile \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac frobby pari"

tiger32 : always
	mkdir -p tiger32
	cd tiger32; LDFLAGS=-static-libgcc ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-dmg \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac frobby pari"

darwin32 : always
	mkdir -p darwin32
	cd darwin32;  ../../../configure \
		CC="gcc -m32" CXX="g++ -m32" \
		--build=i586-apple-darwin \
		--prefix="`pwd`/installed" \
		--enable-dmg \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac frobby pari"
#		--build=i386-apple-darwin9 \
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
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac frobby"

profile32 : always
	mkdir -p profile32
	cd profile32; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-profile \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac frobby pari"

# icc versions
icc-darwin :
	mkdir -p icc-darwin
	cd icc-darwin; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="icc" CXX="icpc" \
		--enable-scscp \
		--enable-dmg \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac pari"

# These are for use on linux/ubuntu/redhat

opt : always
	mkdir -p opt
	cd opt;	../../../configure \
		--prefix="`pwd`/installed" \
		--enable-tarfile \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac lapack frobby pari"

opt64-ubuntu : always
	mkdir -p opt
	cd opt;	../../../configure \
		--prefix="`pwd`/installed" \
		--build=x86_64-unknown-linux-gnu \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac lapack frobby pari"

ubuntu64-givaro : always
	mkdir -p opt64-givaro
	cd opt64-givaro; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-fflas-ffpack \
		--enable-givaro \
		--disable-strip \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac frobby pari"

icc-ubuntu64 :
	mkdir -p icc-opt
	cd icc-opt; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="icc" CXX="icpc" \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac pari"
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
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac lapack frobby pari"


debug : always
	mkdir -p debug
	cd debug; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac lapack frobby pari"

profile : always
	mkdir -p profile
	cd profile; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-profile \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac lapack frobby pari"

profile64-ubuntu : always
	mkdir -p profile
	cd profile; ../../../configure \
		--prefix="`pwd`/installed" \
		--build=x86_64-unknown-linux-gnu \
		--enable-profile \
		--enable-download \
		--enable-build-libraries="gc gdbm mpir mpfr readline ntl factory libfac lapack frobby pari"

upload : always
	scp darwin64/*.dmg u00.math.uiuc.edu:/home/html/www/Macaulay2/Uploads
	scp opt32/*.dmg u00.math.uiuc.edu:/home/html/www/Macaulay2/Uploads