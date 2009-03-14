# checking out the source code:
#
#      svn co svn://Macaulay2.math.uiuc.edu/Macaulay2/trunk/M2 M2

.PHONY: always

always:

opt64 : always
	mkdir -p opt64
	cd opt64; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--build=x86_64-apple-darwin \
		--disable-strip \
		--enable-download \
		--enable-dmg \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac frobby pari"

debug64 : always
	mkdir -p debug64
	cd debug64; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--build=x86_64-apple-darwin \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac frobby pari"

profile64 : always
	mkdir -p profile64
	cd profile64; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--build=x86_64-apple-darwin \
		--enable-profile \
		--enable-download \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac frobby pari"

tiger32 : always
	mkdir -p tiger32
	cd tiger32; LDFLAGS=-static-libgcc ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-dmg \
		--enable-download \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac frobby pari"

darwin32 : always
	mkdir -p darwin32
	cd darwin32; LDFLAGS=-shared-libgcc ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-dmg \
		--enable-download \
		--enable-python \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac frobby pari"

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
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac frobby"

profile32 : always
	mkdir -p profile32
	cd profile32; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-profile \
		--enable-download \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac frobby pari"

# These are for use on linux/ubuntu/redhat

opt : always
	mkdir -p opt
	cd opt;	../../../configure \
		--prefix="`pwd`/installed" \
		--enable-tarfile \
		--enable-download \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac lapack frobby pari"

debug : always
	mkdir -p debug
	cd debug; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-debug \
		--disable-optimize \
		--enable-download \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac lapack frobby pari"

profile : always
	mkdir -p profile
	cd profile; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-profile \
		--enable-download \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac lapack frobby pari"

upload : always
	scp darwin64/*.dmg u00.math.uiuc.edu:/home/html/www/Macaulay2/Uploads
	scp opt32/*.dmg u00.math.uiuc.edu:/home/html/www/Macaulay2/Uploads