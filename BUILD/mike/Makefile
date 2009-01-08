# checking out the source code:
#
#      svn co svn://Macaulay2.math.uiuc.edu/Macaulay2/trunk/M2 M2


opt64 :
	mkdir -p opt64
	cd opt64
	../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--build=x86_64-apple-darwin \
		--enable-dmg \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac" \
		--enable-frobby

debug64 :
	mkdir -p debug64
	cd debug64; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--build=x86_64-apple-darwin \
		--enable-debug \
		--disable-optimize \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac" \
		--enable-frobby

profile64 :
	mkdir -p profile64
	cd profile64; ../../../configure \
		--prefix="`pwd`/installed" \
		CC="gcc -m64" CXX="g++ -m64" \
		--build=x86_64-apple-darwin \
		--enable-profile \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac" \
		--enable-frobby

# These are for use on linux/ubuntu

opt :
	mkdir -p opt
	cd opt
	../../../configure \
		--prefix="`pwd`/installed" \
		--enable-tarfile \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac lapack" \
		--enable-frobby

debug :
	mkdir -p debug
	cd debug; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-debug \
		--disable-optimize \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac lapack" \
		--enable-frobby

profile :
	mkdir -p profile
	cd profile; ../../../configure \
		--prefix="`pwd`/installed" \
		--enable-profile \
		--enable-build-libraries="gc gdbm gmp mpfr readline ntl factory libfac lapack" \
		--enable-frobby
