all:
	$(MAKE) -C build.tmp all install
configure:
	cd ../..; make
	mkdir -p build.tmp
	cd build.tmp ; ../../../configure --prefix=`pwd`/install --enable-download
