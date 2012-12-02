CC = gcc-4.2 -m64
CXX = g++-4.2 -m64
all:
	$(MAKE) -C build.tmp all install
configure:
	cd ../..; make
	mkdir -p build.tmp
	cd build.tmp ; ../../../configure --prefix=`pwd`/install --enable-download CC="$(CC)" CXX="$(CXX)"
veryclean :
	rm -rf build.tmp


