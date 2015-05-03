all:
	$(MAKE) -C build.tmp all install
configure:
	cd ../..; make -f Makefile
	mkdir -p build.tmp 
	cd build.tmp; ../../../configure --enable-download --prefix=$(HOME)/Macaulay2-built CPPFLAGS="-I/usr/local/include -I/usr/local/opt/readline/include" LDFLAGS="-L/usr/local/lib -L/usr/local/opt/readline/lib" CC="gcc-4.9 -m64" CXX="g++-4.9 -m64" --build=x86_64-apple-darwin

veryclean :
	rm -rf build.tmp


