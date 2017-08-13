# 
# Written for Fedora 26
#

.PHONY: always

rpm-docker-build:
	[ ! -z `docker images -q m2-rpm` ] || docker build -t m2-rpm .
	docker run -it \
	  -v `pwd`/../../..:/M2 \
	  m2-rpm \
	  make rpm-build -j4 -C /M2/M2/BUILD/mahrud

rpm-build:
	mkdir -p build;
	make -j4 -C ../..;
	cd build && ../../../configure \
	  --enable-rpm \
	  --prefix=/usr \
	  --enable-download \
	  --enable-build-libraries="factory";
	make -j4 -C build

#	  --enable-build-libraries="factory pari flint ntl fflas_ffpack memtailor mathic mathicgb";
#TODO: can we skip building factory? use FTABLESDIR=/usr/share/factory/ 
