FROM fedora:latest
MAINTAINER mahrud <mahrud@berkeley.edu>

# Fresh Fedora container:
RUN dnf -y update && dnf clean all

# Macaulay2 prerequisites:
RUN dnf -y install autoconf bison boost-devel cddlib-devel emacs factory-devel fakeroot flex gc-devel \
           gcc-c++ gdbm-devel git glpk-devel gmp-devel kernel-devel lapack-devel libatomic_ops-devel libfac-devel \
           libgfortran-static libtool libxml2-devel mpfr-devel mpir-devel ncurses-devel ntl-devel pari-devel patch \
           readline-devel rpm-build rpm-sign rpmdevtools subversion libmpc-devel gtest-devel givaro-devel frobby \
           flint-devel yasm libmpc-devel time which && dnf clean all

# Clone and make M2
RUN /usr/bin/bash
RUN git clone https://github.com/Macaulay2/M2.git M2 && make -j -C /M2/M2/ && \
	mkdir -p /M2/M2/BUILD/docker/build && cd /M2/M2/BUILD/docker/build && \
	/M2/M2/configure \
	  --enable-rpm \
	  --prefix=/usr \
	  --enable-download \
	  --enable-build-libraries="factory pari flint ntl fflas_ffpack memtailor mathic mathicgb" && \
	make -j -C /M2/M2/BUILD/docker/build

# Install the RPM's
RUN dnf -y install /M2/M2/BUILD/docker/build/Macaulay2-*.rpm && dnf clean all

#TODO: can we skip building factory? use FTABLESDIR=/usr/share/factory/ 
