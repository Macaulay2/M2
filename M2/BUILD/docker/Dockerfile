FROM fedora:latest
MAINTAINER mahrud <mahrud@berkeley.edu>

# Fresh Fedora container:
RUN	dnf -y update && dnf clean packages

# Macaulay2 prerequisites:
RUN	dnf -y install git autoconf libtool pkgconfig rpm-build gcc-c++ yasm which \
	  bison flex ncurses-devel lapack-devel libxml2-devel libatomic_ops-devel gc-devel gdbm-devel \
	  readline-devel glpk-devel boost-devel gmp-devel libmpc-devel && dnf clean all

# Clone and make M2
RUN	git clone https://github.com/Macaulay2/M2.git /M2
WORKDIR	/M2/M2/BUILD/docker/build
RUN	make -j -C /M2/M2/  &&  /M2/M2/configure --enable-rpm --prefix=/usr --enable-download  &&  make -j

# Install the RPM
RUN	dnf -y install Macaulay2-*.rpm && dnf clean all
