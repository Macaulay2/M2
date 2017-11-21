FROM fedora:latest as m2-deps-fedora
MAINTAINER mahrud <mahrud@berkeley.edu>

# Fresh Fedora container:
RUN	dnf -y update && dnf clean packages

# Macaulay2 prerequisites:
RUN	dnf -y install git autoconf libtool pkgconfig rpm-build gcc-c++ yasm which \
	  bison flex ncurses-devel lapack-devel libxml2-devel libatomic_ops-devel gc-devel gdbm-devel \
	  readline-devel glpk-devel boost-devel gmp-devel libmpc-devel && dnf clean all

FROM m2-deps-fedora as m2-compile-fedora
ADD	. /M2
WORKDIR	/M2/M2/BUILD/build
RUN	make -j -C /M2/M2/  && \
	/M2/M2/configure --enable-rpm --prefix=/usr --enable-download  &&  make -j

FROM m2-compile-fedora as m2-fedora
# Install M2
RUN	dnf -y install Macaulay2-*.rpm && dnf clean all
# Automatically run M2 when the container starts
ENTRYPOINT ["M2", "--no-readline"]
