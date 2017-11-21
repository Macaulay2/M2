FROM base/archlinux:latest as m2-deps-arch
MAINTAINER mahrud <mahrud@berkeley.edu>

# Fresh Fedora container:
RUN	pacman -Syu --needed archlinux-keyring && \
	pacman-key --populate archlinux && \
	pacman --sync --refresh --sysupgrade

# Macaulay2 prerequisites:
RUN	echo Y | pacman --sync --needed autoconf automake bison boost cddlib emacs flex \
	flint gc gcc gcc-fortran gdbm gfan git givaro glpk gmp gtest lapack make mpfr nauty \
	ntl pkg-config readline texinfo time unzip yasm gtest patch

FROM m2-deps-arch as m2-compile-arch
ADD     . /M2
WORKDIR	/M2/M2/BUILD/build
RUN	make -j -C /M2/M2/  && \
	/M2/M2/configure --enable-tarfile --prefix=/usr --enable-download  &&  make -j

FROM m2-compile-arch as m2-arch
# Install M2
RUN	make install -j
# Automatically run M2 when the container starts
ENTRYPOINT ["M2", "--no-readline"]
