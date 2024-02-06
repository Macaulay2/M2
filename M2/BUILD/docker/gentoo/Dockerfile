# Time usage: 
# Net usage:  
# Disk usage: 

# name the portage image
FROM gentoo/portage:latest as portage

# image is based on stage3
FROM gentoo/stage3:latest

# copy the entire portage volume in
COPY --from=portage /var/db/repos/gentoo /var/db/repos/gentoo

# Build tools
RUN emerge --sync
RUN emerge -n cmake ninja

#RUN emerge --ask -n 4ti2 blas-reference lapack-reference boehm-gc boost cddlib coinor-csdp flint fplll frobby gdbm gfan glpk gmp gtest libatomic_ops lrslib mpc mpfr mpir nauty normaliz ntl topcom yasm sys-process/time

#  	autoconf automake bison make patch ccache \
#	gettext python cmake curl gcc git gnupg \
#	libtool pkg-config ninja yasm

# Required libraries
#RUN pacman --sync --noconfirm --needed openblas openmp intel-tbb libxml2 readline gdbm boost

# Libraries we can build
#RUN pacman --sync --noconfirm --needed eigen fflas-ffpack givaro glpk gmp gc gtest mpfr ntl libatomic_ops

# Optional packages
#RUN pacman --sync --noconfirm mlocate bash-completion

# Add non-root user for building and running Macaulay2
RUN useradd -g root -u 1000 -m macaulay
USER 1000:0

ENV PATH            /home/macaulay/M2/M2/BUILD/build-docker:${PATH}
#ENV CC              /usr/lib/ccache/bin/gcc
#ENV CXX             /usr/lib/ccache/bin/g++

WORKDIR /home/macaulay
ENTRYPOINT M2
