# Time usage: ??
# Net usage:  54+124+
# Disk usage: ??

ARG RELEASE=latest

FROM quay.io/fedora/fedora-minimal:$RELEASE

# Programs we require to build
#  kernel-devel rpm-build rpm-sign rpmdevtools
RUN microdnf install -y autoconf bison ccache cmake curl diffutils file gcc-c++ git \
	gnupg libtool make ninja-build patch yasm rpm-build rpmlint which

# Libraries we require
RUN microdnf install -y openblas-devel libxml2-devel readline-devel gdbm-devel \
	boost-devel libomp-devel tbb-devel

# Libraries we can build (factory not available on ubuntu)
RUN microdnf install -y eigen3-devel glpk-devel gmp-devel mpfr-devel ntl-devel \
	libnauty-devel libnormaliz-devel libfrobby-devel gc-devel

# Programs we can build
# TODO: cohomcalg available soon. Polymake requires firefox???
RUN microdnf install -y cddlib-devel 4ti2 gfan normaliz csdp-tools nauty lrslib

# Optional packages
RUN microdnf install -y mlocate bash-completion

# Add non-root user for building and running Macaulay2
RUN useradd -G wheel -g root -u 1000 -m macaulay && echo "macaulay ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
USER 1000:0

ENV PATH /home/macaulay/M2/M2/BUILD/build-docker:${PATH}

WORKDIR /home/macaulay
ENTRYPOINT M2

#RUN microdnf install libgomp openblas-serial tbb-devel gperftools-libs
#COPY Macaulay2-*.rpm /
#RUN rpm -i /Macaulay2-*.rpm
