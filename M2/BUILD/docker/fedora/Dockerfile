# Time usage: ??
# Net usage:  54+124+
# Disk usage: ??

FROM registry.fedoraproject.org/fedora-minimal:32

# Programs we require to build
#  kernel-devel rpm-build rpm-sign rpmdevtools
RUN microdnf install autoconf bison ccache cmake curl diffutils file gcc-c++ git \
      gnupg libtool make ninja-build patch yasm

# Libraries we require
RUN microdnf install openblas-devel libxml2-devel readline-devel gdbm-devel boost-devel libatomic_ops-devel
# libomp-dev libtbb-dev

# Libraries we can build (factory not available on ubuntu)
RUN microdnf install eigen3-devel glpk-devel gmp-devel mpfr-devel ntl-devel libfrobby-devel gc-devel

# Programs we can build
# TODO: cohomcalg available soon. Polymake requires firefox???
#RUN microdnf install libcdd-devel 4ti2 gfan normaliz coinor-csdp nauty lrslib

# Optional packages
#RUN microdnf install mlocate bash-completion emacs

# Add non-root user for building and running Macaulay2
RUN useradd -G wheel -g root -u 1000 -m macaulay
USER 1000:1000

ENV PATH /home/macaulay/M2/M2/BUILD/build-docker:${PATH}

# Build the Docker image and Macaulay2-in-a-container
# make build

# Start a graphical instance:
# make run-graphical

# Start a non-graphical instance:
# make run-terminal

WORKDIR /home/macaulay/

ENTRYPOINT emacs

#RUN microdnf install libgomp openblas-serial tbb-devel gperftools-libs
#COPY Macaulay2-1.15.0.2-1.x86_64.rpm /Macaulay2-1.15.0.2-1.x86_64.rpm
#RUN rpm -i /Macaulay2-1.15.0.2-1.x86_64.rpm
