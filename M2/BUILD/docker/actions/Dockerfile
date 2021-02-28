# Time usage: <15min
# Net usage:  ~255MB
# Disk usage: <1GB docker image + 650MB Macaulay2 build

FROM ubuntu:20.04

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && \
    apt-get install -y software-properties-common apt-transport-https build-essential curl git gnupg && \
    curl https://apt.kitware.com/keys/kitware-archive-latest.asc -s --output - | apt-key add - && \
    echo 'deb https://apt.kitware.com/ubuntu/ focal main' > \
      /etc/apt/sources.list.d/cmake.list && \
    add-apt-repository -y ppa:macaulay2/macaulay2 && \
    apt-get update && apt-get install -y autoconf bison ccache cmake flex libtool ninja-build yasm && apt-get clean

# Programs and libraries we require to build
RUN apt-get install -y -q --no-install-recommends libatomic-ops-dev libboost-stacktrace-dev \
	libncurses-dev libncurses5-dev libreadline-dev libeigen3-dev liblapack-dev libxml2-dev \
	libgc-dev libgdbm-dev libglpk-dev libgmp3-dev libgtest-dev libmpfr-dev libntl-dev gfan \
	libgivaro-dev libboost-regex-dev fflas-ffpack libflint-dev libmps-dev libfrobby-dev \
	libsingular-dev singular-data libmemtailor-dev libmathic-dev libmathicgb-dev libcdd-dev \
	cohomcalg topcom 4ti2 normaliz coinor-csdp nauty lrslib w3c-markup-validator libtbb-dev

# Testing with clang10 compiler
RUN apt-get install -y libomp5-10 clang-10 llvm-10 && apt-get clean

RUN apt-get install -y -q --no-install-recommends mlocate bash-completion && apt-get clean && updatedb

RUN add-apt-repository ppa:ubuntu-toolchain-r/test && \
    apt-get update

# Needed for building with autotools
# RUN apt-get install -y -q liblzma-dev

# Add non-root user for building and running Macaulay2
RUN useradd -G sudo -g root -u 1000 -m macaulay
USER 1000:0

ENV PKG_CONFIG_PATH /usr/lib/x86_64-linux-gnu/pkgconfig
ENV LDFLAGS         -L/usr/lib/x86_64-linux-gnu
ENV PATH            /home/macaulay/M2/M2/BUILD/build-actions:${PATH}

WORKDIR /home/macaulay
ENTRYPOINT M2
