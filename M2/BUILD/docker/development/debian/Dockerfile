# Time usage: <
# Net usage:  ~
# Disk usage: <

FROM debian:buster

# Programs we require to build
RUN apt-get update && \
    apt-get install -y apt-transport-https bison build-essential curl git gnupg wget && \
    wget -c https://github.com/Kitware/CMake/releases/download/v3.17.2/cmake-3.17.2-Linux-x86_64.tar.gz -O - | \
	tar xz -C /usr --strip 1 && \
    apt-get update && apt-get install -y autoconf ccache libtool ninja-build yasm && apt-get clean

# Libraries we require
RUN apt-get install -y libopenblas-dev libxml2-dev libreadline-dev libgdbm-dev libboost-stacktrace-dev && \
    apt-get clean # libomp-dev libtbb-dev

# This is a dependency that we should get rid of
RUN apt-get install -y libatomic-ops-dev && apt-get clean

# Libraries we can build (factory not available on ubuntu)
RUN apt-get install -y libeigen3-dev  libglpk-dev libgmp3-dev libmpfr-dev \
      libntl-dev libfrobby-dev libgc-dev && apt-get clean

# Programs we can build
# TODO: cohomcalg available soon. Polymake requires firefox???
# RUN apt-get install -y libcdd-dev 4ti2 gfan normaliz coinor-csdp nauty lrslib && apt-get clean

# Optional packages
RUN apt-get install -y mlocate bash-completion emacs

RUN apt-get install -y python
RUN apt-get install -y lintian

# Add non-root user for building and running Macaulay2
RUN useradd -G sudo -g root -u 1000 -m macaulay
USER 1000:1000

ENV PKG_CONFIG_PATH /usr/lib/x86_64-linux-gnu/pkgconfig
ENV LDFLAGS         -L/usr/lib/x86_64-linux-gnu
ENV PATH            /home/macaulay/M2/M2/BUILD/build-docker:${PATH}

# Build the Docker image and Macaulay2-in-a-container
# make build

# Start a graphical instance:
# make run-graphical

# Start a non-graphical instance:
# make run-terminal

WORKDIR /home/macaulay/

ENTRYPOINT emacs
