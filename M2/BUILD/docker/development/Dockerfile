# Time usage: <15min
# Net usage:  ~255MB
# Disk usage: <1GB docker image + 650MB Macaulay2 build

FROM ubuntu:18.04
# TODO: consider gcc:9.3 which is based on Debian 10, but is much heavier

# Programs we require to build
RUN apt-get update && \
    apt-get install -y apt-transport-https bison build-essential curl git gnupg && \
    curl https://faculty.math.illinois.edu/Macaulay2/PublicKeys/Macaulay2-key -s --output - | apt-key add - && \
    curl https://apt.kitware.com/keys/kitware-archive-latest.asc -s --output - | apt-key add - && \
    echo 'deb http://www.math.uiuc.edu/Macaulay2/Repositories/Ubuntu bionic main' > \
      /etc/apt/sources.list.d/macaulay2.list && \
    echo 'deb https://apt.kitware.com/ubuntu/ bionic main' > \
      /etc/apt/sources.list.d/cmake.list && \
    apt-get update && apt-get install -y autoconf ccache cmake libtool ninja-build yasm && apt-get clean

# Libraries we require
RUN apt-get install -y libopenblas-dev libxml2-dev libreadline-dev libgdbm-dev libboost-stacktrace-dev && \
    apt-get clean # libomp-dev libtbb-dev

# This is a dependency that we should get rid of
RUN apt-get install -y libatomic-ops-dev && apt-get clean

# Libraries we can build (factory not available on ubuntu)
RUN apt-get install -y libeigen3-dev  libglpk-dev libgmp3-dev libmpfr-dev \
      libntl-dev libfrobby-dev libgc-dev && apt-get clean

#RUN apt-get install -y fflas-ffpack libgivaro-dev libgc-dev libflint-dev && apt-get clean

# Programs we can build
# TODO: cohomcalg available soon. Polymake requires firefox???
# RUN apt-get install -y libcdd-dev 4ti2 gfan normaliz coinor-csdp nauty lrslib && apt-get clean

# Optional packages
RUN apt-get install -y mlocate bash-completion emacs

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
