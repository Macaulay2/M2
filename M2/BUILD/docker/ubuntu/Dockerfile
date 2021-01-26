# Time usage: <15min
# Net usage:  ~255MB
# Disk usage: <1GB docker image + 650MB Macaulay2 build

FROM ubuntu:focal

# Programs we require to build
RUN apt-get update && \
    apt-get install -y apt-transport-https build-essential curl git gnupg && \
    curl https://apt.kitware.com/keys/kitware-archive-latest.asc -s --output - | apt-key add - && \
    echo 'deb https://apt.kitware.com/ubuntu/ focal main' > \
      /etc/apt/sources.list.d/cmake.list && \
    apt-get update && apt-get install -y autoconf bison ccache cmake flex libtool ninja-build yasm && apt-get clean

# FIXME: this is needed to make tzdata install without asking a question
RUN ln -fs /usr/share/zoneinfo/America/New_York /etc/localtime

# Libraries we require
RUN apt-get install -y libopenblas-dev libxml2-dev libreadline-dev libgdbm-dev \
	libboost-regex-dev libboost-stacktrace-dev libomp-dev libtbb-dev && \
	apt-get clean

# This is a dependency that we should get rid of
RUN apt-get install -y libatomic-ops-dev && apt-get clean

# Libraries we can build (factory not available on ubuntu)
RUN apt-get install -y libeigen3-dev libglpk-dev libgmp3-dev libmpfr-dev \
      libntl-dev libfrobby-dev libgc-dev && apt-get clean

# FIXME: The base container doesn't have Python, which is needed by MPSolve
RUN apt-get install -y python && apt-get clean

# Programs we can build
# TODO: cohomcalg available soon.
# RUN apt-get install -y --no-install-recommends libcdd-dev 4ti2 gfan polymake normaliz coinor-csdp nauty lrslib && apt-get clean

# Optional packages
RUN apt-get install -y mlocate bash-completion

# Add non-root user for building and running Macaulay2
RUN useradd -G sudo -g root -u 1000 -m macaulay && echo "macaulay ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
USER 1000:0

ENV PKG_CONFIG_PATH /usr/lib/x86_64-linux-gnu/pkgconfig
ENV LDFLAGS         -L/usr/lib/x86_64-linux-gnu
ENV PATH            /home/macaulay/M2/M2/BUILD/build-docker:${PATH}

WORKDIR /home/macaulay
ENTRYPOINT M2

#RUN apt-get update && apt-get install -y libxml2 libgomp1 libreadline8 libopenblas-base libtbb2 libtcmalloc-minimal4 libgdbm6
#COPY Macaulay2-*.deb /
#RUN dpkg -i /Macaulay2-*.deb
