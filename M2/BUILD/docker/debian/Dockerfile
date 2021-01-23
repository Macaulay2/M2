# Time usage: <
# Net usage:  ~
# Disk usage: <

FROM debian:buster

# Programs we require to build
RUN apt-get update && \
    apt-get install -y apt-transport-https bison build-essential curl git gnupg wget && \
    wget -c https://github.com/Kitware/CMake/releases/download/v3.17.2/cmake-3.17.2-Linux-x86_64.tar.gz -O - | \
	tar xz -C /usr --strip 1 && \
    apt-get update && apt-get install -y autoconf ccache libtool lintian ninja-build python yasm && apt-get clean

# Libraries we require
RUN apt-get install -y libopenblas-dev libxml2-dev libreadline-dev libgdbm-dev libboost-regex-dev libboost-stacktrace-dev && \
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
RUN apt-get install -y mlocate bash-completion

# Add non-root user for building and running Macaulay2
RUN useradd -G sudo -g root -u 1000 -m macaulay && echo "macaulay ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
USER 1000:0

ENV PKG_CONFIG_PATH /usr/lib/x86_64-linux-gnu/pkgconfig
ENV LDFLAGS         -L/usr/lib/x86_64-linux-gnu
ENV PATH            /home/macaulay/M2/M2/BUILD/build-docker:${PATH}

WORKDIR /home/macaulay
ENTRYPOINT M2

# RUN apt-get install --no-install-recommends -y \
#     libgdbm6 libgomp1 libopenblas-base libreadline7 libtbb2 libtcmalloc-minimal4 libxml2
#COPY Macaulay2-*.deb /
#RUN dpkg -i /Macaulay2-*.deb
