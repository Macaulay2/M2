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

# Libraries we can build
RUN apt-get install -y --no-install-recommends software-properties-common && \
    add-apt-repository -y ppa:macaulay2/macaulay2 && apt-get update && \
    apt-get install -y -q --no-install-recommends libeigen3-dev libgc-dev \
	libglpk-dev libgmp3-dev libmpfr-dev libmps-dev libgtest-dev \
	libntl-dev libflint-dev libsingular-dev singular-data libfrobby-dev \
	libmemtailor-dev libmathic-dev libmathicgb-dev && apt-get clean

# Programs we can build
RUN apt-get install -y --no-install-recommends libcdd-dev lrslib \
	4ti2 cohomcalg coinor-csdp gfan nauty normaliz polymake topcom && \
    apt-get clean

# Setting up Macaulay2 repository
RUN apt-get update && \
    apt-get install -y --no-install-recommends software-properties-common apt-transport-https && \
    add-apt-repository ppa:macaulay2/macaulay2 && add-apt-repository ppa:macaulay2/macaulay2 && \
    apt-get update && apt-get clean

RUN apt-get install -y libgivaro-dev fflas-ffpack
RUN apt-get install -y libmpfi-dev

# Optional packages
RUN apt-get install -y mlocate bash-completion valgrind

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
