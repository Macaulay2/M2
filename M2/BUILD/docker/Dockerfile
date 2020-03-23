# Time usage: <15min
# Net usage:  ~255MB
# Disk usage: <1GB docker image + 650MB Macaulay2 build

FROM ubuntu:18.04
# TODO: consider gcc:9.3 which is based on Debian 10, but is much heavier

# Programs we require to build
# TODO: consider emacs ca-certificates software-properties-common (for apt-add-repository)
# TODO: try rm -rf /var/lib/apt/lists/* instead of apt-get clean? about 27M better
RUN apt-get update && \
    apt-get install -y apt-transport-https bison build-essential curl git gnupg && \
    curl https://faculty.math.illinois.edu/Macaulay2/PublicKeys/Macaulay2-key -s --output - | apt-key add - && \
    curl https://apt.kitware.com/keys/kitware-archive-latest.asc -s --output - | apt-key add - && \
    echo 'deb http://www.math.uiuc.edu/Macaulay2/Repositories/Ubuntu bionic main' > \
      /etc/apt/sources.list.d/macaulay2.list && apt-get clean && \
    echo 'deb https://apt.kitware.com/ubuntu/ bionic main' > \
      /etc/apt/sources.list.d/cmake.list && \
    apt-get update && apt-get install -y autoconf cmake libtool && \
    apt-get clean

# Libraries we require (except gc)
# TODO: need to build gc for large memory machines
RUN apt-get install -y libopenblas-dev libeigen3-dev \
    libxml2-dev libreadline-dev libomp-dev libtbb-dev \
    libgc-dev libgdbm-dev && apt-get clean

# Libraries we can build (factory not available on ubuntu)
RUN apt-get install -y fflas-ffpack libcdd-dev libgivaro-dev \
    libglpk-dev libgmp3-dev libmpc-dev libmpfr-dev libntl-dev \
    libflint-dev libfrobby-dev libatomic-ops-dev && apt-get clean

# Programs we can build
# TODO: libpari-dev still needed? cohomcalg available soon. Polymake requires firefox???
RUN apt-get install -y 4ti2 gfan normaliz coinor-csdp nauty lrslib && apt-get clean

# Emacs is optional
RUN apt-get install -y emacs

# Add non-root user for building and running Macaulay2
RUN useradd -G sudo -g root -u 1000 -m macaulay
USER 1000:1000

# FIXME: This address might not always be the same
ENV PKG_CONFIG_PATH /usr/lib/x86_64-linux-gnu/pkgconfig
ENV LDFLAGS         -L/usr/lib/x86_64-linux-gnu
ENV PATH            /home/macaulay/M2/M2/BUILD/build-docker:${PATH}

# Build the Docker image:
# make build-image

# Build Macaulay2:
# make build-M2

# Start a graphical instance:
# make run-graphical

# Start a non-graphical instance:
# make run-terminal

WORKDIR /home/macaulay/

ENTRYPOINT emacs
