ARG DISTRIBUTION=rockylinux
ARG RELEASE=8

FROM $DISTRIBUTION:$RELEASE
ARG RELEASE

# Configure EPEL
RUN dnf install -y 'dnf-command(config-manager)' && \
    if [ $RELEASE = 8 ];                            \
    then                                            \
      dnf config-manager --set-enabled powertools;  \
    else                                            \
      dnf config-manager --set-enabled crb;         \
    fi                                           && \
    dnf install -y epel-release

# Install dependencies
RUN dnf -y install autoconf automake bison boost-devel bzip2 cmake     \
    diffutils eigen3 flex gc-devel gcc-c++ gcc-gfortran gdbm-devel git \
    glpk-devel gmp-devel lapack-devel libffi-devel libtool             \
    libxml2-devel make mpfr-devel ncurses-devel openblas-devel patch   \
    python3-devel readline-devel tbb-devel which xz-devel zlib-devel

WORKDIR /home/macaulay
