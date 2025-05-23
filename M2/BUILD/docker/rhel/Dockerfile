ARG DISTRIBUTION=rockylinux
ARG RELEASE=9

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
    glpk-devel gmp-devel gtest-devel lapack-devel libffi-devel libtool \
    libxml2-devel make mpfr-devel ncurses-devel ninja-build            \
    openblas-devel patch python3-devel R readline-devel rpm-build      \
    tbb-devel which xz-devel zlib-devel

# Add non-root user for building and running Macaulay2
RUN useradd -G wheel -g root -u 1000 -m macaulay && echo "macaulay ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
USER 1000:0

ENV PATH /home/macaulay/M2/M2/BUILD/build-docker:${PATH}

WORKDIR /home/macaulay
ENTRYPOINT M2
