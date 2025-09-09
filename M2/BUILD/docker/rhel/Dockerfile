ARG DISTRIBUTION=almalinux
ARG RELEASE=latest

FROM $DISTRIBUTION:$RELEASE

# Install dependencies
RUN dnf -y install autoconf automake bison boost-devel bzip2 chrpath \
    cmake diffutils flex gcc-c++ gcc-gfortran git gmp-devel info     \
    libffi-devel libtool libxml2-devel make mpfr-devel ncurses-devel \
    patch python3-devel readline-devel rpm-build tbb-devel which     \
    xz-devel zlib-devel
RUN test "$RELEASE" != "8" && dnf -y install flexiblas-devel || true

# Add non-root user for building and running Macaulay2
RUN useradd -G wheel -g root -u 1000 -m macaulay && echo "macaulay ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
USER 1000:0

ENV PATH /home/macaulay/M2/M2/BUILD/build-docker:${PATH}

WORKDIR /home/macaulay
ENTRYPOINT M2
