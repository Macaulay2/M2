ARG DISTRIBUTION=almalinux
ARG RELEASE=latest

FROM $DISTRIBUTION:$RELEASE
ARG RELEASE

# Install dependencies
RUN dnf -y install 'dnf-command(config-manager)' || true
# crb (powertools on EL8) for jansson-devel (EL9+) and openblas-devel (EL8)
RUN if [ "$RELEASE" = "8" ]; then \
        dnf config-manager --set-enabled powertools; \
    else \
        dnf config-manager --set-enabled crb; \
    fi
RUN dnf -y install autoconf automake bison boost-devel bzip2 chrpath \
    cmake diffutils flex gcc-c++ gcc-gfortran git gmp-devel info     \
    jansson-devel libffi-devel libtool libxml2-devel make mpfr-devel \
    ncurses-devel patch python3-devel readline-devel rpm-build       \
    tbb-devel which xz-devel zlib-devel
RUN if [ "$RELEASE" = "8" ]; then \
        dnf -y install openblas-devel; \
    else \
        dnf -y install flexiblas-devel; \
    fi

# Add non-root user for building and running Macaulay2
RUN useradd -G wheel -g root -u 1000 -m macaulay && echo "macaulay ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
USER 1000:0

ENV PATH /home/macaulay/M2/M2/BUILD/build-docker:${PATH}

WORKDIR /home/macaulay
ENTRYPOINT M2
