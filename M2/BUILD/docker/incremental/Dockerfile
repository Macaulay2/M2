FROM ubuntu:24.04 AS build-dependencies

ENV DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates curl gnupg software-properties-common && \
    curl -fsSL https://apt.kitware.com/keys/kitware-archive-latest.asc | \
        gpg --dearmor -o /usr/share/keyrings/kitware.gpg
RUN echo 'deb [signed-by=/usr/share/keyrings/kitware.gpg] https://apt.kitware.com/ubuntu/ noble main' \
        > /etc/apt/sources.list.d/kitware.list && \
    add-apt-repository -y -n ppa:macaulay2/macaulay2 && apt-get update && \
    apt-get install -y --no-install-recommends \
        autoconf automake bison build-essential ccache cmake dpkg-dev emacs-nox flex \
        gfortran git libtool-bin ninja-build pkg-config python3 python3-dev rsync texinfo yasm \
        liblzma-dev libboost-stacktrace-dev libboost-regex-dev libncurses-dev libreadline-dev \
        libeigen3-dev libopenblas-dev libxml2-dev libgc-dev libgdbm-dev libglpk-dev libgmp3-dev \
        libgtest-dev libmpfr-dev libmpfi-dev libntl-dev libgivaro-dev fflas-ffpack libflint-dev \
        libmps-dev libfrobby-dev libsingular-dev singular-data libcdd-dev libnormaliz-dev \
        libnauty-dev libtbb-dev libfplll-dev libjansson-dev libffi-dev \
        gfan cohomcalg topcom 4ti2 normaliz coinor-csdp nauty lrslib polymake phcpack qepcad \
        msolve r-base palp && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

RUN useradd --uid 10001 --create-home builder && \
    mkdir -p /opt/m2 && chown builder:builder /opt/m2
ENV HOME=/home/builder
USER 10001:10001
WORKDIR /opt/m2
CMD ["bash"]

# Prebuild every library/program selected by M2's normal fallback detection.
# Preserve their installation paths and ExternalProject stamps in the same PAX
# archive used by the full build. No M2 runtime or package installation runs here.
FROM build-dependencies AS dependency-builder
COPY . /input
ARG BUILD_JOBS=2
RUN CMAKE_BUILD_PARALLEL_LEVEL="$BUILD_JOBS" bash /input/.github/ci/container-build.sh dependencies

FROM build-dependencies
COPY --chown=10001:10001 --from=dependency-builder /opt/m2/build-cache.tar /opt/m2/build-cache.tar
