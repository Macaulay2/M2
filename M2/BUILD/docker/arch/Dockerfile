# Time usage: <15min
# Net usage:  ~256 + tarfiles/clones
# Disk usage: ~1.25GB docker image + ~2G Macaulay2 build

FROM archlinux:latest

# manually run all alpm hooks that can't be run inside the fakechroot
RUN ldconfig && update-ca-trust && locale-gen
RUN sh -c 'ls usr/lib/sysusers.d/*.conf | /usr/share/libalpm/scripts/systemd-hook sysusers'

# initialize the archilnux keyring, but discard any private key that may be shipped.
RUN pacman-key --init && pacman-key --populate archlinux
RUN rm -rf etc/pacman.d/gnupg/{openpgp-revocs.d/,private-keys-v1.d/,pugring.gpg~,gnupg.S.}*

# Build tools
RUN pacman --sync --noconfirm --refresh --needed \
  	autoconf automake bison make patch ccache \
	gettext python cmake curl gcc git gnupg \
	libtool pkg-config ninja yasm

# Required libraries
RUN pacman --sync --noconfirm --needed openblas openmp intel-tbb libxml2 readline gdbm boost

# Libraries we can build
RUN pacman --sync --noconfirm --needed eigen fflas-ffpack givaro glpk gmp gc gtest mpfr ntl libatomic_ops

# Optional packages
RUN pacman --sync --noconfirm mlocate bash-completion

RUN useradd -g root -u 1000 -m macaulay
USER 1000:0

ENV PATH            /home/macaulay/M2/M2/BUILD/build-docker:${PATH}
ENV CC              /usr/lib/ccache/bin/gcc
ENV CXX             /usr/lib/ccache/bin/g++

WORKDIR /home/macaulay
ENTRYPOINT M2
