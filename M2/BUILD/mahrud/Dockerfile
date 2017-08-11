FROM fedora:latest
MAINTAINER mahrud <mahrud@berkeley.edu>

RUN dnf -y update && dnf clean all
RUN dnf -y install autoconf bison boost-devel cddlib-devel emacs factory-devel fakeroot flex gc-devel \
           gcc-c++ gdbm-devel git glpk-devel gmp-devel kernel-devel lapack-devel libatomic_ops-devel libfac-devel \
           libgfortran-static libtool libxml2-devel mpfr-devel mpir-devel ncurses-devel ntl-devel pari-devel patch \
           readline-devel rpm-build rpm-sign rpmdevtools subversion libmpc-devel gtest-devel givaro-devel frobby \
           flint-devel yasm libmpc-devel time && dnf clean all
