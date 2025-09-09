# very loosely based on the fedora .spec:
# https://src.fedoraproject.org/rpms/Macaulay2/blob/rawhide/f/Macaulay2.spec

Name:    Macaulay2
Version: 1.25.06

# release convention: 0.x.m2 (so official fedora package will take precedence)
# increment x as needed, reset to 1 with each new m2 release
Release: 0.1.m2%{?dist}
Summary: System for algebraic geometry and commutative algebra

# https://github.com/Macaulay2/M2/issues/2604
%global _lto_cflags %{nil}

License: GPL-2.0-or-later
URL:     https://macaulay2.com/

Source0: %{name}-%{version}.tar.gz

# compile-time program dependencies
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: bison
BuildRequires: chrpath
BuildRequires: diffutils
BuildRequires: flex
BuildRequires: gcc-c++
BuildRequires: gcc-gfortran
BuildRequires: libtool
BuildRequires: make
BuildRequires: patch
%if 0%{?fedora}
BuildRequires: environment-modules
%endif

# libraries
BuildRequires: boost-devel
BuildRequires: gmp-devel
BuildRequires: libffi-devel
BuildRequires: libxml2-devel
BuildRequires: mpfr-devel
BuildRequires: ncurses-devel
BuildRequires: python3-devel
BuildRequires: readline-devel
BuildRequires: tbb-devel
BuildRequires: xz-devel
BuildRequires: zlib-devel
%if 0%{?rhel} >= 9 || 0%{?fedora}
BuildRequires: flexiblas-devel
%endif
%if 0%{?fedora}
BuildRequires: cddlib-devel
BuildRequires: eigen3-devel
BuildRequires: factory-devel
BuildRequires: fflas-ffpack-devel
BuildRequires: flint-devel
BuildRequires: gc-devel
BuildRequires: gdbm-devel
BuildRequires: glpk-devel
BuildRequires: gtest-devel
BuildRequires: libfrobby-devel
BuildRequires: libnauty-devel
BuildRequires: libnormaliz-devel
BuildRequires: mathic-devel
BuildRequires: mathicgb-devel
BuildRequires: memtailor-devel
BuildRequires: mpfi-devel
BuildRequires: mpsolve-devel
BuildRequires: ntl-devel
%endif

# run-time program dependencies
BuildRequires: which
Requires:      which
%if 0%{?fedora}
BuildRequires: 4ti2
Requires:      4ti2
BuildRequires: TOPCOM
Requires:      TOPCOM
BuildRequires: cohomCalg
Requires:      cohomCalg
BuildRequires: csdp-tools
Requires:      csdp-tools
Requires:      factory-gftables
BuildRequires: gfan
Requires:      gfan
BuildRequires: lrslib-utils
Requires:      lrslib-utils
BuildRequires: msolve
Requires:      msolve
BuildRequires: nauty
Requires:      nauty
BuildRequires: normaliz
Requires:      normaliz
%endif

Obsoletes: Macaulay2-common < %{version}-%{release}
Provides:  Macaulay2-common = %{version}-%{release}
Obsoletes: Macaulay2-doc < %{version}-%{release}
Provides:  Macaulay2-doc = %{version}-%{release}
Obsoletes: Macaulay2-emacs < %{version}-%{release}
Provides:  Macaulay2-emacs = %{version}-%{release}
Provides:  macaulay2 = %{version}-%{release}

%description
Macaulay 2 is a software system devoted to supporting research in
algebraic geometry and commutative algebra written by Daniel
R. Grayson and Michael E. Stillman

%prep
%setup -q
cd M2
./autogen.sh

%if 0%{?fedora}
%global config_args --with-system-libs --prefix=/usr
%else
%global config_args --prefix=/usr
%endif

%build
cd M2

# load lrslib module
%if 0%{?fedora} >= 41
source /etc/profile.d/modules.sh
module load lrslib-%{_arch}
%else
# trick build into thinking we've updated the submodules in rhel
for SUBMODULE in fflas_ffpack flint frobby gc givaro gtest mathic mathicgb \
		    memtailor
do
    touch libraries/$SUBMODULE/.submodule-updated
done
%endif

./configure %{config_args}
make %{?_smp_mflags}

%install
cd M2
%make_install

## unpackaged files
# info dir
rm -fv %{buildroot}%{_infodir}/dir

%files
%{_bindir}/M2
%{_bindir}/M2-binary
%{_prefix}/lib/Macaulay2/
%{_libexecdir}/Macaulay2/
%{_datadir}/emacs/site-lisp/macaulay2/
%{_datadir}/Macaulay2/
%{_docdir}/Macaulay2/
%{_infodir}/*.info*
%{_mandir}/man1/*

%changelog
* Fri Sep 12 2025 Doug Torrance <dtorrance@piedmont.edu> - 1.25.06-0.1.m2
- First draft of Macaulay2 spec file.
