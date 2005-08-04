# RPM spec file for Macaulay2
# by Christian Cornelssen <ccorn@cs.tu-berlin.de>

%define factory_version	2-0-4
%define factory_dir	factory
%define libfac_version	2.0.4
%define libfac_dir	libfac

Name:		Macaulay2
Version:	0.9.2
Release:	1
License:	GPL

URL:		http://www.math.uiuc.edu/Macaulay2/

Source0:	ftp://Macaulay2:Macaulay2@ftp.math.uiuc.edu/Macaulay2-%{version}-src.tar.gz
Source1:	ftp://Macaulay2:Macaulay2@ftp.math.uiuc.edu/README-%{version}-src
Patch0:		Macaulay2-%{version}-Makefile.in.diff
Patch1:		Macaulay2-%{version}-configure.ac.diff
Patch2:		Macaulay2-%{version}-configure.diff
Source3:	ftp://www.mathematik.uni-kl.de/pub/Math/Singular/Factory/factory-%{factory_version}.tar.gz
Patch3:		factory-%{factory_version}-NTLconvert.diff
Source5:	ftp://www.mathematik.uni-kl.de/pub/Math/Singular/Libfac/libfac-%{libfac_version}.tar.gz
Patch5:		libfac-%{libfac_version}-Makefile.in.diff
Patch6:		libfac-%{libfac_version}-version.cc.diff
Patch7:		libfac-%{libfac_version}-configure.in.diff
Patch8:		libfac-%{libfac_version}-configure.diff
Patch9:		libfac-%{libfac_version}-tmpl_inst.cc.diff

Buildroot:	%{_tmppath}/%{name}-root
BuildRequires:	gc-devel gmp-devel ntl-devel gdbm-devel

Requires:	gc gmp ntl gdbm
Group:		Applications/Math
Summary:	Computational Algebraic Geometry System

%description
Macaulay 2 is a software system for algebraic geometry research, written by
Daniel R. Grayson and Michael E. Stillman with generous support of the
National Science Foundation, for which we thank them.

%prep
%setup -q -T -b0 -a3 -a5
%patch0 -p0 -b .orig
%patch1 -p0 -b .orig
%patch2 -p0 -b .orig
cd %{factory_dir}
touch configure		# timestamp problem; avoid autoconf
%patch3 -p0 -b .orig
cd ../%{libfac_dir}
%patch5 -p0 -b .orig
%patch6 -p0 -b .orig
%patch7 -p0 -b .orig
%patch8 -p0 -b .orig
%patch9 -p0 -b .orig

%build
facprefix=$PWD/fac
: ${CC=gcc}
: ${CXX=g++}
: ${CFLAGS=$RPM_OPT_FLAGS}
: ${CXXFLAGS=$RPM_OPT_FLAGS}
CXXFLAGS="$CXXFLAGS -fno-exceptions"
CPPFLAGS="-I$facprefix/include $CPPFLAGS"
LDFLAGS="-L$facprefix/lib $LDFLAGS"
LIBS="-lntl -lgmp -ldl -lpthread -lm $LIBS"
export CC CXX CPPFLAGS CFLAGS CXXFLAGS LDFLAGS LIBS

########################### factory ###########################
cd %{factory_dir}
# Special factory configure options:
#  --with-memman(=<memman>) specify Factory memory manager.
#                          <memman> is either `old' (default), `new', or `no'.
#  --with-omalloc           build for use with omalloc
#  --with-gmp(=<gmp_inc_path>(,<gmp_lib_path>))
#                          specify where to find gmp library.
#  --with-Singular         build for use with computer algebra system Singular.
#  --with-NTL              build for use with NTL.
#  --enable-cf-inline      build Factory with "configurable inline methods"
#                          enabled.
#  --disable-streamio      build Factory without stream IO
#  --enable-memdebug=<level> switch on memory management debugging.  Meaningful
#                          with `--with-memman=new' only.  <level> may be either
#                          `no' (default), `normal', or `full'.
#  --enable-assertions     build Factory with assertions activated
#  --enable-timing         build Factory so it will print timing information
#  --enable-debugoutput    build Factory so it will print debugging information
#  --enable-gmp            together with `--with-Singular' means: installation
#                          in process, be graceful when there is no `gmp.h'
CXXFLAGS="$CXXFLAGS -fno-rtti" \
./configure --prefix=$facprefix \
	--with-gmp --with-NTL --disable-streamio
make
make install

########################### libfac ###########################
cd ../%{libfac_dir}

# Special libfac configure options:
#   --with-debug
#   --with-Singular		don't instantiate templates
ac_cv_c_const=yes ac_cv_c_inline=yes \
CPPFLAGS="$CPPFLAGS -DHAVE_SINGULAR_ERROR=1" \
CXXFLAGS="$CXXFLAGS -fno-rtti" \
./configure --prefix=$facprefix
make
#make tests
make install

########################### Macaulay2 ###########################
cd ..
# Special Macaulay2 configure options:
#  --disable-dumpdata      do not cache data with dumpdata
#  --enable-dumpdata=old   cache data with the old version of dumpdata
#  --enable-profile        enable profiling (and disable stripping)
#  --enable-debug          enable debugging
#  --enable-verbose        enable verbose memory allocation
#  --enable-static         enable static linking
#  --enable-memdebug       enable memory allocation debugging
#  --disable-optimize      disable optimization
#  --disable-strip         do not strip the Macaulay 2 binary
#  --enable-encap          create an encap package
#  --enable-gc-for-new     use gc instead of builtin_new
./configure \
	--prefix=%{_prefix} \
	--exec-prefix=$(echo %{_exec_prefix} | sed 's,^%{_prefix},${prefix},')\
	--bindir=$(echo %{_bindir} | sed 's,^%{_prefix},${prefix},')\
	--libdir=$(echo %{_libdir} | sed 's,^%{_prefix},${prefix},')\
	--enable-gc-for-new
make
#(cd Macaulay2/test && touch memleak3.okay global.okay) # Timeout (10m)
#make -k check || true

%install
rm -rf %{buildroot}
make target=%{buildroot}%{_prefix} install
config/install-sh -d %{buildroot}%{_docdir}/%{name}
for i in COPYING CHANGES README html; do
  ln -s %{_libdir}/%{name}-%{version}/$i %{buildroot}%{_docdir}/%{name}/$i
done

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root)
%{_bindir}/*
%{_libdir}/*
%doc %{_docdir}/*

%changelog
* Sat Mar 29 2003  Christian Cornelssen <ccorn@cs.tu-berlin.de> 0.9.2-1
- initial spec file
