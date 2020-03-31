###############################################################################
## This script is responsible for finding location of libraries and programs
## TIP: use cmake -LA to list resolved variables

## Requirement	Debian package	RPM package	OSX package
#   BLAS/LAPACK	libopenblas-dev	openblas-devel	
#   Eigen3	libeigen3-dev	eigen3-devel	
#   Threads	libc6-dev	glibc-headers	
#   LibXML2	libxml2-dev	libxml2-devel	
#   OpenMP	libomp-dev	libomp-devel	
#   TBB		libtbb-dev	tbb-devel	
#   GDBM	libgdbm-dev	gdbm-devel	
#   (also pkg-config, git, and bison + yasm for compiling MPIR)

## Platform dependent requirements:
#    readline, history, termcap, ...
#    (provided by libreadline-dev on Ubuntu)

## Libraries we can download and build:
#   bdw-gc	Boehm-Demers-Weiser conservative C/C++ Garbage Collector
#   mpir	Multiple Precision Integers & Rationals	(optional plug-in replacement for gmp)
#   mpfr	Multiple Precision Floating Point	(needs gmp)
#   ntl		Victor Shoup's Number Theory Library	(needs gmp, mpfr)
#   flint	Fast Library for Number Theory		(needs gmp, ntl)
#   factory	Multivariate Polynomal Package		(needs flint, ntl and gmp)
#   frobby	Computations With Monomial Ideals	(needs on gmp)
#   cddlib	Double Description Method of Motzkin	
#   glpk	GNU Linear Programming Kit
#   mpsolve	Multiprecision Polynomial SOLVEr
#   givaro	prime field and algebraic computations	(needs gmp)
#   fflas	Finite Field Linear Algebra Routines	(needs givaro and LAPACK)
#   memtailor	special purpose memory allocators	(needs pthread)
#   mathic	symbolic algebra data structures	(needs memtailor and pthread)
#   mathicgb	signature Groebner bases library	(needs mathic, memtailor, pthread, and tbb)
#   googletest	C++ unit-testing library

## Programs we can download and build:
#   4ti2 gfan normaliz csdp cohomcalg nauty topcom lrslib (we don't build Polymake)

## TODO: Do we still want these?
#   fplll	Lattice algorithms using floating-point arithmetic	(uses mpir and mpfr)
#   linbox	Exact computational linear algebra	(needs fflas and givaro)
## Requested by Greg Smith for future use:
#   cddplus	Double Description Method
#   lrslib	vertex enumeration/convex hull problems

################################################################
## pkg-config is useful for fflas-ffpack and certain other packages
find_package(PkgConfig  REQUIRED QUIET)

## Setting the prefix so pkg-config can find libraries we've built
list(APPEND CMAKE_PREFIX_PATH ${M2_HOST_PREFIX})
set(ENV{PKG_CONFIG_PATH}      ${M2_HOST_PREFIX}/lib/pkgconfig:$ENV{PKG_CONFIG_PATH})

################################################################
## Look for prerequisite packages and libraries using CMake or pkg-config

## Find libraries available as CMake modules
## We provide modules for finding come of these libraries in cmake/
find_package(LAPACK      REQUIRED QUIET)
find_package(Eigen3  3.3 REQUIRED QUIET NO_MODULE)
find_package(Threads 2.1 REQUIRED QUIET)
find_package(LibXml2 2.9 REQUIRED QUIET) # d/xml-c.c needs xmlNewNode
find_package(Readline    REQUIRED QUIET)
find_package(History     REQUIRED QUIET)
# TODO: replace readline with https://github.com/AmokHuginnsson/replxx

# TODO: which to use: TBB or OpenMP?
# OpenMP is required for building the library csdp and good for building normaliz
find_package(OpenMP	REQUIRED QUIET)
# TBB is required for threading in mathicgb
find_package(TBB	REQUIRED QUIET) # required by mathicgb

## Two options for multiprecision arithmetic
find_package(GMP	6.0.0)
find_package(MPIR	3.0.0)

## These are not required because we can build them if they are not found.
find_package(MPFR	4.0.1)
find_package(BDWGC	7.6.4)
find_package(Flint	2.5.3)
# TODO: add minimum version checks
find_package(CDD)     # 094h?
find_package(NTL	10.5.0)
find_package(GLPK	4.59.0)
find_package(Frobby	0.9.0)
find_package(Memtailor	1.0.0)
find_package(Mathic	1.0.0)
find_package(Mathicgb	1.0.0)
find_package(MPSolve	3.1.8)

if(BUILD_TESTING)
  find_package(GTest)
endif()

## Find libraries available via pkg-config
# TODO: add relevant X_CFLAGS_OTHER flags!
# TODO: investigate error when factory-devel package is installed:
# sample Factory finite field addition table file missing, needed for factorization:
# /home/mahrud/Projects/M2/M2/M2/BUILD/mahrud/build/usr-dist//usr/share/factory/
pkg_search_module(FACTORY	factory>=4.1.1
                                singular-factory>=4.1.1	IMPORTED_TARGET)
pkg_search_module(FFLAS_FFPACK	fflas-ffpack>=2.2.2	IMPORTED_TARGET)
pkg_search_module(GIVARO	givaro>=4.0.2		IMPORTED_TARGET)

# TODO: remove this or deal with it differently
find_library(LIBGDBM gdbm)

## Need a flavor of make for building libraries and programs
find_program(MAKE_EXE	NAMES	make gmake nmake)
## Need a flavor of etags for building TAGS files
find_program(ETAGS	NAMES	etags ctags)

## Find external programs used by Macaulay2 packages
find_program(4TI2	NAMES	4ti2-circuits circuits	PATH ${M2_HOST_PREFIX}/bin)
find_program(COHOMCALG		cohomcalg		PATH ${M2_HOST_PREFIX}/bin)
find_program(GFAN		gfan			PATH ${M2_HOST_PREFIX}/bin)
# TODO: library or program?
find_program(LRSLIB		lrs			PATH ${M2_HOST_PREFIX}/bin)
find_program(CSDP		csdp			PATH ${M2_HOST_PREFIX}/bin)
find_program(NORMALIZ		normaliz		PATH ${M2_HOST_PREFIX}/bin)
find_program(NAUTY	NAMES	dreadnaut nauty-complg	PATH ${M2_HOST_PREFIX}/bin)
find_program(TOPCOM	NAMES	checkregularity		PATH ${M2_HOST_PREFIX}/bin)
find_program(POLYMAKE	NAMES	polymake)

###############################################################################

# TODO: do we use these?
## topcom depends on cddlib, but includes setoper.h, rather than cdd/setoper.h, so we do, too
if(CDD_FOUND)
  check_include_files(setoper.h HAVE_CDDLIB)
endif()

if(NTL_FOUND)
  check_include_files(NTL/version.h HAVE_NTL)
endif()

if(GIVARO_FOUND)
  set(CMAKE_REQUIRED_INCLUDES "${GIVARO_INCLUDE_DIRS}")
  # whether givaro has isUnit (4.0.3) or isunit (4.0.2)
  check_cxx_source_compiles([[#include <givaro/gfq.h>
    int main(){class Givaro::GFqDom<long int> foo; foo.isunit(0);return 0;}]] HAVE_GIVARO_isunit)
endif()

if(FACTORY_FOUND)
  set(CMAKE_REQUIRED_INCLUDES "${FACTORY_INCLUDE_DIRS}")
  # whether factory was built with --enable-streamio
  check_cxx_source_compiles([[#include <factory/factory.h>
    int main(){Variable x; x = Variable(); std::cout << x;return 0;}]] FACTORY_STREAMIO)
  # whether Prem() from factory is public
  check_cxx_source_compiles([[#include <factory/factory.h>
    int main(){CanonicalForm p,q; Prem(p,q);return 0;}]] HAVE_FACTORY_PREM)
endif()
