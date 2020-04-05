###############################################################################
## This script is responsible for finding location of libraries and programs
## TIP: use cmake -LA . to list resolved variables
##      use cmake -U*NTL* . to reconfigure NTL variables

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

## These lists will be used in Macaulay2/{e,bin}/CMakeLists.txt
set(PKGLIB_LIST    FFLAS_FFPACK GIVARO)
set(LIBRARY_LIST   HISTORY READLINE MPSOLVE)
set(LIBRARIES_LIST LAPACK LIBXML2 MP BDWGC MPFR NTL FLINT FACTORY MATHICGB MATHIC MEMTAILOR TBB FROBBY)

################################################################
## pkg-config is useful for fflas-ffpack and certain other packages
find_package(PkgConfig  REQUIRED QUIET)

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
# mpir.h and gmp.h both serve as multiple precision rational and integer arithmetic
# libraries and are surrounded by #ifndef __GMP_H__ ... #endif so only one can be loaded.
# Similarly for gmpxx.h and mpirxx.h.  However, the contents of the files differ.
# For example, mpf_cmp_z is defined only in gmp.h.
# TODO: find a way so switching from one to another is possible
if(MP_LIBRARY MATCHES "gmp|GMP")
  find_package(GMP	6.0.0)
  set(MP_LIBRARY GMP)
elseif(MP_LIBRARY MATCHES "mpir|MPIR")
  find_package(MPIR	3.0.0)
  # TODO: use WITH_MPIR instead of MP_LIBRARY?
  set(WITH_MPIR ON)
  set(MP_LIBRARY MPIR)
else()
  message(FATAL "multiple precision rational and integer arithmetic library not found")
endif()

# MP will mask either GMP or MPIR
foreach(var IN ITEMS FOUND INCLUDE_DIRS LIBRARIES VERSION_OK)
  set(MP_${var} ${${MP_LIBRARY}_${var}})
endforeach()

## These are not required because we can build them if they are not found.
find_package(MPFR	4.0.1)
find_package(BDWGC	7.6.4)
find_package(Factory	4.1.0)
find_package(Flint	2.5.3)
find_package(NTL       10.5.0)
# TODO: add minimum version checks
find_package(CDD)     # 094h?
find_package(Mathic	1.0.0)
find_package(Mathicgb	1.0.0)
find_package(Memtailor	1.0.0)
find_package(MPSolve	3.1.8)
find_package(Frobby	0.9.0)
find_package(GLPK      4.59.0) # needed by 4ti2

if(BUILD_TESTING)
  find_package(GTest)
endif()

## Find libraries available via pkg-config
# TODO: take care of c flags such as -fopenmp and -fabi-version=6
pkg_search_module(FFLAS_FFPACK	IMPORTED_TARGET	fflas-ffpack>=2.3.2)
pkg_search_module(GIVARO	IMPORTED_TARGET	givaro>=4.0.3)

# TODO: remove this or deal with it differently
# IDEA: replace with capnproto.org or msgpack.org
# Alternatively protobuf: https://developers.google.com/protocol-buffers/docs/proto#maps
find_library(LIBGDBM gdbm)

## Need a flavor of make for building libraries and programs
find_program(MAKE_EXE	NAMES	make gmake nmake)
## Need a flavor of etags for building TAGS files
find_program(ETAGS	NAMES	etags ctags)

## Find external programs used by Macaulay2 packages
find_program(4TI2	NAMES	4ti2-circuits circuits	PATHS ${M2_INSTALL_PROGRAMSDIR}/bin)
find_program(COHOMCALG	NAMES	cohomcalg		PATHS ${M2_INSTALL_PROGRAMSDIR}/bin)
find_program(GFAN	NAMES	gfan			PATHS ${M2_INSTALL_PROGRAMSDIR}/bin)
# TODO: library or program?
find_program(LRSLIB	NAMES	lrs			PATHS ${M2_INSTALL_PROGRAMSDIR}/bin)
find_program(CSDP	NAMES	csdp			PATHS ${M2_INSTALL_PROGRAMSDIR}/bin)
find_program(NORMALIZ	NAMES	normaliz		PATHS ${M2_INSTALL_PROGRAMSDIR}/bin)
find_program(NAUTY	NAMES	dreadnaut nauty-complg	PATHS ${M2_INSTALL_PROGRAMSDIR}/bin)
find_program(TOPCOM	NAMES	checkregularity		PATHS ${M2_INSTALL_PROGRAMSDIR}/bin)
find_program(POLYMAKE	NAMES	polymake) # TODO

###############################################################################

# TODO: do we use these?
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

## Check to make sure that the libraries found can be linked together
# This catches linking conflicts early.
# eg: don't check MPFR if choice of MP isn't found
# TODO: is it better to directly use try_compile?
# TIP: cmake --debug-trycompile keeps the termporary sources and binaries
set(CMAKE_REQUIRED_LIBRARIES "")
set(CMAKE_REQUIRED_INCLUDES "")
set(CHECK_LIBRARY_COMPATIBILITY ON)

foreach(LIB IN LISTS LIBRARY_LIST LIBRARIES_LIST PKGLIB_LIST)
  if(NOT ${LIB}_FOUND)
    set(CHECK_LIBRARY_COMPATIBILITY OFF)
    break()
  endif()
endforeach()

if(CHECK_LIBRARY_COMPATIBILITY)
  message("## All libraries are found. Checking library compatibility ...")

  foreach(LIB IN LISTS LIBRARY_LIST)
    list(APPEND CMAKE_REQUIRED_LIBRARIES ${${LIB}_LIBRARY})
  endforeach()

  foreach(LIB IN LISTS LIBRARIES_LIST)
    list(APPEND CMAKE_REQUIRED_LIBRARIES ${${LIB}_LIBRARIES})
  endforeach()

  foreach(LIB IN LISTS PKGLIB_LIST)
    list(APPEND CMAKE_REQUIRED_LIBRARIES PkgConfig::${LIB})
  endforeach()

  check_cxx_source_compiles([[int main(){return 0;}]] LIBRARY_COMPATIBILITY)

  if(NOT LIBRARY_COMPATIBILITY)
    unset(FACTORY_FOUND)
    unset(FROBBY_FOUND)
    unset(FLINT_FOUND)
    unset(MPFR_FOUND)
    unset(NTL_FOUND)
    unset(GLPK_FOUND)
  endif()

  unset(LIBRARY_COMPATIBILITY CACHE)

else()
  message("## The build-libraries target is not yet finished.")
endif()
