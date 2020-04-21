###############################################################################
## This script is responsible for finding location of libraries and programs
## TIP: use cmake -LA . to list resolved variables
##      use cmake -U*NTL* . to reconfigure NTL variables

## Requirement	Debian package	RPM package	Homebrew package
#   BLAS/LAPACK	libopenblas-dev	openblas-devel	N/A
#   Threads	libc6-dev	glibc-headers	N/A
#   OpenMP	libomp-dev	libomp-devel	libomp
#   TBB		libtbb-dev	tbb-devel	tbb
#   GDBM	libgdbm-dev	gdbm-devel	gdbm
#   (also pkg-config, git, and bison + yasm for compiling MPIR)

## Optional	Debian package	RPM package	Homebew package
#   LibXML2	libxml2-dev	libxml2-devel	N/A
#   MySQL	libmysql	mysql-devel	N/A
#   Python3	libpython3-dev	python3-libs	N/A

## Platform dependent requirements:
#    readline, history, termcap, ...
#    (provided by libreadline-dev on Ubuntu)

## Libraries we can download and build:
#   eigen3	C++ template library for linear algebra
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
set(LIBRARY_LIST   HISTORY READLINE)
set(LIBRARIES_LIST LAPACK MP MPFR BDWGC NTL FLINT FACTORY FROBBY MATHICGB MATHIC MEMTAILOR MPSOLVE TBB)

## List of programs and libraries that we can build
set(LIBRARY_OPTIONS
  Eigen3 BDWGC MPIR MPFR NTL Flint Factory Frobby cddlib MPSolve
  Givaro FFLAS_FFPACK GLPK GTest Memtailor Mathic Mathicgb)
set(PROGRAM_OPTIONS 4ti2 cohomCalg Gfan lrslib CSDP Normaliz Nauty TOPCOM)

message(CHECK_START " Checking for existing libraries and programs")

################################################################
## pkg-config is useful for fflas-ffpack and certain other packages
find_package(PkgConfig   REQUIRED QUIET)

################################################################
## Look for prerequisite packages and libraries using CMake or pkg-config

## Find libraries available as CMake modules
## We provide modules for finding come of these libraries in cmake/
find_package(LAPACK      REQUIRED QUIET)
find_package(Threads 2.1 REQUIRED QUIET)
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
if(USING_MPIR)
  find_package(MPIR	3.0.0)
  set(MP_LIBRARY MPIR)
else()
  find_package(GMP	6.0.0 REQUIRED)
  set(MP_LIBRARY GMP)
endif()
# MP will mask either GMP or MPIR
foreach(var IN ITEMS FOUND INCLUDE_DIRS LIBRARIES VERSION_OK)
  set(MP_${var} ${${MP_LIBRARY}_${var}})
endforeach()

## These are not required because we can build them if they are not found.
find_package(MPFR	4.0.1)
find_package(BDWGC	7.6.4)
find_package(Eigen3	3.3.0 PATHS ${M2_HOST_PREFIX})
find_package(Factory	4.1.0)
find_package(Flint	2.5.3)
find_package(NTL       10.5.0)
# TODO: add minimum version checks
find_package(CDDLIB)  # 094h?
find_package(Mathic	1.0.0)
find_package(Mathicgb	1.0.0)
find_package(Memtailor	1.0.0)
find_package(MPSolve	3.1.8)
find_package(Frobby	0.9.0)
find_package(GLPK      4.59.0) # needed by 4ti2

if(BUILD_DOCS)
  find_package(Doxygen REQUIRED)
  find_package(Sphinx  REQUIRED)
endif()

if(BUILD_TESTING)
  find_package(GTest)
endif()

if(WITH_XML)
  find_package(LibXml2 2.9 REQUIRED)
  list(APPEND LIBRARIES_LIST LIBXML2)
endif()

if(WITH_SQL)
  find_package(SQLite3 3.0 REQUIRED)
  list(APPEND LIBRARIES_LIST SQLite3)
endif()

if(WITH_PYTHON)
  find_package(Python3 3.7 REQUIRED)
  list(APPEND LIBRARIES_LIST Python3)
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
find_program(MAKE	NAMES	make gmake nmake)
## Need a flavor of etags for building TAGS files
find_program(ETAGS	NAMES	etags ctags)

## Find external programs used by Macaulay2 packages
find_program(4TI2	NAMES	circuits)
find_program(COHOMCALG	NAMES	cohomcalg)
find_program(GFAN	NAMES	gfan)
# TODO: library or program?
find_program(LRSLIB	NAMES	lrs)
find_program(CSDP	NAMES	csdp)
find_program(NORMALIZ	NAMES	normaliz)
find_program(NAUTY	NAMES	dreadnaut)
find_program(TOPCOM	NAMES	checkregularity)
find_program(POLYMAKE	NAMES	polymake) # TODO

#############################################################################
## List installed components and unset those that we wish to build ourselves
set(INSTALLED_LIBRARIES "")
set(INSTALLED_PROGRAMS "")

# Detect which libraries are installed, which we want to build, and which we have yet to build
foreach(_library IN LISTS LIBRARY_OPTIONS)
  string(TOUPPER "${BUILD_LIBRARIES}" BUILD_LIBRARIES)
  string(TOUPPER "${_library}" _name)
  if(${_name}_FOUND)
    if(${_name}_INCLUDE_DIR MATCHES ${M2_HOST_PREFIX} OR
	${_name}_LIBRARIES MATCHES ${M2_HOST_PREFIX})
      # we built it
      list(APPEND INSTALLED_LIBRARIES ${_library})
    elseif(BUILD_LIBRARIES MATCHES "(ALL|ON)" OR "${_name}" IN_LIST BUILD_LIBRARIES)
      # exists on the system, but we want to build it
      unset(${_name}_FOUND)
      unset(${_library}_DIR CACHE)
      unset(${_name}_LIBRARIES CACHE)
      unset(${_name}_INCLUDE_DIR CACHE)
      unset(${_name}_INCLUDE_DIRS CACHE)
    else()
      # exists on the system
    endif()
  else()
    # was not found
  endif()
endforeach()

# Detect which programs are installed, which we want to build, and which we have yet to build
foreach(_program IN LISTS PROGRAM_OPTIONS)
  string(TOUPPER "${BUILD_PROGRAMS}" BUILD_PROGRAMS)
  string(TOUPPER "${_program}" _name)
  if(${_name})
    if(${_name} MATCHES ${M2_INSTALL_PROGRAMSDIR})
      # we built it
      list(APPEND INSTALLED_PROGRAMS ${_program})
    elseif(BUILD_PROGRAMS MATCHES "(ALL|ON)" OR "${_name}" IN_LIST BUILD_PROGRAMS)
      # exists on the system, but we want to build it
      unset(${_name} CACHE) # Unlike libraries, programs are set in cache
    else()
      # exists on the system
    endif()
  else()
    # was not found
  endif()
endforeach()

###############################################################################
## Check to make sure that the found libraries can be linked to catch linking conflicts early.
# when a conflict is detected, we default to building all involved libraries from source.
# TIP: cmake --debug-trycompile keeps the termporary sources and binaries
set(CHECK_LIBRARY_COMPATIBILITY ON)

set(CMAKE_REQUIRED_LIBRARIES "")
set(CMAKE_REQUIRED_INCLUDES "")
set(CHECKED_LIBRARIES "")

# FIXME: hack to force building mpir, mpfr, ntl, flint, factory
if(MP_LIBRARY MATCHES MPIR AND NOT MP_INCLUDE_DIRS MATCHES ${M2_HOST_PREFIX})
  unset(MP_FOUND)
  unset(MPIR_FOUND)
endif()
if(NOT MP_FOUND)
  unset(MPFR_FOUND)
  unset(NTL_FOUND)
  unset(FLINT_FOUND)
  unset(FACTORY_FOUND)
  unset(FROBBY_FOUND)
  unset(GIVARO_FOUND)
endif()
if(NOT GIVARO_FOUND)
  unset(FFLAS_FFPACK_FOUND)
endif()

if(CHECK_LIBRARY_COMPATIBILITY)
  message(CHECK_START " Checking library compatibility")

  foreach(LIB IN LISTS LIBRARIES_LIST)
    if(${LIB}_FOUND)
      list(APPEND CMAKE_REQUIRED_LIBRARIES ${${LIB}_LIBRARIES})
      list(APPEND CHECKED_LIBRARIES ${LIB})
    endif()
  endforeach()

  foreach(LIB IN LISTS PKGLIB_LIST)
    if(${LIB}_FOUND)
      list(APPEND CMAKE_REQUIRED_LIBRARIES PkgConfig::${LIB})
      list(APPEND CHECKED_LIBRARIES ${LIB})
    endif()
  endforeach()

  check_cxx_source_compiles([[int main(){return 0;}]] LIBRARY_COMPATIBILITY
    FAIL_REGEX "warning")

  if(NOT LIBRARY_COMPATIBILITY)
    message(CHECK_FAIL " Detected library incompatibilities; rerun the build-libraries target")
    foreach(LIB IN LISTS CHECKED_LIBRARIES)
      unset(${LIB}_FOUND)
    endforeach()
  else()
    message(CHECK_PASS " Libraries are compatible!")
  endif()

  unset(LIBRARY_COMPATIBILITY CACHE)
endif()

###############################################################################

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
