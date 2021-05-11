###############################################################################
## This script is responsible for finding location of libraries and programs.
## - force building those components:
##     cmake -DBUILD_LIBRARES="X Y Z" -DBUILD_PROGRAMS="A B C" .
##
## - list NTL variables:        cmake -LA . | grep NTL
##    reconfigure NTL variables: cmake -U*NTL* .

# These are the libraries linked with Macaulay2 in Macaulay2/{e,bin}/CMakeLists.txt
set(PKGLIB_LIST    FFLAS_FFPACK GIVARO)

set(LIBRARIES_LIST MPSOLVE MATHICGB MATHIC MEMTAILOR FROBBY FACTORY FLINT NTL MPFI MPFR MP BDWGC LAPACK)
set(LIBRARIES_LIST MPSOLVE MATHICGB MATHIC MEMTAILOR FROBBY FACTORY FLINT NTL MPFI MPFR MP BDWGC LAPACK TBB)
set(LIBRARY_LIST   READLINE HISTORY GDBM ATOMICOPS)

message(CHECK_START " Checking for existing libraries and programs")

###############################################################################
## Program requirements: (git and bison are checked for elsewhere)
find_package(PkgConfig	REQUIRED QUIET)
find_program(MAKE  NAMES gmake make)
find_program(ETAGS NAMES etags)

if(BUILD_DOCS)
  find_package(Doxygen)
  find_package(Sphinx)
endif()

###############################################################################
## Requirement	Debian package	RPM package	Homebrew package
#   Threads	libc6-dev	glibc-headers	N/A
#   LAPACK	libopenblas-dev	openblas-devel	N/A (Accelerate)
#   Boost	libboost-dev    boost-devel     boost (Regex and Stacktrace)
#   TBB 	libtbb-dev	tbb-devel	tbb
#   OpenMP	libomp-dev	libomp-devel	libomp (Optional)
#   GDBM	libgdbm-dev	gdbm-devel	gdbm
#   libatomic_ops libatomic_ops-dev libatomic_ops-devel libatomic_ops

# Set this variable to specify the linear algebra library.
# See `cmake --help-module FindLAPACK` for the list of options
#set(BLA_VENDOR OpenBLAS)

find_package(Threads	REQUIRED QUIET)
find_package(LAPACK	REQUIRED QUIET)
find_package(Boost	REQUIRED QUIET COMPONENTS regex OPTIONAL_COMPONENTS stacktrace_backtrace stacktrace_addr2line)
if(Boost_STACKTRACE_BACKTRACE_FOUND)
  set(Boost_stacktrace_lib "Boost::stacktrace_backtrace")
elseif(Boost_STACKTRACE_ADDR2LINE_FOUND)
  set(Boost_stacktrace_lib "Boost::stacktrace_addr2line")
else()
  #fallback to header only mode
  set(Boost_stacktrace_header_only YES)
endif()

find_package(TBB	REQUIRED QUIET) # See FindTBB.cmake
# TODO: replace gdbm, see https://github.com/Macaulay2/M2/issues/594
find_package(GDBM	REQUIRED QUIET) # See FindGDBM.cmake
# TODO: replace libatomic_ops, see https://github.com/Macaulay2/M2/issues/1113
find_package(AtomicOps	REQUIRED QUIET) # See FindAtomicOps.cmake

if(WITH_OMP)
  find_package(OpenMP REQUIRED)
else()
  find_package(OpenMP)
endif()
foreach(lang IN ITEMS C CXX)
  foreach(_dir IN LISTS OpenMP_${lang}_INCLUDE_DIRS)
    set(OpenMP_${lang}_FLAGS "${OpenMP_${lang}_FLAGS} -I${_dir}")
  endforeach()
  foreach(_lib IN LISTS OpenMP_${lang}_LIB_NAMES)
    get_filename_component(_libdir "${OpenMP_${_lib}_LIBRARY}" DIRECTORY)
    # TODO: remove when this is fixed: https://gitlab.kitware.com/cmake/cmake/-/issues/20934
    string(REGEX REPLACE "^lib" "" _lib "${_lib}")
    set(OpenMP_${lang}_LDLIBS "${OpenMP_${lang}_LDLIBS} -L${_libdir} -l${_lib}")
  endforeach()
endforeach()

###############################################################################
## Platform dependent requirements:
#   readline, history, termcap, ...
#    (provided by libreadline-dev on Ubuntu and readline )

# TODO: replace these two with libedit, see https://github.com/Macaulay2/M2/issues/825
find_package(Readline	REQUIRED QUIET) # See FindReadline.cmake
find_package(History	REQUIRED QUIET) # See FindHistory.cmake

###############################################################################
## Multi-precision arithmetic requirements:
#   gmp		GNU Multiple Precision Arithmetic Library
#   mpir	Multiple Precision Integers & Rationals
#
# CAUTION: switching from one to another in the same build directory is not advisable.
# mpir.h and gmp.h both serve as multiple precision rational and integer arithmetic
# libraries and are surrounded by #ifndef __GMP_H__ ... #endif so only one can be loaded.
# Similarly for gmpxx.h and mpirxx.h. However, the contents of the files differ.
# For example, mpf_cmp_z is defined only in gmp.h.

if(USING_MPIR)
  find_package(MPIR	3.0.0)
  set(MP_LIBRARY MPIR)
  include_directories(BEFORE ${CMAKE_SOURCE_DIR}/include/M2/gmp-to-mpir)
else()
  find_package(GMP	6.0.0 REQUIRED)
  set(MP_LIBRARY GMP)
endif()
# MP will mask either GMP or MPIR
foreach(var IN ITEMS FOUND ROOT INCLUDE_DIRS LIBRARY_DIRS LIBRARIES VERSION_OK)
  set(MP_${var} ${${MP_LIBRARY}_${var}})
endforeach()

###############################################################################
## Libraries we can download and build:
#   eigen3	C++ template library for linear algebra
#   bdw-gc	Boehm-Demers-Weiser conservative C/C++ Garbage Collector
#   mpir	Multiple Precision Integers & Rationals	(needs yasm)
#   mpfr	Multiple Precision Floating Point	(needs gmp)
#   mpfi	Multiple Precision F.-P. Interval	(needs gmp, mpfr)
#   ntl		Victor Shoup's Number Theory Library	(needs gmp, mpfr)
#   flint	Fast Library for Number Theory		(needs gmp, mpfr, ntl)
#   factory	Multivariate Polynomal Package		(needs gmp, mpfr, ntl, flint)
#   frobby	Computations With Monomial Ideals	(needs gmp)
#   cddlib	Double Description Method of Motzkin	(needs gmp)
#   mpsolve	Multiprecision Polynomial SOLVEr	(needs gmp, mpfr)
#   googletest	C++ unit-testing library
#   memtailor	special purpose memory allocators	(needs googletest + thread)
#   mathic	symbolic algebra data structures	(needs memtailor  + thread)
#   mathicgb	signature Groebner bases library	(needs mathic     + thread, tbb)
#   glpk	GNU Linear Programming Kit              (needs gmp)
#   givaro	prime field and algebraic computations	(needs gmp)
#  fflas_ffpack	Finite Field Linear Algebra Routines	(needs gmp, givaro + LAPACK)

find_package(Eigen3	3.3.0 PATHS ${M2_HOST_PREFIX})
find_package(BDWGC	7.6.4)
find_package(MPFR	4.0.1)
find_package(MPFI	1.5.1)
find_package(NTL       10.5.0)
find_package(Flint	2.6.0)
find_package(Factory	4.1.0)
find_package(MPSolve	3.2.0)
# TODO: add minimum version checks
find_package(Frobby	0.9.0)
find_package(CDDLIB)  # 0.94m?
find_package(GTest	1.10)
find_package(Memtailor	1.0.0)
find_package(Mathic	1.0.0)
find_package(Mathicgb	1.0.0)
find_package(GLPK      4.59.0)

pkg_search_module(FFLAS_FFPACK	IMPORTED_TARGET	fflas-ffpack>=2.4.3)
pkg_search_module(GIVARO	IMPORTED_TARGET	givaro>=4.1.1)
# TODO: add FindModules for these two as well

set(LIBRARY_OPTIONS
  Eigen3 BDWGC MPIR MPFR MPFI NTL Flint Factory Frobby cddlib MPSolve
  GTest Memtailor Mathic Mathicgb GLPK Givaro FFLAS_FFPACK)

###############################################################################
## Optional libraries:
#   LibXML2	libxml2-dev	libxml2-devel	N/A
#   MySQL	libmysql	mysql-devel	N/A
#   Python3	libpython3-dev	python3-libs	N/A

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

###############################################################################
## TODO: Do we still want these libraries?
#   fplll	Lattice algorithms using floating-point arithmetic	(uses mpir and mpfr)
#   linbox	Exact computational linear algebra	(needs fflas and givaro)
#   arb		arbitrary-precision ball arithmetic
## Requested by Greg Smith for future use:
#   cddplus	Double Description Method
#   lrslib	vertex enumeration/convex hull problems

###############################################################################
## Programs we can download and build:
#   4ti2	combinatorial problems on linear spaces		(needs gmp, glpk)
#   cohomCalg	sheaf cohomology for line bundles on toric varieties
#   Gfan	Grobner fans and tropical varieties		(needs gmp, cddlib, factory)
#   lrslib	vertex enumeration/convex hull problems		(needs gmp)
#   CSDP	semidefinite programming problems		(needs LAPACK, OpenMP)
#   Nauty	automorphism groups of graphs and digraphs
#   Normaliz	affine monoids, lattice polytopes, and rational cones		(needs gmp, nauty, OpenMB)
#   TOPCOM	triangulations of point configurations and oriented matroids	(needs cddlib)

find_program(4TI2	NAMES	circuits 4ti2-circuits 4ti2_circuits)
find_program(COHOMCALG	NAMES	cohomcalg)
find_program(GFAN	NAMES	gfan)
# TODO: library or program?
find_program(LRSLIB	NAMES	lrs)
# TODO: check for alternatives as well: sdpa or mosek
find_program(CSDP	NAMES	csdp)
find_program(NORMALIZ	NAMES	normaliz)
find_program(NAUTY	NAMES	dreadnaut nauty-dreadnaut)
find_program(TOPCOM	NAMES	checkregularity topcom-checkregularity)
# NOTE: we don't build the following by default, but some packages use them, so
# we provide targets build-polymake, build-bertini, build-phcpack for building them.
find_program(POLYMAKE	NAMES	polymake)
find_program(BERTINI	NAMES	bertini)
find_program(PHCPACK	NAMES	phc)
find_program(HOM4PS2	NAMES	hom4ps2) # TODO: http://www.math.nsysu.edu.tw/~leetsung/works/HOM4PS_soft.htm
# TODO: Maple and package convex

set(PROGRAM_OPTIONS 4ti2 cohomCalg Gfan lrslib CSDP Nauty Normaliz TOPCOM)

###############################################################################
## List installed components and unset those that we wish to build ourselves
set(INSTALLED_LIBRARIES "")
set(INSTALLED_PROGRAMS "")

# Detect which libraries are installed, which we want to build, and which we have yet to build
foreach(_library IN LISTS LIBRARY_OPTIONS)
  string(TOUPPER "${BUILD_LIBRARIES}" BUILD_LIBRARIES)
  string(TOUPPER "${_library}" _name)
  if(${_name}_FOUND)
    if(${_library}_DIR MATCHES ${M2_HOST_PREFIX} OR
        ((${_name}_INCLUDE_DIR MATCHES ${M2_HOST_PREFIX} OR ${_name}_INCLUDE_DIRS MATCHES ${M2_HOST_PREFIX}) AND
	  (${_name}_LIBRARY MATCHES ${M2_HOST_PREFIX}    OR ${_name}_LIBRARIES MATCHES ${M2_HOST_PREFIX} OR
	    ${_name}_LIBDIR MATCHES ${M2_HOST_PREFIX})))
      # we built it
      list(APPEND INSTALLED_LIBRARIES ${_library})
    elseif(BUILD_LIBRARIES MATCHES "(ALL|ON)" OR "${_name}" IN_LIST BUILD_LIBRARIES)
      # exists on the system, but we want to build it
      unset(${_library}_DIR CACHE) # for Eigen3
      unset(${_name}_FOUND CACHE)
      unset(${_name}_LIBDIR CACHE)
      unset(${_name}_LIBRARY CACHE)
      unset(${_name}_LIBRARIES CACHE)
      unset(${_name}_INCLUDEDIR CACHE)
      unset(${_name}_INCLUDE_DIR CACHE)
      unset(${_name}_INCLUDE_DIRS CACHE)
      # for GTest:
      unset(${_name}_MAIN_LIBRARY CACHE)
      unset(${_name}_MAIN_LIBRARY_DEBUG CACHE)
      unset(${_name}_LIBRARY_DEBUG CACHE)
      unset(${_name}_LIBRARY CACHE)
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
  if(EXISTS ${${_name}})
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
    unset(${_name} CACHE)
  endif()
endforeach()

###############################################################################
## Check that found libraries can be linked to catch linking conflicts early.
# When a conflict is detected, default to building all involved libraries.
# TIP: cmake --debug-trycompile keeps the termporary sources and binaries
option(CHECK_LIBRARY_COMPATIBILITY "Check for library incompatibilities" ON)

set(CMAKE_REQUIRED_LIBRARIES "")
set(CMAKE_REQUIRED_INCLUDES "")
set(CHECKED_LIBRARIES "")

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
    FAIL_REGEX "conflict")

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
## Set three library related definitions

if(GIVARO_FOUND)
  set(CMAKE_REQUIRED_INCLUDES "${GIVARO_INCLUDE_DIRS}")
  # whether givaro has isUnit (4.0.3) or isunit (4.0.2)
  check_cxx_source_compiles([[#include <givaro/gfq.h>
    int main(){class Givaro::GFqDom<long int> foo; foo.isunit(0);return 0;}]] HAVE_GIVARO_isunit)
else()
  unset(HAVE_GIVARO_isunit CACHE)
endif()

if(FACTORY_FOUND)
  set(CMAKE_REQUIRED_INCLUDES "${FACTORY_INCLUDE_DIR}")
  # whether factory was built with --enable-streamio
  check_cxx_source_compiles([[#include <factory/factory.h>
    int main(){Variable x; x = Variable(); std::cout << x;return 0;}]] FACTORY_STREAMIO)
  # whether Prem() from factory is public
  # TODO: revert cb64eb37 once factory is updated above 4.0.3 everywhere
  # see https://github.com/Macaulay2/M2/pull/1538#discussion_r537901750
  check_cxx_source_compiles([[#include <factory/factory.h>
    int main(){CanonicalForm p,q; Prem(p,q);return 0;}]] HAVE_FACTORY_PREM)
else()
  unset(FACTORY_STREAMIO CACHE)
endif()

if(FROBBY_FOUND)
  set(CMAKE_REQUIRED_INCLUDES "${FROBBY_INCLUDE_DIR};${MP_INCLUDE_DIRS}")
  # whether frobby has constants::version <0.9.4 or frobby_version >=0.9.4
  # TODO: remove when frobby is updated above 0.9.4 everywhere
  check_cxx_source_compiles([[#include <frobby.h>
    int main(){frobby_version;return 0;}]] HAVE_FROBBY_VERSION)
else()
  unset(HAVE_FROBBY_VERSION CACHE)
endif()
