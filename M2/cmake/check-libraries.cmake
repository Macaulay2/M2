## tip: use cmake -LA to list resolved variables
#  TODO: Greg Smith requested cddplus and lrslib for future use

# Requirements:
#    BLAS/LAPACK: lapack includes blas, makes both libblas and liblapack
#    Eigen3
#    Threads
#    LibXML2
#    LibLZMA
#    OpenMP
#    TBB
#    (also pkg-config, git, and bison for compiling)

# Platform dependent requirements:
#    readline, history, termcap, ...

# The list LIBLIST is the list of libraries that might be used and linked into M2.
#set(LIBLIST
#   gc gdbm mpir mpfr ntl flint factory # lapack
#   frobby glpk cddlib fplll givaro linbox boost mpc # qd gtest
#  )

# Libraries we can build:
#    mpir is a plug-in replacement for gmp and can provide libgmp and libgmpxx, too. (optional)
#    flint depends on gmp (or mpir) and mpfr
#    factory needs flint, ntl and gmp; it includes and installs gftables, so doesn't need it separately
#    givaro uses gmp (or mpir)
#    frobby depends on gmp
##   googletest
##    bdw-gc
#    fflas_ffpack needs givaro and lapack
#    memtailor needs pthread
#    mathic needs memtailor and pthread
#    mathicgb needs mathic, memtailor, pthread, and tbb

################################################################
## pkg-config is useful for fflas-ffpack and certain other packages
find_package(PkgConfig  REQUIRED QUIET)

## Setting the prefix so pkg-config can find libraries we've built
list(APPEND CMAKE_PREFIX_PATH ${M2_HOST_PREFIX})
set(ENV{PKG_CONFIG_PATH}      ${M2_HOST_PREFIX}/lib/pkgconfig:$ENV{PKG_CONFIG_PATH})
# TODO: the latter should be unnecessary:
# https://cmake.org/cmake/help/latest/module/FindPkgConfig.html#variable:PKG_CONFIG_USE_CMAKE_PREFIX_PATH

################################################################
## Look for prerequisite packages and libraries using CMake or pkg-config

## Find libraries available as CMake modules
# TODO: specify minimum version
# TODO: use LAPACK/BLAS from Eigen3
find_package(LAPACK      REQUIRED QUIET) # also sets BLAS variables
find_package(Eigen3  3.3 REQUIRED QUIET NO_MODULE)
find_package(Threads 2.1 REQUIRED QUIET) # pthread
find_package(LibXml2 2.9 REQUIRED QUIET) # d/xml-c.c needs xmlNewNode
find_package(Readline    REQUIRED QUIET)
find_package(History     REQUIRED QUIET)
# TODO: replace readline with https://github.com/AmokHuginnsson/replxx
#find_package(LibLZMA 5.2 REQUIRED QUIET) # TODO: where is this needed?
# TODO: which to use: TBB or OpenMP?
# OpenMP is required for building the library csdp and good for building the library normaliz
find_package(OpenMP      REQUIRED QUIET)
# TBB is required for threading in mathicgb
find_package(TBB         REQUIRED) # required by mathicgb

## We provide modules for finding these libraries in cmake/
## They are not required because we can build them if they are not found.
# TODO: specify minimum version
find_package(CDD)
find_package(MPIR)
find_package(Flint)
find_package(Frobby)
find_package(Memtailor)
find_package(Mathic)
find_package(Mathicgb)

find_package(GMP  6.1.0 QUIET)
find_package(MPC  1.1.0 QUIET)
find_package(MPFR 4.0.1 QUIET)
find_package(NTL  10.5.0 QUIET)

## Find libraries available via pkg-config
# TODO: investigate error when factory-devel package is installed:
# sample Factory finite field addition table file missing, needed for factorization:
# /home/mahrud/Projects/M2/M2/M2/BUILD/mahrud/build/usr-dist//usr/share/factory/
pkg_search_module(FACTORY	factory>=4.1.1
                                singular-factory>=4.1.1	IMPORTED_TARGET)
pkg_search_module(FFLAS_FFPACK	fflas-ffpack>=2.2.2	IMPORTED_TARGET)
pkg_search_module(GIVARO	givaro>=4.0.2		IMPORTED_TARGET)
pkg_search_module(BDW_GC	bdw-gc			IMPORTED_TARGET)

# TODO: remove this or deal with it differently
find_library(LIBGDBM gdbm)

## Need a flavor of make for building libraries and programs
find_program(MAKE_EXE NAMES make gmake nmake)

## Find external programs used by Macaulay2 packages
find_program(4TI2	NAMES	4ti2-circuits circuits	PATH ${M2_HOST_PREFIX}/bin)
find_program(COHOMCALG		cohomcalg		PATH ${M2_HOST_PREFIX}/bin)
find_program(GFAN		gfan			PATH ${M2_HOST_PREFIX}/bin)
# TODO: library or program?
find_program(LRSLIB		lrs			PATH ${M2_HOST_PREFIX}/bin)
find_program(CSDP		csdp			PATH ${M2_HOST_PREFIX}/bin)
find_program(NORMALIZ		normaliz		PATH ${M2_HOST_PREFIX}/bin)
find_program(NAUTY	NAMES	dreadnaut nauty-complg	PATH ${M2_HOST_PREFIX}/bin)

###############################################################################

# Still processing:
#    boost??
#    gdbm??
#    glpk??
#    cddlib uses gmp
#    fplll uses mpir and mpfr
#    mpfr needs gmp (or mpir
#    	  mpfr puts pointers to gmp numbers in thread local variables, unless
# 	  specially configured, so we shouldn't tell gmp to use libgc (we used to do that)
#    mpc needs mpfr
#    ntl needs gmp (or mpir)
#    linbox needs fflas_ffpack and givaro and is provided as an option for experimentation

# The list PROGLIST is the list of programs and libraries for them that are distributed with M2.
#     Initially, we offer no option for not compiling some of them.
#set(PROGLIST 4ti2 gfan normaliz csdp nauty cddplus lrslib topcom cohomcalg)

#    4ti2 needs glpk and is used by the package FourTiTwo
#    glpk needs gmp (or mpir)
#    topcom depends on cddlib
#    gfan needs cddlib and is used by the packages gfanInterface and StatePolytope
#    polymake cannot be included in Macaulay2 because its compile/build/install procedure is flawed (FIXME)
#    normaliz needs libgmp, libgmpxx, boost and is used by the package Normaliz
#    nauty is used by the package Nauty
