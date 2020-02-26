################################## WARNING!! ##################################
## This is not yet the official build system of Macaulay2. See INSTALL.
## Why use CMake at all? See https://lwn.net/Articles/188693/.
##
## Instructions:
##  1. Build M2 using configure and make in M2/BUILD/cmake-bootstrap directory
##     (or make a symlink to an existing build directory)
##  2. cd M2/BUILD/build-cmake
##  3. cmake ../..
##  4. make
##
## You can also skip steps 2-4 and simply use an IDE such as Xcode or vscode.
## Any help in removing the bootstrap dependence is welcome!
###############################################################################

## Works with 3.14 and tested through 3.16
cmake_minimum_required(VERSION 3.14...3.16)

## Specify the C++ standard
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED True)

#set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/usr-dist)
set(CMAKE_STAGING_PREFIX ${CMAKE_BINARY_DIR}/usr-dist)

## Set the project name, version, and languages
## This is the source of truth for the Macaulay2 version number
project(Macaulay2
  VERSION	1.15.0.2 # branch eigen on cmake
  DESCRIPTION	"A software system for algebraic geometry research"
  HOMEPAGE_URL	https://faculty.math.illinois.edu/Macaulay2/ # http://macaulay2.com/ Macaulay2@math.uiuc.edu 
  LANGUAGES	C CXX
  )

## Enable support for folders in IDEs
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(CMAKE_BUILD_TYPE		Debug) # TODO: Release
set(CMAKE_VERBOSE_MAKEFILE	OFF) # Useful for debugging build system issues
set(SUPPRESS_COMPILER_WARNINGS	ON) # Mahrud: this is just for me to work on CMake issues

# FIXME: usr-dist/common/share gives error?!
set(SHARE_DIR ${CMAKE_BINARY_DIR}/Macaulay2/share)
set(DIST_DIR  ${CMAKE_BINARY_DIR}/usr-dist)
set(CORE_DIR  ${SHARE_DIR}/Macaulay2/Core)

# FIXME: currently various pieces and libraries are not built by cmake
# BUILD/cmake-bootstrap should be a symlink to an existing build directory
set(BOOTSTRAP ${CMAKE_SOURCE_DIR}/BUILD/cmake-bootstrap)
message("## Bootstrapping from previous build in ${BOOTSTRAP}")

## Set path for CMake modules
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(configure)     ## CMake script for configuring various variables
include(prechecks)     ## CMake script for preprocessor tests and linting
include(distributions) ## CMake script for making distribution packages

## Configure M2/config.h
# TODO: review other config files in the config folder
#configure_file(include/M2/config.h.cmake include/M2/config.h)
#include_directories(${PROJECT_BINARY_DIR}/include)

# FIXME: should these stay here or go to bin/CMakeLists.txt?
# TODO: which ones need to be patched and built?
## Find libraries available as CMake modules
find_package(BLAS    3.8 REQUIRED)
find_package(LAPACK  3.8 REQUIRED)
find_package(Eigen3  3.3 REQUIRED NO_MODULE)
find_package(LibXml2 2.9 REQUIRED) # need xmlNewNode
find_package(Threads 2.1 REQUIRED) # pthread
find_package(Curses  6.1 REQUIRED) # ncurses
find_package(LibLZMA 5.2 REQUIRED) # need lzma_end
#find_package(MPIR    3.0 REQUIRED) # cmake/FindMPIR.cmake
find_package(GLPK   4.59 REQUIRED) # cmake/FindGLPK.cmake
# OpenMP is required for building the library csdp and good for building the library normaliz
find_package(OpenMP      REQUIRED) # TODO: use OPENMP_LIBS/CXXFLAGS for csdb
find_package(PkgConfig   REQUIRED QUIET)

## Set paths for pkg-config
set(ENV{PKG_CONFIG_PATH} "${BOOTSTRAP}/usr-host/lib/pkgconfig:${BOOTSTRAP}/submodules/mathicgb/build/autotools:${BOOTSTRAP}/submodules/mathic/build/autotools:${BOOTSTRAP}/submodules/memtailor/build/autotools")

## Find libraries available via pkg-config
## tip: use cmake -LA to list resolved variables
pkg_search_module(GIVARO    REQUIRED givaro                   IMPORTED_TARGET)
pkg_search_module(MATHICGB  REQUIRED mathicgb                 IMPORTED_TARGET)
# TODO: investigate error when factory-devel package is installed:
# sample Factory finite field addition table file missing, needed for factorization:
# /home/mahrud/Projects/M2/M2/M2/BUILD/mahrud/build/usr-dist//usr/share/factory/
pkg_search_module(FACTORY   REQUIRED factory singular-factory IMPORTED_TARGET)
# To fix the error, change the givaro requirement in ${BOOTSTRAP}/usr-host/lib/pkgconfig/fflas-ffpack.pc to 4.0.2
#pkg_search_module(FFLAS     REQUIRED fflas-ffpack             IMPORTED_TARGET)
#pkg_search_module(READLINE  REQUIRED readline                 IMPORTED_TARGET)
# TODO: should we export GC_LARGE_ALLOC_WARN_INTERVAL=1?
pkg_search_module(GC        REQUIRED bdw-gc                   IMPORTED_TARGET)

## Find all other libraries
find_library(LIBHISTORY history)
#find_library(LIBFROBBY frobby)
find_library(LIBGDBM gdbm)
#find_library(LIBCDD cdd)
find_library(LIBMPC mpc)
find_library(LIBM m) # need pow from math.h
find_library(LIBC c)
find_library(LIBZ z) # need gzopen

# TODO:
#test:
#	: "PKG_CONFIG_PATH    = $(PKG_CONFIG_PATH)"
#	: "M2_PKG_CONFIG_PATH = $(M2_PKG_CONFIG_PATH)"
#	:
#	: "GIVARO_CXXFLAGS       = $(GIVARO_CXXFLAGS)"
#	: "FFLAS_FFPACK_CXXFLAGS = $(FFLAS_FFPACK_CXXFLAGS)"
#	: "M2_CXXFLAGS           = $(M2_CXXFLAGS)"
#	: "M2_BOTH               = $(M2_BOTH)"
#	:
#	: "GIVARO_LIBS           = $(GIVARO_LIBS)"
#	: "FFLAS_FFPACK_LIBS     = $(FFLAS_FFPACK_LIBS)"
#	: "M2_LIBRARIES          = $(M2_LIBRARIES)"

## Kick off the Macaulay2 directory
add_subdirectory(Macaulay2)

## Find or download and build external libraries and executables
add_subdirectory(libraries)

# FIXME
execute_process(
  COMMAND ${CMAKE_COMMAND} -E copy ${BOOTSTRAP}/config.status ${CMAKE_BINARY_DIR}
  COMMAND ln -fs                             ${DIST_DIR}/common/share ${SHARE_DIR}
  COMMAND ${CMAKE_COMMAND} -E make_directory ${DIST_DIR}/common/share
  COMMAND ${CMAKE_COMMAND} -E make_directory ${DIST_DIR}/common/share/man
  COMMAND ${CMAKE_COMMAND} -E make_directory ${DIST_DIR}/common/share/info
  COMMAND ${CMAKE_COMMAND} -E make_directory ${DIST_DIR}/common/share/Macaulay2
  COMMAND ${CMAKE_COMMAND} -E make_directory ${DIST_DIR}/common/share/doc/Macaulay2
  )

# FIXME
add_custom_target(copy_licenses ALL
  COMMAND ${CMAKE_COMMAND} -E copy ${BOOTSTRAP}/usr-dist/common/share/doc/Macaulay2/COPYING-GPL-2 ${DIST_DIR}/common/share/doc/Macaulay2/
  COMMAND ${CMAKE_COMMAND} -E copy ${BOOTSTRAP}/usr-dist/common/share/doc/Macaulay2/COPYING-GPL-3 ${DIST_DIR}/common/share/doc/Macaulay2/
  DEPENDS ${DIST_DIR}/common/share/doc/Macaulay2/
  )

# FIXME
add_custom_target(generate_symlinks ALL
  COMMAND ln -fs ${BOOTSTRAP}/usr-dist/${MACHINE}/lib      ${CMAKE_BINARY_DIR}/Macaulay2/
  COMMAND ln -fs ${BOOTSTRAP}/usr-dist/${MACHINE}/lib64    ${CMAKE_BINARY_DIR}/Macaulay2/
  COMMAND ln -fs ${BOOTSTRAP}/usr-dist/${MACHINE}/libexec  ${CMAKE_BINARY_DIR}/Macaulay2/
  COMMAND ln -fs ${CMAKE_BINARY_DIR}/Macaulay2/bin/M2-binary ${CMAKE_BINARY_DIR}/M2
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CORE_DIR}
  )
