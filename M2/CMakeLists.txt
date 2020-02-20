################################## WARNING!! ##################################
## This is not yet the official build system of Macaulay2. See INSTALL.
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

#set(CMAKE_VERBOSE_MAKEFILE on)
set(CMAKE_BUILD_TYPE Debug)
set(SUPPRESS_COMPILER_WARNINGS on)

cmake_minimum_required(VERSION 3.14)

## specify the C++ standard
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED True)

## set the project name, version, and languages
project(Macaulay2
  VERSION 1.15.0.2 # branch eigen on cmake
  DESCRIPTION "A software system for algebraic geometry research"
  HOMEPAGE_URL https://faculty.math.illinois.edu/Macaulay2/
  LANGUAGES C CXX
  )

## Static checks
set(CMAKE_LINK_WHAT_YOU_USE TRUE)
#set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE iwyu --transitive_includes_only)
#set(CMAKE_CXX_CLANG_TIDY clang-tidy -checks=-*,readability-*)
#set(CMAKE_CXX_CLANG_TIDY clang-tidy -checks=-*,cppcoreguidelines-*,clang-analyzer-*,modernize-*,performance-*,readability-*)
#set(CMAKE_CXX_CPPCHECK cppcheck --std=c++11)
#set(CMAKE_CXX_CPPLINT  cpplint  --linelength=79)

## Set version info
#configure_file (
#  "${PROJECT_SOURCE_DIR}/include/My/Version.h.in"
#  "${PROJECT_BINARY_DIR}/include/My/Version.h"
#  )
# You should include the binary include directory as well when building your project.

## Include extra CMake files that are used for testing and linting.
#include(${CMAKE_SOURCE_DIR}/cmake/unit_test.cmake)

# FIXME: usr-dist/common/share gives error?!
set(SHARE_DIR ${CMAKE_BINARY_DIR}/Macaulay2/share)
set(DIST_DIR  ${CMAKE_BINARY_DIR}/usr-dist)
set(CORE_DIR  ${SHARE_DIR}/Macaulay2/Core)

# FIXME: currently various pieces and libraries are not built by cmake
# BUILD/cmake-bootstrap should be a symlink to an existing build directory
set(BOOTSTRAP ${CMAKE_SOURCE_DIR}/BUILD/cmake-bootstrap)
set(DIST_TYPE ${CMAKE_SYSTEM_PROCESSOR}-Linux-Fedora-31)
message("## Bootstrapping from previous build in ${BOOTSTRAP} for ${DIST_TYPE}")

## Set path for CMake modules
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

# FIXME: should these stay here or go to bin/CMakeLists.txt?
# TODO: which ones need to be patched and built?
## Find libraries available as CMake modules
find_package(BLAS    3.8 REQUIRED)
find_package(LAPACK  3.8 REQUIRED)
find_package(Eigen3  3.3 REQUIRED NO_MODULE)
find_package(LibXml2 2.9 REQUIRED)
find_package(Threads 2.1 REQUIRED) # pthread
find_package(Curses  6.1 REQUIRED) # ncurses
find_package(LibLZMA 5.2 REQUIRED)
find_package(MPIR    3.0 REQUIRED) # cmake/FindMPIR.cmake
find_package(GLPK   4.59 REQUIRED) # cmake/FindGLPK.cmake
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
pkg_search_module(FFLAS     REQUIRED fflas-ffpack             IMPORTED_TARGET)
pkg_search_module(READLINE  REQUIRED readline                 IMPORTED_TARGET)
pkg_search_module(GC        REQUIRED bdw-gc                   IMPORTED_TARGET)

## Find all other libraries
find_library(LIBHISTORY history)
find_library(LIBFROBBY frobby)
find_library(LIBGDBM gdbm)
#find_library(LIBCDD cdd)
find_library(LIBMPC mpc)
find_library(LIBM m) # math.h
find_library(LIBC c)
find_library(LIBZ z)

## Installation directories:
# set(CMAKE_INSTALL_PREFIX /usr) # this doesn't work, where should it be set?

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
  COMMAND ln -fs ${BOOTSTRAP}/usr-dist/${DIST_TYPE}/lib      ${CMAKE_BINARY_DIR}/Macaulay2/
  COMMAND ln -fs ${BOOTSTRAP}/usr-dist/${DIST_TYPE}/lib64    ${CMAKE_BINARY_DIR}/Macaulay2/
  COMMAND ln -fs ${BOOTSTRAP}/usr-dist/${DIST_TYPE}/libexec  ${CMAKE_BINARY_DIR}/Macaulay2/
  COMMAND ln -fs ${CMAKE_BINARY_DIR}/Macaulay2/bin/M2-binary ${CMAKE_BINARY_DIR}/M2
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CORE_DIR}
  )
