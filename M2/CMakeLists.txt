##### WARNING!! ##################################
# This is not yet functional
# it is designed for me (MES) to be able to use the CLion IDE
# and its refactorization tools.  If anyone would like to
# help get this functional, please contact us!
#
# In particular: one currently needs the built include files
# from our build tree (see CMakeLists.txt in Macaulay2/e)
##################################################

#set(CMAKE_VERBOSE_MAKEFILE on)
set(CMAKE_BUILD_DEBUG on)
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

# FIXME: usr-dist/common/share gives error?!
set(SHARE_DIR ${CMAKE_BINARY_DIR}/Macaulay2/share)
set(DIST_DIR  ${CMAKE_BINARY_DIR}/usr-dist)
set(CORE_DIR  ${SHARE_DIR}/Macaulay2/Core)

# FIXME: currently various pieces and libraries are not built by cmake
# BUILD/cmake-bootstrap should be a symlink to an existing build directory
set(BOOTSTRAP ${CMAKE_SOURCE_DIR}/BUILD/cmake-bootstrap)
message("## Bootstrapping from previous build in: ${BOOTSTRAP}")

# FIXME: should these stay here or go to bin/CMakeLists.txt?
## Find libraries available as CMake modules
find_package(BLAS    3.8 REQUIRED)
find_package(LAPACK  3.8 REQUIRED)
find_package(Eigen3  3.3 REQUIRED NO_MODULE)
find_package(LibXml2 2.9 REQUIRED)
find_package(Threads 2.1 REQUIRED) # pthread
find_package(Curses  6.1 REQUIRED) # ncurses
find_package(LibLZMA 5.2 REQUIRED)
find_package(PkgConfig   REQUIRED QUIET)

## Set paths for pkg-config
set(ENV{PKG_CONFIG_PATH} "${BOOTSTRAP}/usr-host/lib/pkgconfig:${BOOTSTRAP}/submodules/mathicgb/build/autotools:${BOOTSTRAP}/submodules/mathic/build/autotools:${BOOTSTRAP}/submodules/memtailor/build/autotools")

## Find libraries available via pkg-config
pkg_search_module(GIVARO    REQUIRED givaro                   IMPORTED_TARGET)
pkg_search_module(MATHICGB  REQUIRED mathicgb                 IMPORTED_TARGET)
pkg_search_module(FACTORY   REQUIRED factory singular-factory IMPORTED_TARGET)
# To fix the error, change the givaro requirement in ${BOOTSTRAP}/usr-host/lib/pkgconfig/fflas-ffpack.pc to 4.0.2
pkg_search_module(FFLAS     REQUIRED fflas-ffpack             IMPORTED_TARGET)
pkg_search_module(READLINE  REQUIRED readline                 IMPORTED_TARGET)
pkg_search_module(GC        REQUIRED bdw-gc                   IMPORTED_TARGET)

## Find all other libraries
find_library(LIBHISTORY history)
find_library(LIBFROBBY frobby)
find_library(LIBGLPK glpk)
find_library(LIBGDBM gdbm)
find_library(LIBCDD cdd)
find_library(LIBMPC mpc)
find_library(LIBDL dl)
find_library(LIBM m)
find_library(LIBC c)
find_library(LIBZ z)

## Installation directories:
option(CMAKE_INSTALL_PREFIX "installation prefix" /usr/local)

## Kick off the Macaulay2 directory
add_subdirectory(Macaulay2)

# FIXME
execute_process(
  COMMAND ${CMAKE_COMMAND} -E copy ${BOOTSTRAP}/config.status ${CMAKE_BINARY_DIR}
  COMMAND ${CMAKE_COMMAND} -E make_directory ${DIST_DIR}/common/share
  COMMAND ln -fs ${DIST_DIR}/common/share ${CMAKE_BINARY_DIR}/Macaulay2/
  COMMAND ln -fs ${CMAKE_BINARY_DIR}/Macaulay2/bin/M2-binary ${CMAKE_BINARY_DIR}/M2
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CORE_DIR}
  COMMAND ${CMAKE_COMMAND} -E make_directory ${DIST_DIR}/common/share/doc/Macaulay2
  COMMAND ${CMAKE_COMMAND} -E make_directory ${DIST_DIR}/common/share/man
  COMMAND ${CMAKE_COMMAND} -E make_directory ${DIST_DIR}/common/share/info
  COMMAND ${CMAKE_COMMAND} -E make_directory ${DIST_DIR}/common/share/Macaulay2
  COMMAND ${CMAKE_COMMAND} -E copy ${BOOTSTRAP}/usr-dist/common/share/doc/Macaulay2/COPYING-GPL-2 ${DIST_DIR}/common/share/doc/Macaulay2
  COMMAND ${CMAKE_COMMAND} -E copy ${BOOTSTRAP}/usr-dist/common/share/doc/Macaulay2/COPYING-GPL-3 ${DIST_DIR}/common/share/doc/Macaulay2
  )

# TODO: Where to move this?
#   GFTABLESDIR path to gftables directory if factory is already installed
set(GFTABLES 10201 1024 10609 11449 11881 121 12167 125 12769 128 1331 1369 14641 15625 16 16129 16384 16807 1681 169 17161 1849 18769 19321 19683 2048 2187 2197 2209 22201 22801 2401 243 24389 24649 25 256 26569 27 27889 2809 28561 289 29791 29929 3125 32 32041 32761 32768 343 3481 361 36481 3721 37249 38809 39601 4 4096 44521 4489 49 4913 49729 5041 50653 512 51529 52441 529 5329 54289 57121 58081 59049 6241 625 63001 64 6561 6859 6889 729 7921 8 81 8192 841 9 9409 961)

add_custom_target(factory-target
  COMMAND
    ${CMAKE_COMMAND} -E make_directory ${CORE_DIR}/factory/gftables
  COMMAND
    ${CMAKE_COMMAND} -E copy_if_different ${GFTABLES} ${CORE_DIR}/factory/gftables
  WORKING_DIRECTORY ${BOOTSTRAP}/libraries/gftables/build/gftables
  )

# FIXME
add_custom_target(platform-libs ALL
  COMMAND
    ln -fs ${BOOTSTRAP}/usr-dist/x86*/libexec ${CMAKE_BINARY_DIR}/Macaulay2
  COMMAND
    ln -fs ${BOOTSTRAP}/usr-dist/x86*/lib64   ${CMAKE_BINARY_DIR}/Macaulay2
  COMMAND
    ln -fs ${BOOTSTRAP}/usr-dist/x86*/lib   ${CMAKE_BINARY_DIR}/Macaulay2
  )
