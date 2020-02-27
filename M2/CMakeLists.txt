################################## WARNING!! ##################################
## This is not yet the official build system of Macaulay2. See INSTALL.
## Why use CMake at all? See https://lwn.net/Articles/188693/ and
## https://gitlab.kitware.com/cmake/community/-/wikis/doc/cmake/Really-Cool-CMake-Features
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
cmake_policy(VERSION 3.16)

## Specify the C++ standard
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_C_STANDARD 11)

###############################################################################

## Specify a few build configurations
set(CMAKE_BUILD_TYPE		Debug) # TODO: Release
set(CMAKE_VERBOSE_MAKEFILE	OFF) # Useful for debugging build system issues

set(CMAKE_INSTALL_PREFIX	${CMAKE_BINARY_DIR}/usr-dist) # must come before project()
#set(CMAKE_STAGING_PREFIX	${CMAKE_BINARY_DIR}/usr-dist)

## Enable support for folders in IDEs
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

###############################################################################

## Set the project name, version, and languages
## This is the source of truth for the version number
project(Macaulay2
  VERSION	1.15.0.2 # branch eigen on cmake
  DESCRIPTION	"A software system for algebraic geometry research"
  HOMEPAGE_URL	https://faculty.math.illinois.edu/Macaulay2/ # http://macaulay2.com/ Macaulay2@math.uiuc.edu
  LANGUAGES	C CXX)

###############################################################################

## Set path for CMake modules
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(configure)	## CMake script for configuring various variables
include(prechecks)	## CMake script for preprocessor tests and linting
include(distributions)	## CMake script for making distribution packages

## Configure M2/config.h
configure_file(include/M2/config.h.cmake include/M2/config.h)
# TODO: should we just do this here for everything?
#include_directories(include ${CMAKE_BINARY_DIR}/include)

###############################################################################

## Kick off the Macaulay2 directory
add_subdirectory(Macaulay2)

## Find or download and build external libraries and executables
add_subdirectory(libraries)

###############################################################################
## This section is temporary hacks, will be gone soon.

set(SUPPRESS_COMPILER_WARNINGS	ON) # Mahrud: this is just for me to work on CMake issues

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
