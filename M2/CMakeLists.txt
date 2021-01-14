###############################################################################
## CMake is a cross-platform system for generating build environments using
## native tools such as Make and Ninja or IDEs such as Xcode and Visual Studio.
##
## See INSTALL-CMake.md for a comprehensive guide to building M2 using CMake.
## Take a look at cmake/configure.cmake for a list of options
## and read cmake/check-libraries.cmake for a list of requirements.
##
## Short instructions:
##  1. cmake -GNinja -S . -B BUILD/cmake
##  2. cmake --build BUILD/cmake --target build-libraries build-programs
##  2. cmake --build BUILD/cmake --target M2-engine M2-binary M2-core M2-emacs
##  2. cmake --build BUILD/cmake --target install-packages check-packages
##  6. cmake --install BUILD/cmake
###############################################################################

## Requires CMake version 3.15 and is tested with 3.19
cmake_minimum_required(VERSION 3.15...3.19)
cmake_policy(VERSION 3.14)

## Use the C++14 standard
set(CMAKE_CXX_STANDARD 14)
## Use the C11 (C0X) standard
set(CMAKE_C_STANDARD 11)

###############################################################################
## Set the default build configuration
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()

## Guard against in-source builds
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  message(FATAL_ERROR "In-source builds are not allowed. Delete CMakeFiles and CMakeCache.txt from the source directory.")
endif()
if(EXISTS ${CMAKE_SOURCE_DIR}/include/M2/config.h)
  message(FATAL_ERROR "In-source build artifacts detected. Run `make clean distclean` from the source directory.")
endif()

## Enable support for folders in IDEs
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

## Enable ccache or distcc acceleration with Makefile and Ninja
# Note: libraries and programs aren't using ccache this way.
find_program(CCACHE ccache)
if(CCACHE)
  # run ccache -s to get the CCache statistics
  set(CMAKE_C_COMPILER_LAUNCHER "${CCACHE}")
  set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE}")
endif()

###############################################################################
## Source of truth for project version
file(STRINGS VERSION VERSION)

## Set the project name, version, and languages
project(Macaulay2
  VERSION	${VERSION}
  DESCRIPTION	"A software system for algebraic geometry research"
  HOMEPAGE_URL	http://macaulay2.com # contact: Macaulay2@math.uiuc.edu
  LANGUAGES	C CXX)

###############################################################################
## Set path for CMake modules
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(CTest) ## CMake module for generating a 'test' target for all unit-tests
include(prechecks) ## CMake macros for preprocessor checks and linting
include(profiling) ## CMake macros for profiling Macaulay2 code
include(configure) ## CMake script for configuring various variables
include(check-libraries) ## CMake script for checking which libraries exist and which need to be built
include(build-libraries) ## CMake script for downloading, building, and installing external libraries
include(startup) ## CMake script for messy substitutions in Macaulay2/bin/startup.c

###############################################################################
## Configure M2/config.h
configure_file(include/M2/config.h.cmake include/M2/config.h)

###############################################################################
## Kick off the Macaulay2 directory
add_subdirectory(Macaulay2)

###############################################################################

include(packaging) ## CMake script for making distribution packages for Macaulay2

###############################################################################
## This section is temporary hacks! Try to reduce or move them elsewhere.

set(M2_CONTENT "#!/bin/sh\nexec ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR}/M2 \"$@\"")
execute_process(
  COMMAND ${CMAKE_COMMAND} -E echo "${M2_CONTENT}"
  OUTPUT_FILE ${CMAKE_BINARY_DIR}/M2)
execute_process(
  COMMAND chmod +x ${CMAKE_BINARY_DIR}/M2)

# Hackish way to set topSrcdir in startup.m2
# FIXME: DELETING THIS CAUSES CONFUSING PATH ERRORS!!!
file(RELATIVE_PATH LIST_FILE ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_LIST_FILE})
get_filename_component(RELATIVE_SOURCE_DIR ${LIST_FILE} DIRECTORY)
file(GENERATE OUTPUT srcdir CONTENT "${RELATIVE_SOURCE_DIR}/\n")
