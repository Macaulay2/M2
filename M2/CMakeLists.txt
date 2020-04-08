################################## WARNING!! ##################################
## This is not yet the official build system of Macaulay2. See INSTALL.
## Why use CMake at all? See https://lwn.net/Articles/188693/ and
## https://gitlab.kitware.com/cmake/community/-/wikis/doc/cmake/Really-Cool-CMake-Features
##
## Full instructions: INSTALL-CMake.md
## Short instructions:
##  1. cd M2/BUILD/build-cmake
##  2. cmake ../..
##  3. make build-libraries build-programs rebuild_cache
##  4. make M2-engine M2-binary M2-core M2-emacs
##  5. make install install-packages check-packages
##
## IDEs such as Xcode or vscode can also configure and build using CMake.
###############################################################################

## Works with 3.14 and tested through 3.17
cmake_minimum_required(VERSION 3.14...3.17)
cmake_policy(VERSION 3.14)

## Guard against in-source builds
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  message(FATAL_ERROR "In-source builds are not allowed.")
endif()

## Use the C++14 standard
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED True)
## Use the C11 (C0X) standard
set(CMAKE_C_STANDARD 11)

###############################################################################
## Specify a few build configurations
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()
if(NOT CMAKE_INSTALL_MESSAGE)
  # Print "Installing .." but not "Up-to-date .." messages
  # TODO: this doesn't seem to be working, so we use COPY instead of INSTALL
  set(CMAKE_INSTALL_MESSAGE LAZY)
endif()
if(CMAKE_BINARY_DIR MATCHES ".*(apple|darwin).*")
  ## TODO: cross-compile for Darwin from Linux. Currently not working.
  set(TOOLCHAIN ${CMAKE_CURRENT_LIST_DIR}/cmake/darwin.cmake)
  message("## Cross-compiling from toolchain file: ${TOOLCHAIN}")
  set(CMAKE_TOOLCHAIN_FILE ${TOOLCHAIN})
endif()

## Enable support for folders in IDEs
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

###############################################################################
## Set the project name, version, and languages
## This is the source of truth for the version number
project(Macaulay2
  VERSION	1.15.0.2 # branch eigen on cmake
  DESCRIPTION	"A software system for algebraic geometry research"
  HOMEPAGE_URL	http://macaulay2.com # contact: Macaulay2@math.uiuc.edu
  LANGUAGES	C CXX)

###############################################################################
## Set path for CMake modules
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(CTest) ## CMake module for generating a 'test' target for all unit-tests
include(configure) ## CMake script for configuring various variables
include(prechecks) ## CMake script for preprocessor checks and linting
include(check-libraries) ## CMake script for checking which libraries exist and which need to be built
include(build-libraries) ## CMake script for downloading, building, and installing external libraries
# Note: we separate the two so that check-libraries.cmake can be called by Macaulay2/bin/CMakeLists.txt

###############################################################################
## Configure M2/config.h
configure_file(include/M2/config.h.cmake include/M2/config.h)

###############################################################################
## Kick off the Macaulay2 directory
add_subdirectory(Macaulay2)

###############################################################################

#include(testing) ## CMake script for testing
include(packaging) ## CMake script for making distribution packages for Macaulay2

###############################################################################
## This section is temporary hacks, will be gone soon.

file(RELATIVE_PATH M2_PATH ${CMAKE_BINARY_DIR} ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR})
file(CREATE_LINK ${M2_PATH}/M2 ${CMAKE_BINARY_DIR}/M2 SYMBOLIC)
file(CREATE_LINK ${M2_PATH}/M2-binary ${CMAKE_BINARY_DIR}/M2-binary SYMBOLIC)

# Hackish way to set topSrcdir in startup.m2
# FIXME: DELETING THIS CAUSES CONFUSING PATH ERRORS!!!
file(RELATIVE_PATH LIST_FILE ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_LIST_FILE})
get_filename_component(RELATIVE_SOURCE_DIR ${LIST_FILE} DIRECTORY)
file(GENERATE OUTPUT srcdir CONTENT "${RELATIVE_SOURCE_DIR}/\n")
