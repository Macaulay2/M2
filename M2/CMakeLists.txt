################################## WARNING!! ##################################
## This is not yet the official build system of Macaulay2. See INSTALL.
## Why use CMake at all? See https://lwn.net/Articles/188693/ and
## https://gitlab.kitware.com/cmake/community/-/wikis/doc/cmake/Really-Cool-CMake-Features
##
## Full instructions: INSTALL-CMake.md
## Short instructions:
##  1. cd M2/BUILD/build-cmake
##  2. cmake ../..
##  3. make install-libraries
##  4. make
##  5. make check-packages
##
## IDEs such as Xcode or vscode can also configure and build using CMake.
###############################################################################

## Works with 3.14 and tested through 3.16
cmake_minimum_required(VERSION 3.14...3.16)
cmake_policy(VERSION 3.16)

## Use the C++14 standard
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED True)
## Use the C11 (C0X) standard
set(CMAKE_C_STANDARD 11)

###############################################################################
## Specify a few build configurations
set(CMAKE_BUILD_TYPE		Debug) # TODO: Release
set(CMAKE_VERBOSE_MAKEFILE	OFF)   # Useful for debugging build system issues
# TODO: this doesn't seem to be working, so we use COPY instead of INSTALL
set(CMAKE_INSTALL_MESSAGE	LAZY)  # Print "Installing .." but not "Up-to-date .." messages

## TODO: cross-compile for Darwin from Linux. Currently not working.
if("${CMAKE_BINARY_DIR}" MATCHES ".*(apple|darwin).*")
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
  HOMEPAGE_URL	https://faculty.math.illinois.edu/Macaulay2/ # http://macaulay2.com/ Macaulay2@math.uiuc.edu
  LANGUAGES	C CXX)

###############################################################################
## Set path for CMake modules
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(configure) ## CMake script for configuring various variables
include(prechecks) ## CMake script for preprocessor tests and linting
include(packaging) ## CMake script for making distribution packages for Macaulay2
include(check-libraries) ## CMake script for checking which libraries exist and which need to be built
include(build-libraries) ## CMake script for downloading, building, and installing external libraries

# Note: we separate the two so that check-libraries.cmake can be called by Macaulay2/bin/CMakeLists.txt

###############################################################################
## Configure M2/config.h
configure_file(include/M2/config.h.cmake include/M2/config.h)

# TODO: is it okay to do this globally?
add_compile_options(-I${CMAKE_SOURCE_DIR}/include -I${CMAKE_BINARY_DIR}/include -I${M2_HOST_PREFIX}/include)
add_link_options(-L${M2_HOST_PREFIX}/lib)

###############################################################################
message("## Target operating system information:
     ISSUE             = ${ISSUE}
     NODENAME          = ${NODENAME}
     OS REL            = ${OS} ${REL}
     ARCH              = ${ARCH}
     OPTIMIZE          = ${OPTIMIZE}
     DEBUG             = ${DEBUG}
     GIT_DESCRIPTION   = ${GIT_DESCRIPTION}
     MP_LIBRARY        = ${MP_LIBRARY}")
# TODO
# message("## Host operating system information:")

###############################################################################
## Kick off the Macaulay2 directory
add_subdirectory(Macaulay2)

# TODO: one way to solve the library situation is to include Macaulay2 as an
# ExternalProject so it is configured after the others.

###############################################################################
## This section is temporary hacks, will be gone soon.

# This is just for Mahrud to work on CMake issues without messy compiler errors
set(SUPPRESS_COMPILER_WARNINGS ON)

# TODO: this should be done somewhere else, perhaps install, but at build time, not install time
file(CREATE_LINK ${CMAKE_INSTALL_FULL_BINDIR}/M2-binary M2 SYMBOLIC)
execute_process(
  COMMAND ${CMAKE_COMMAND} -E make_directory ${M2_CORE_DIR}
  COMMAND ${CMAKE_COMMAND} -E make_directory ${M2_EXEC_PREFIX}
  COMMAND ${CMAKE_COMMAND} -E make_directory ${M2_COMMON_PREFIX}/${CMAKE_INSTALL_INFODIR}
  COMMAND ${CMAKE_COMMAND} -E make_directory ${M2_COMMON_PREFIX}/${CMAKE_INSTALL_DOCDIR}
  )

# Hackish way to set topSrcdir in startup.m2
# FIXME: DELETING THIS CAUSES CONFUSING PATH ERRORS!!!
file(RELATIVE_PATH LIST_FILE ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_LIST_FILE})
get_filename_component(RELATIVE_SOURCE_DIR ${LIST_FILE} DIRECTORY)
file(GENERATE OUTPUT srcdir CONTENT "${RELATIVE_SOURCE_DIR}/\n")

################################################################

# FIXME: currently various pieces and libraries are not built by cmake
# BUILD/cmake-bootstrap should be a symlink to an existing build directory
set(BOOTSTRAP ${CMAKE_SOURCE_DIR}/BUILD/cmake-bootstrap)
message("## Bootstrapping from previous build in ${BOOTSTRAP}")
file(CREATE_LINK ${BOOTSTRAP}/usr-dist/${MACHINE}/${CMAKE_INSTALL_LIBEXECDIR} ${M2_EXEC_PREFIX}/${CMAKE_INSTALL_LIBEXECDIR} SYMBOLIC)
