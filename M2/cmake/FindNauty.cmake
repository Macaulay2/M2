# Try to find the Nauty executable and library
# See https://pallini.di.uniroma1.it
#
# This module supports requiring a minimum version, e.g. you can do
#   find_package(Nauty 2.8.0)
# to require version 2.8.0 to newer of NAUTY.
#
# Once done this will define
#
#  NAUTY_FOUND		- the Nauty library has correct version
#  NAUTY_EXECUTABLE	- the Nauty executable
#  NAUTY_INCLUDE_DIR	- the Nauty include directory
#  NAUTY_LIBRARIES	- the Nauty library
#  NAUTY_VERSION	- the Nauty version

# Copyright (c) 2024, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

# Set Nauty_FIND_VERSION to 1.0.0 if no minimum version is specified
if(NOT Nauty_FIND_VERSION)
  if(NOT Nauty_FIND_VERSION_MAJOR)
    set(Nauty_FIND_VERSION_MAJOR 1)
  endif()
  if(NOT Nauty_FIND_VERSION_MINOR)
    set(Nauty_FIND_VERSION_MINOR 0)
  endif()
  if(NOT Nauty_FIND_VERSION_PATCH)
    set(Nauty_FIND_VERSION_PATCH 0)
  endif()

  set(Nauty_FIND_VERSION "${Nauty_FIND_VERSION_MAJOR}.${Nauty_FIND_VERSION_MINOR}.${Nauty_FIND_VERSION_PATCH}")
endif()

macro(_NAUTY_check_version)
  # Query NAUTY_VERSION
  file(READ "${NAUTY_INCLUDE_DIR}/nauty/nauty.h" _NAUTY_H)

  string(REGEX MATCH "define[ \t]+NAUTYVERSION[ \t]+\"([0-9]+)\.([0-9]+)\.([0-9]+)"
    _NAUTY_version_match "${_NAUTY_H}")
  set(NAUTY_MAJOR_VERSION "${CMAKE_MATCH_1}")
  set(NAUTY_MINOR_VERSION "${CMAKE_MATCH_2}")
  set(NAUTY_PATCH_VERSION "${CMAKE_MATCH_3}")

  set(NAUTY_VERSION
    ${NAUTY_MAJOR_VERSION}.${NAUTY_MINOR_VERSION}.${NAUTY_PATCH_VERSION})

  # Check whether found version exceeds minimum required
  if(${NAUTY_VERSION} VERSION_LESS ${Nauty_FIND_VERSION})
    set(NAUTY_VERSION_OK FALSE)
    message(STATUS "Nauty version ${NAUTY_VERSION} found in ${NAUTY_INCLUDE_DIR}, "
                   "but at least version ${Nauty_FIND_VERSION} is required")
  else()
    set(NAUTY_VERSION_OK TRUE)
  endif()
endmacro(_NAUTY_check_version)

if(NOT NAUTY_FOUND)
  set(NAUTY_INCLUDE_DIR NOTFOUND)
  set(NAUTY_LIBRARIES NOTFOUND)

  # search first if an NautyConfig.cmake is available in the system,
  # if successful this would set NAUTY_INCLUDE_DIR and the rest of
  # the script will work as usual
  find_package(Nauty ${Nauty_FIND_VERSION} NO_MODULE QUIET)

  # find the Nauty executable
  find_program(NAUTY_EXECUTABLE NAMES dreadnaut nauty-dreadnaut)

  if(NOT NAUTY_INCLUDE_DIR)
    find_path(NAUTY_INCLUDE_DIR NAMES nauty/nauty.h
      HINTS ENV NAUTYDIR
      PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
      PATH_SUFFIXES nauty
      )
  endif()

  if(NAUTY_INCLUDE_DIR)
    _NAUTY_check_version()
  endif()

  if(NOT NAUTY_LIBRARIES)
    find_library(NAUTY_LIBRARIES NAMES nauty
      HINTS ENV NAUTYDIR
      PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
      )
  endif()

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(Nauty DEFAULT_MSG NAUTY_EXECUTABLE NAUTY_INCLUDE_DIR NAUTY_LIBRARIES NAUTY_VERSION_OK)

  mark_as_advanced(NAUTY_EXECUTABLE NAUTY_INCLUDE_DIR NAUTY_LIBRARIES)

endif()
