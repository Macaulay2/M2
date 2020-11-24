# Try to find the Flint libraries
# See http://www.flintlib.org/
#
# This module supports requiring a minimum version, e.g. you can do
#   find_package(Flint 2.5.3)
# to require version 2.5.3 to newer of FLINT.
#
# Once done this will define
#
#  FLINT_FOUND		- system has the FLINT library with correct version
#  FLINT_ROOT		- the FLINT install prefix
#  FLINT_INCLUDE_DIR	- the FLINT include directory
#  FLINT_LIBRARIES	- the FLINT library
#  FLINT_VERSION	- FLINT version

# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

# Set Flint_FIND_VERSION to 2.5.2 if no minimum version is specified
if(NOT Flint_FIND_VERSION)
  if(NOT Flint_FIND_VERSION_MAJOR)
    set(Flint_FIND_VERSION_MAJOR 2)
  endif()
  if(NOT Flint_FIND_VERSION_MINOR)
    set(Flint_FIND_VERSION_MINOR 5)
  endif()
  if(NOT Flint_FIND_VERSION_PATCH)
    set(Flint_FIND_VERSION_PATCH 2)
  endif()

  set(Flint_FIND_VERSION "${Flint_FIND_VERSION_MAJOR}.${Flint_FIND_VERSION_MINOR}.${Flint_FIND_VERSION_PATCH}")
endif()

macro(_flint_check_version)
  # Query Flint_VERSION
  file(READ "${FLINT_INCLUDE_DIR}/flint/flint.h" _flint_version_header)

  string(REGEX MATCH "define[ \t]+__FLINT_VERSION[ \t]+([0-9]+)"
    _flint_major_version_match "${_flint_version_header}")
  set(FLINT_MAJOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+__FLINT_VERSION_MINOR[ \t]+([0-9]+)"
    _flint_minor_version_match "${_flint_version_header}")
  set(FLINT_MINOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+__FLINT_VERSION_PATCHLEVEL[ \t]+([0-9]+)"
    _flint_patchlevel_version_match "${_flint_version_header}")
  set(FLINT_PATCHLEVEL_VERSION "${CMAKE_MATCH_1}")

  set(FLINT_VERSION
    ${FLINT_MAJOR_VERSION}.${FLINT_MINOR_VERSION}.${FLINT_PATCHLEVEL_VERSION})

  # Check whether found version exceeds minimum required
  if(${FLINT_VERSION} VERSION_LESS ${Flint_FIND_VERSION})
    set(FLINT_VERSION_OK FALSE)
    message(STATUS "Flint version ${FLINT_VERSION} found in ${FLINT_INCLUDE_DIR}, "
                   "but at least version ${Flint_FIND_VERSION} is required")
  else()
    set(FLINT_VERSION_OK TRUE)
  endif()
endmacro(_flint_check_version)

if(NOT FLINT_FOUND)
  set(FLINT_INCLUDE_DIR NOTFOUND)
  set(FLINT_LIBRARIES NOTFOUND)

  # search first if an FlintConfig.cmake is available in the system,
  # if successful this would set FLINT_INCLUDE_DIR and the rest of
  # the script will work as usual
  find_package(Flint ${Flint_FIND_VERSION} NO_MODULE QUIET)

  if(NOT FLINT_INCLUDE_DIR)
    find_path(FLINT_INCLUDE_DIR NAMES flint/flint.h
      HINTS ENV FLINTDIR
      PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
      PATH_SUFFIXES flint flint2
      )
  endif()

  if(FLINT_INCLUDE_DIR)
    _flint_check_version()
  endif()

  if(NOT FLINT_LIBRARIES)
    find_library(FLINT_LIBRARIES NAMES flint
      HINTS ENV FLINTDIR
      PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
      )
  endif()

  string(REGEX REPLACE "/include.*" "" FLINT_ROOT "${FLINT_INCLUDE_DIR}")

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(Flint DEFAULT_MSG FLINT_ROOT FLINT_INCLUDE_DIR FLINT_LIBRARIES FLINT_VERSION_OK)

  mark_as_advanced(FLINT_ROOT FLINT_INCLUDE_DIR FLINT_LIBRARIES)

endif()
