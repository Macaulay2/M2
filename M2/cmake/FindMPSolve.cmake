# Try to find the MPSolve libraries
# See https://numpi.dm.unipi.it/software/mpsolve
#
# This module supports requiring a minimum version, e.g. you can do
#   find_package(MPSolve 3.2.1)
# to require version 3.2.0 to newer of MPSOLVE.
#
# Once done this will define
#
#  MPSOLVE_FOUND	- system has the MPSOLVE library with correct version
#  MPSOLVE_INCLUDE_DIR	- the MPSOLVE include directory
#  MPSOLVE_LIBRARIES	- the MPSOLVE library
#  MPSOLVE_VERSION	- MPSOLVE version

# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

# Set MPSolve_FIND_VERSION to 3.0.0 if no minimum version is specified
if(NOT MPSolve_FIND_VERSION)
  if(NOT MPSolve_FIND_VERSION_MAJOR)
    set(MPSolve_FIND_VERSION_MAJOR 3)
  endif()
  if(NOT MPSolve_FIND_VERSION_MINOR)
    set(MPSolve_FIND_VERSION_MINOR 0)
  endif()
  if(NOT MPSolve_FIND_VERSION_PATCH)
    set(MPSolve_FIND_VERSION_PATCH 0)
  endif()

  set(MPSolve_FIND_VERSION "${MPSolve_FIND_VERSION_MAJOR}.${MPSolve_FIND_VERSION_MINOR}.${MPSolve_FIND_VERSION_PATCH}")
endif()

macro(_mpsolve_check_version)
  # Query MPSolve_VERSION
  file(READ "${MPSOLVE_INCLUDE_DIR}/mps/version.h" _mpsolve_version_header)

  string(REGEX MATCH "define[ \t]+MPS_MAJOR_VERSION[ \t]+([0-9]+)"
    _mpsolve_major_version_match "${_mpsolve_version_header}")
  set(MPSOLVE_MAJOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+MPS_MINOR_VERSION[ \t]+([0-9]+)"
    _mpsolve_minor_version_match "${_mpsolve_version_header}")
  set(MPSOLVE_MINOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+MPS_PATCH_VERSION[ \t]+([0-9]+)"
    _mpsolve_patch_version_match "${_mpsolve_version_header}")
  set(MPSOLVE_PATCH_VERSION "${CMAKE_MATCH_1}")

  set(MPSOLVE_VERSION
    ${MPSOLVE_MAJOR_VERSION}.${MPSOLVE_MINOR_VERSION}.${MPSOLVE_PATCH_VERSION})

  # Check whether found version exceeds minimum required
  if(${MPSOLVE_VERSION} VERSION_LESS ${MPSolve_FIND_VERSION})
    set(MPSOLVE_VERSION_OK FALSE)
    message(STATUS "MPSolve version ${MPSOLVE_VERSION} found in ${MPSOLVE_INCLUDE_DIR}, "
                   "but at least version ${MPSolve_FIND_VERSION} is required")
  else()
    set(MPSOLVE_VERSION_OK TRUE)
  endif()
endmacro(_mpsolve_check_version)

if(NOT MPSOLVE_FOUND)
  set(MPSOLVE_INCLUDE_DIR NOTFOUND)
  set(MPSOLVE_LIBRARIES NOTFOUND)

  # search first if an MPSolveConfig.cmake is available in the system,
  # if successful this would set MPSOLVE_INCLUDE_DIR and the rest of
  # the script will work as usual
  find_package(MPSolve ${MPSolve_FIND_VERSION} NO_MODULE QUIET)

  if(NOT MPSOLVE_INCLUDE_DIR)
    find_path(MPSOLVE_INCLUDE_DIR NAMES mps/mps.h
      HINTS ENV MPSDIR
      PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
      PATH_SUFFIXES mps
      )
  endif()

  if(MPSOLVE_INCLUDE_DIR)
    _mpsolve_check_version()
  endif()

  if(NOT MPSOLVE_LIBRARIES)
    find_library(MPSOLVE_LIBRARIES NAMES mps
      HINTS ENV MPSDIR
      PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
      )
  endif()

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(MPSolve DEFAULT_MSG MPSOLVE_INCLUDE_DIR MPSOLVE_LIBRARIES MPSOLVE_VERSION_OK)

  mark_as_advanced(MPSOLVE_INCLUDE_DIR MPSOLVE_LIBRARIES)

endif()
