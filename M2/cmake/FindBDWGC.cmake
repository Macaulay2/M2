# Try to find the bdwgc libraries
# See https://github.com/ivmai/bdwgc
#
# This file sets up gc for CMake. Once done this will define
#  BDWGC_FOUND             - system has BDWGC lib
#  BDWGC_INCLUDE_DIR       - the BDWGC include directory
#  BDWGC_LIBRARIES         - Libraries needed to use BDWGC
#
# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

# Set BDWGC_FIND_VERSION to 1.0.0 if no minimum version is specified
if(NOT BDWGC_FIND_VERSION)
  if(NOT BDWGC_FIND_VERSION_MAJOR)
    set(BDWGC_FIND_VERSION_MAJOR 1)
  endif()
  if(NOT BDWGC_FIND_VERSION_MINOR)
    set(BDWGC_FIND_VERSION_MINOR 0)
  endif()
  if(NOT BDWGC_FIND_VERSION_MICRO)
    set(BDWGC_FIND_VERSION_MICRO 0)
  endif()

  set(BDWGC_FIND_VERSION "${BDWGC_FIND_VERSION_MAJOR}.${BDWGC_FIND_VERSION_MINOR}.${BDWGC_FIND_VERSION_MICRO}")
endif()

macro(_gc_check_version)
  # Query BDWGC_VERSION
  file(READ "${BDWGC_INCLUDE_DIR}/gc/gc_version.h" _gc_version_header)

  string(REGEX MATCH "define[ \t]+GC_TMP_VERSION_MAJOR[ \t]+([0-9]+)"
    _gc_major_version_match "${_gc_version_header}")
  set(BDWGC_MAJOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+GC_TMP_VERSION_MINOR[ \t]+([0-9]+)"
    _gc_minor_version_match "${_gc_version_header}")
  set(BDWGC_MINOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+GC_TMP_VERSION_MICRO[ \t]+([0-9]+)"
    _gc_micro_version_match "${_gc_version_header}")
  set(BDWGC_MICRO_VERSION "${CMAKE_MATCH_1}")

  set(BDWGC_VERSION
    ${BDWGC_MAJOR_VERSION}.${BDWGC_MINOR_VERSION}.${BDWGC_MICRO_VERSION})

  # Check whether found version exceeds minimum required
  if(${BDWGC_VERSION} VERSION_LESS ${BDWGC_FIND_VERSION})
    set(BDWGC_VERSION_OK FALSE)
    message(STATUS "BDWGC version ${BDWGC_VERSION} found in ${BDWGC_INCLUDE_DIR}, "
                   "but at least version ${BDWGC_FIND_VERSION} is required")
  else()
    set(BDWGC_VERSION_OK TRUE)
  endif()
endmacro(_gc_check_version)

if(NOT BDWGC_VERSION_OK)
  set(BDWGC_INCLUDE_DIR NOTFOUND)
  set(BDWGC_LIBRARIES NOTFOUND)

  # search first if an BDWGCConfig.cmake is available in the system,
  # if successful this would set BDWGC_INCLUDE_DIR and the rest of
  # the script will work as usual
  find_package(BDWGC ${BDWGC_FIND_VERSION} NO_MODULE QUIET)

  if(NOT BDWGC_INCLUDE_DIR)
    find_path(BDWGC_INCLUDE_DIR NAMES gc/gc.h
      HINTS ENV BDWGCDIR
      PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
      PATH_SUFFIXES gc bdwgc
      )
  endif()

  if(BDWGC_INCLUDE_DIR)
    _gc_check_version()
  endif()

  if(NOT BDWGC_LIBRARIES)
    find_library(BDWGC_LIBRARIES NAMES gc gccpp gc gccpp
      HINTS ENV BDWGCDIR
      PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
      )
  endif()

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(BDWGC DEFAULT_MSG BDWGC_INCLUDE_DIR BDWGC_LIBRARIES BDWGC_VERSION_OK)

  mark_as_advanced(BDWGC_INCLUDE_DIR BDWGC_LIBRARIES)

endif()
