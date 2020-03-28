# Try to find the gc libraries
# See https://github.com/ivmai/bdwgc
#
# This file sets up gc for CMake. Once done this will define
#  GC_FOUND             - system has GC lib
#  GC_INCLUDE_DIR       - the GC include directory
#  GC_LIBRARIES         - Libraries needed to use GC
#
# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

# Set GC_FIND_VERSION to 1.0.0 if no minimum version is specified
if(NOT GC_FIND_VERSION)
  if(NOT GC_FIND_VERSION_MAJOR)
    set(GC_FIND_VERSION_MAJOR 1)
  endif()
  if(NOT GC_FIND_VERSION_MINOR)
    set(GC_FIND_VERSION_MINOR 0)
  endif()
  if(NOT GC_FIND_VERSION_MICRO)
    set(GC_FIND_VERSION_MICRO 0)
  endif()

  set(GC_FIND_VERSION "${GC_FIND_VERSION_MAJOR}.${GC_FIND_VERSION_MINOR}.${GC_FIND_VERSION_MICRO}")
endif()

macro(_gc_check_version)
  # Query GC_VERSION
  file(READ "${GC_INCLUDE_DIR}/gc/gc_version.h" _gc_version_header)

  string(REGEX MATCH "define[ \t]+GC_TMP_VERSION_MAJOR[ \t]+([0-9]+)"
    _gc_major_version_match "${_gc_version_header}")
  set(GC_MAJOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+GC_TMP_VERSION_MINOR[ \t]+([0-9]+)"
    _gc_minor_version_match "${_gc_version_header}")
  set(GC_MINOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+GC_TMP_VERSION_MICRO[ \t]+([0-9]+)"
    _gc_micro_version_match "${_gc_version_header}")
  set(GC_MICRO_VERSION "${CMAKE_MATCH_1}")

  set(GC_VERSION
    ${GC_MAJOR_VERSION}.${GC_MINOR_VERSION}.${GC_MICRO_VERSION})

  # Check whether found version exceeds minimum required
  if(${GC_VERSION} VERSION_LESS ${GC_FIND_VERSION})
    set(GC_VERSION_OK FALSE)
    message(STATUS "GC version ${GC_VERSION} found in ${GC_INCLUDE_DIR}, "
                   "but at least version ${GC_FIND_VERSION} is required")
  else()
    set(GC_VERSION_OK TRUE)
  endif()
endmacro(_gc_check_version)

if (GC_INCLUDE_DIR AND GC_LIBRARY)

  # Exists in cache already
  _gc_check_version()
  set(GC_FOUND ${GC_VERSION_OK})

else()

  # search first if an GCConfig.cmake is available in the system,
  # if successful this would set GC_INCLUDE_DIR and the rest of
  # the script will work as usual
  find_package(GC ${GC_FIND_VERSION} NO_MODULE QUIET)

  if(NOT GC_INCLUDE_DIR)
    find_path(GC_INCLUDE_DIR NAMES gc/gc.h
      HINTS ENV GCDIR
      PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
      PATH_SUFFIXES gc bdwgc
      )
  endif()

  if(GC_INCLUDE_DIR)
    _gc_check_version()
  endif()

  if(NOT GC_LIBRARY)
    find_library(GC_LIBRARY NAMES gc gccpp gc gccpp
      HINTS ENV GCDIR
      PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
      )
  endif()

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(GC DEFAULT_MSG GC_INCLUDE_DIR GC_LIBRARIES GC_VERSION_OK)

  mark_as_advanced(GC_INCLUDE_DIR GC_LIBRARIES)

endif()
