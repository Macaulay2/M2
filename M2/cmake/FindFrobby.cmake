# Try to find the frobby libraries
# See https://github.com/Macaulay2/frobby
#
# This file sets up frobby for CMake. Once done this will define
#  FROBBY_FOUND             - system has FROBBY lib
#  FROBBY_INCLUDE_DIR       - the FROBBY include directory
#  FROBBY_LIBRARIES         - Libraries needed to use FROBBY
#
# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

if(NOT FROBBY_FOUND)
  set(FROBBY_INCLUDE_DIR NOTFOUND)
  set(FROBBY_LIBRARIES NOTFOUND)

  # search first if an FrobbyConfig.cmake is available in the system,
  # if successful this would set FROBBY_INCLUDE_DIR and the rest of
  # the script will work as usual
  find_package(Frobby ${Frobby_FIND_VERSION} NO_MODULE QUIET)

  if(NOT FROBBY_INCLUDE_DIR)
    find_path(FROBBY_INCLUDE_DIR NAMES frobby.h
      HINTS ENV FROBBYDIR
      PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
      PATH_SUFFIXES frobby
      )
  endif()

  if(NOT FROBBY_LIBRARIES)
    find_library(FROBBY_LIBRARIES NAMES frobby
      HINTS ENV FROBBYDIR
      PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
      )
  endif()

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(Frobby DEFAULT_MSG FROBBY_INCLUDE_DIR FROBBY_LIBRARIES)

  mark_as_advanced(FROBBY_INCLUDE_DIR FROBBY_LIBRARIES)
endif()
