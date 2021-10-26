# Try to find the MEMTAILOR libraries
# See https://github.com/Macaulay2/memtailor
#
# This file sets up GLPK for CMake. Once done this will define
#  MEMTAILOR_FOUND             - system has MEMTAILOR lib
#  MEMTAILOR_INCLUDE_DIR       - the MEMTAILOR include directory
#  MEMTAILOR_LIBRARIES         - Libraries needed to use MEMTAILOR
#
# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(MEMTAILOR_INCLUDE_DIR NAMES memtailor.h
  PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
  PATH_SUFFIXES memtailor
  )
find_library(MEMTAILOR_LIBRARIES NAMES memtailor
  PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
  )

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Memtailor DEFAULT_MSG MEMTAILOR_INCLUDE_DIR MEMTAILOR_LIBRARIES)

mark_as_advanced(MEMTAILOR_INCLUDE_DIR MEMTAILOR_LIBRARIES)
