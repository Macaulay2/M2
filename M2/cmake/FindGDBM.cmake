# Try to find the GDBM library
# https://www.gnu.org.ua/software/gdbm/
#
# This file sets up GDBM for CMake. Once done this will define
#  GDBM_FOUND             - system has the GDBM library
#  GDBM_INCLUDE_DIR       - the GDBM include directory
#  GDBM_LIBRARY           - the GDBM library
#
# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(GDBM_INCLUDE_DIR NAMES gdbm.h
  HINTS ${HOMEBREW_PREFIX}/opt/gdbm/include
  PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
  )

find_library(GDBM_LIBRARY NAMES gdbm
  HINTS ${HOMEBREW_PREFIX}/opt/gdbm/lib
  PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
  )

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(GDBM DEFAULT_MSG GDBM_INCLUDE_DIR GDBM_LIBRARY)

mark_as_advanced(GDBM_INCLUDE_DIR GDBM_LIBRARY)
