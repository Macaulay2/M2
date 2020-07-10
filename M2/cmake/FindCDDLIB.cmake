# Try to find the cddlib library
# See https://www.inf.ethz.ch/personal/fukudak/cdd_home/
#
# This file sets up cddlib for CMake. Once done this will define
#  CDDLIB_FOUND             - system has the CDDLIB library
#  CDDLIB_ROOT              - the CDDLIB install prefix
#  CDDLIB_INCLUDE_DIR       - the CDDLIB include directory
#  CDDLIB_LIBRARIES         - Libraries needed to use CDDLIB
#
# Copyright (c) 2006, Laurent Montel, <montel@kde.org>
# Copyright (c) 2018, Thomas Baumgart <tbaumgart@kde.org>
# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(CDDLIB_INCLUDE_DIR NAMES cdd.h
  PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
  PATH_SUFFIXES cdd cddlib
  )

find_library(CDDLIB_LIBRARIES NAMES cdd cddgmp
  PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
  )

if(CDDLIB_LIBRARIES)
  get_filename_component(CDDLIB_LIBRARY_DIR "${CDDLIB_LIBRARIES}" DIRECTORY)
endif()

string(REGEX REPLACE "/include(/${CMAKE_LIBRARY_ARCHITECTURE}$)?" "" CDDLIB_ROOT "${CDDLIB_INCLUDE_DIR}")

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(CDDLIB DEFAULT_MSG CDDLIB_ROOT CDDLIB_INCLUDE_DIR CDDLIB_LIBRARY_DIR CDDLIB_LIBRARIES)

mark_as_advanced(CDDLIB_ROOT CDDLIB_INCLUDE_DIR CDDLIB_LIBRARY_DIR CDDLIB_LIBRARIES)
