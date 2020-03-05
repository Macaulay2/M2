# Try to find the frobby librairies
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

find_path(FROBBY_INCLUDE_DIR NAMES frobby.h PATHS ${CMAKE_PREFIX_PATH}/include)
find_library(FROBBY_LIBRARIES NAMES frobby libfrobby PATHS ${CMAKE_PREFIX_PATH}/lib)

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(FROBBY DEFAULT_MSG FROBBY_INCLUDE_DIR FROBBY_LIBRARIES)

mark_as_advanced(FROBBY_INCLUDE_DIR FROBBY_LIBRARIES)
