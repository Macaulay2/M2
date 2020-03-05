# Try to find the flint librairies
# See http://www.flintlib.org/
#
# This file sets up flint for CMake. Once done this will define
#  FLINT_FOUND             - system has FLINT lib
#  FLINT_INCLUDE_DIR       - the FLINT include directory
#  FLINT_LIBRARIES         - Libraries needed to use FLINT
#
# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(FLINT_INCLUDE_DIR NAMES flint/flint.h PATHS ${CMAKE_PREFIX_PATH}/include)
find_library(FLINT_LIBRARIES NAMES flint libflint PATHS ${CMAKE_PREFIX_PATH}/lib)

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(FLINT DEFAULT_MSG FLINT_INCLUDE_DIR FLINT_LIBRARIES)

mark_as_advanced(FLINT_INCLUDE_DIR FLINT_LIBRARIES)
