# Try to find the NTL libraries
# See https://www.shoup.net/ntl/
#
# This file sets up NTL for CMake. Once done this will define
#  NTL_FOUND             - system has NTL lib
#  NTL_INCLUDE_DIR       - the NTL include directory
#  NTL_LIBRARIES         - Libraries needed to use NTL
#
# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(NTL_INCLUDE_DIR NAMES NTL/version.h)
find_library(NTL_LIBRARIES NAMES ntl libntl)

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(NTL DEFAULT_MSG NTL_INCLUDE_DIR NTL_LIBRARIES)

mark_as_advanced(NTL_INCLUDE_DIR NTL_LIBRARIES)
