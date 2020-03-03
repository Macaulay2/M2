# Try to find the mathic librairies
# See https://github.com/Macaulay2/mathic
#
# This file sets up mathic for CMake. Once done this will define
#  MATHIC_FOUND             - system has MATHIC lib
#  MATHIC_INCLUDE_DIR       - the MATHIC include directory
#  MATHIC_LIBRARIES         - Libraries needed to use MATHIC
#
# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(MATHIC_INCLUDE_DIR NAMES mathic.h )
find_library(MATHIC_LIBRARIES NAMES mathic libmathic)

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(MATHIC DEFAULT_MSG MATHIC_INCLUDE_DIR MATHIC_LIBRARIES)

mark_as_advanced(MATHIC_INCLUDE_DIR MATHIC_LIBRARIES)
