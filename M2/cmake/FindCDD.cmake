# Try to find the CDD libraries
# See https://www.inf.ethz.ch/personal/fukudak/cdd_home/
#
# This file sets up CDD for CMake. Once done this will define
#  CDD_FOUND             - system has CDD lib
#  CDD_INCLUDE_DIR       - the CDD include directory
#  CDD_LIBRARIES         - Libraries needed to use CDD
#
# Copyright (c) 2006, Laurent Montel, <montel@kde.org>
# Copyright (c) 2018, Thomas Baumgart <tbaumgart@kde.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(CDD_INCLUDE_DIR NAMES cdd/cdd.h)
find_library(CDD_LIBRARIES NAMES cdd libcdd)

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(CDD DEFAULT_MSG CDD_INCLUDE_DIR CDD_LIBRARIES)

mark_as_advanced(CDD_INCLUDE_DIR CDD_LIBRARIES)
