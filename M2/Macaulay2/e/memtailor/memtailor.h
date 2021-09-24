/* Copyright (C) 2011 Bjarke Hammersholt Roune (www.broune.com)
   MemTailor is distributed under the Modified BSD License. See license.txt. */

// Include this file to pull in all external MemTailor files

#ifndef MEMT_ALL_GUARD
#define MEMT_ALL_GUARD

#include "memtailor/Arena.h"
#include "memtailor/ArenaVector.h"
#include "memtailor/BufferPool.h"

extern "C" {
  // Put a C function in the library so that it can be detected by the autoconf
  // macro AC_CHECK_LIB. That macro can only check for libraries that contain
  // at least one C function.
  void libmemtailorIsPresent(void); // This function does nothing.

  extern char MEMTAILOR_VERSION_STRING[];
}
#define MEMTAILOR_VERSION VERSION
#endif
