// (c) 1994 Michael E. Stillman
#ifndef _style_hh_
#define _style_hh_

/**
 * @file style.hpp
 * @brief Engine-wide stylistic constants: `LT` / `EQ` / `GT` codes, `INTSIZE`, `GEOHEAP_SIZE`.
 *
 * Pulls together the boilerplate every engine translation unit
 * needs. The comparison-result codes `LT = -1`, `EQ = 0`, and
 * `GT = 1` are the integer return convention used by every
 * `compare` in the codebase; `INCOMPARABLE = 2` and
 * `EXCEPTION = -2` are reserved for partial-order and
 * error-signalling extensions of the same convention but are
 * not referenced from current engine code. `INTSIZE(a)` is the
 * standard `static_cast<int>((a).size())` shortcut that avoids
 * the signed/unsigned warnings produced by mixing
 * `std::vector::size()` with engine `int` indices.
 *
 * `GEOHEAP_SIZE = 15` is the engine-wide depth of every
 * size-quadrupling geometric heap (polynomial, vector,
 * resolution-vector accumulators); the matching
 * `heap_size[GEOHEAP_SIZE]` capacity table
 * (`4, 16, 64, ..., 1073741824`, each level four times the
 * previous) is defined in `engine.cpp` and declared `extern`
 * here.
 *
 * @see engine.cpp
 * @see geobucket.hpp
 */

#include "newdelete.hpp"
#include "engine-includes.hpp"

#define INTSIZE(a) static_cast<int>((a).size())

const int LT = -1;
const int EQ = 0;
const int GT = 1;
const int INCOMPARABLE = 2;
const int EXCEPTION = -2;

// Used for all of the heap types: polynomial, vector, resolution vectors.
#define GEOHEAP_SIZE 15

extern const int heap_size[GEOHEAP_SIZE];

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
