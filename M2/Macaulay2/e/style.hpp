// (c) 1994 Michael E. Stillman
#ifndef _style_hh_
#define _style_hh_

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
