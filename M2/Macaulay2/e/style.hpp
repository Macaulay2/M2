// (c) 1994 Michael E. Stillman
#ifndef _style_hh_
#define _style_hh_

#include "engine-includes.hpp"

#if 0
#include <cmath>    // to get fabs(), gcc 3.0
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <string.h>
#include <algorithm>
#include <stddef.h>
#include <gmp.h>
#include <mpfr.h>
#endif

#define VECTOR(T) std::vector< T, gc_allocator< T > >
#define INTSIZE(a) static_cast<int>((a).size())

#include "error.h"
#include "buffer.hpp"
#include "mem.hpp"

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
