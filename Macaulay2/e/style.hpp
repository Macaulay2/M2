// (c) 1994 Michael E. Stillman
#ifndef _style_hh_
#define _style_hh_

#include <cmath>    // to get fabs(), gcc 3.0
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <string.h>
#include <algorithm>
#include <gmp.h>

#define VECTOR(T) std::vector< T, gc_allocator< T > >

extern "C" char newline[];

#include "error.h"
#include "buffer.hpp"
#include "mem.hpp"

typedef unsigned int unsigned_int;
typedef int *ptr_to_int;
typedef unsigned int *ptr_to_unsigned_int;

#if defined(_WIN32)
template <class T> 
inline void swap(T &t1, T &t2)
{
  T tmp = t1;
  t1 = t2;
  t2 = tmp;
}
#endif

const int LT = -1;
const int EQ = 0;
const int GT = 1;
const int INCOMPARABLE = 2;
const int EXCEPTION = -2;

// Used for all of the heap types: polynomial, vector, resolution vectors.
#define GEOHEAP_SIZE 15

extern int heap_size[GEOHEAP_SIZE];


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
