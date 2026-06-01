// (c) 1995  Michael E. Stillman
#ifndef M2_MEM_HH_
#define M2_MEM_HH_

#include "newdelete.hpp"

extern size_t engine_allocated;
extern size_t engine_highwater;

static inline void engine_alloc(size_t n)
{
  engine_allocated += n;
  if (engine_allocated > engine_highwater) engine_highwater = engine_allocated;
}

static inline void engine_dealloc(size_t n) { engine_allocated -= n; }
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
