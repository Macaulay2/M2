#include "myalloc.hpp"

size_t StatsAlloc::mNumAllocs = 0;
size_t StatsAlloc::mNumDeallocs = 0;
size_t StatsAlloc::mAllocSize = 0;
size_t StatsAlloc::mCurrentAllocSize = 0;
size_t StatsAlloc::mHighWater = 0;

std::ostream& operator<<(std::ostream& o, StatsAlloc a)
{
  o << "allocs/deallocs, total/high/current size allocated: "
    << a.mNumAllocs << "/" << a.mNumDeallocs << "  "
    << a.mAllocSize << "/" << a.mHighWater << "/" << a.mCurrentAllocSize;
  return o;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
