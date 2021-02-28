#include "myalloc.hpp"

size_t AllocLogger::mNumAllocs = 0;
size_t AllocLogger::mNumDeallocs = 0;
size_t AllocLogger::mAllocSize = 0;
long AllocLogger::mCurrentAllocSize = 0;
size_t AllocLogger::mHighWater = 0;

std::ostream& operator<<(std::ostream& o, AllocLogger a)
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
