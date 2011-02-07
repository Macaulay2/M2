// Copyright 2011  Michael E. Stillman

#include "spair-heap.hpp"

spair_heap::spair_heap(const Monoid *M)
  : M_(M),
    spairCompareObject_(),
    S_(spairCompareObject_)
{
}
